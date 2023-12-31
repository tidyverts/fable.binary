#' @importFrom stats ar complete.cases
train_nnet <- function(.data, specials, n_nodes, n_networks, scale_inputs, wts = NULL,...) {
  require_package("nnet")
  if (length(measured_vars(.data)) > 1) {
    abort("Only univariate responses are supported by BINNET.")
  }
  y <- unclass(.data)[[measured_vars(.data)]]
  if (all(is.na(y))) {
    abort("All observations are missing, a model cannot be estimated without data.")
  }
  n <- length(y)
  if (n < 3) {
    stop("Not enough data to fit a model")
  }
  xreg <- specials$xreg[[1]]
  if (is.null(xreg)) {
    abort("Exogenous regressors are required for BINNET.")
  }
  # Check for constant data in xreg
  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    if (any(apply(xreg, 2, is.constant))) {
      warn("Constant xreg column, setting `scale_inputs=FALSE`")
      scale_inputs <- FALSE
    }
  }

  # Scale inputs
  xreg_scale <- NULL
  if (is.list(scale_inputs)) {
    if(!is.null(xreg)){
      xreg <- sweep(xreg, 2, scale_inputs$xreg$center, "-")
      xreg <- sweep(xreg, 2, scale_inputs$xreg$scale, "/")
    }
    scales <- scale_inputs
    scale_inputs <- TRUE
  } else {
    if (scale_inputs) {
      if (!is.null(xreg)) {
        xreg <- scale(xreg, center = TRUE, scale = TRUE)
        xreg_scale <- list(
          center = attr(xreg, "scaled:center"),
          scale = attr(xreg, "scaled:scale")
        )
      }
    }
    scales <- list(xreg = xreg_scale)
  }

  if (is.null(n_nodes)) {
    n_nodes <- round((NCOL(xreg) + 1) / 2)
  }

  # Remove missing values if present
  nonmissing <- complete.cases(xreg, y)
  ## Stop if there's no data to fit
  if (NROW(xreg[nonmissing, , drop = FALSE]) == 0) {
    abort("No data to fit (possibly due to missing values)")
  }

  # Fit the nnet and consider the Wts argument for nnet::nnet() if provided:
  if (is.null(wts)) {
    nn_models <- map(
      seq_len(n_networks),
      function(.) wrap_nnet(xreg[nonmissing, , drop = FALSE], y[nonmissing], size = n_nodes, ...)
    )
  } else {
    maxnwts <- max(lengths(wts), na.rm = TRUE)
    nn_models <- map(
      wts,
      function(i) {
        wrap_nnet(x = xreg[nonmissing, , drop = FALSE], y = y[nonmissing], size = n_nodes, MaxNWts = maxnwts, Wts = i, ...)
      })
  }

  # Calculate fitted values
  pred <- map_dbl(transpose(map(nn_models, predict)), function(x) mean(unlist(x)))
  fits <- y*NA
  fits[nonmissing] <- pred
  res <- y - fits
  

  # Construct model output
  structure(
    list(
      model = nn_models,
      par = tibble(),
      est = tibble(.fitted = fits, .resid = res),
      fit = tibble(sigma2 = stats::var(res, na.rm = TRUE)),
      spec = tibble(size = n_nodes),
      scales = scales
    ),
    class = "BINNET"
  )
}

# Wrap nnet to change the default for linout to be FALSE
wrap_nnet <- function(x, y, linout = FALSE, trace = FALSE, ...) {
  if(linout) {
    warning("linout reset to FALSE for binary response data")
  }
  nnet::nnet(x, y, linout = FALSE, trace = trace, ...)
}

#' Neural Network Binary Time Series Forecasts
#'
#' Feed-forward neural networks with a single hidden layer and lagged inputs
#' for forecasting univariate binary time series.
#'
#' A feed-forward neural network is fitted with a single hidden layer containing `size` nodes.
#'
#' Exogenous regressors are used as inputs.
#' A total of `repeats` networks are
#' fitted, each with random starting weights. These are then averaged when
#' computing forecasts.
#'
#' @aliases report.BINNET
#'
#' @param formula Model specification (see "Specials" section).
#' @param n_nodes Number of nodes in the hidden layer. Default is half of the
#' number of external regressors plus 1.
#' @param n_networks Number of networks to fit with different random starting
#' weights. These are then averaged when producing forecasts.
#' @param scale_inputs If TRUE, inputs are scaled by subtracting the column
#' means and dividing by their respective standard deviations.
#' @param ... Other arguments passed to `\link[nnet]{nnet}`.
#'
#' @return A model specification.
#'
#' @examples
#' melb_rain |>
#'   model(nn = BINNET(Wet ~ fourier(K = 1, period = "year")))
#'
#' @export
BINNET <- function(formula, n_nodes = NULL, n_networks = 20, scale_inputs = TRUE, ...) {
  nnet_model <- new_model_class("BINNET", train_nnet, specials_nnet,
    origin = NULL, check = all_tsbl_checks
  )
  new_model_definition(nnet_model, !!enquo(formula),
    n_nodes = n_nodes,
    n_networks = n_networks, scale_inputs = scale_inputs, ...
  )
}

specials_nnet <- new_specials(
  common_xregs,
  xreg = model_xreg,
  .xreg_specials = names(common_xregs)
)

#' Forecast a model from the fable package
#'
#'  Produces forecasts from a trained model.
#'
#' @inheritParams generics::forecast
#' @param specials (passed by [`fabletools::forecast.mdl_df()`]).
#' @param times The number of sample paths to use in estimating the forecast distribution when `simulate = TRUE`.
#' @param simulate If `TRUE`, then forecast distributions are computed using simulation from a Bernoulli model.
#' @param new_data A tsibble containing the time points and exogenous regressors to produce forecasts for.
#'
#' @examples
#' melb_rain |>
#'   model(nn = BINNET(Wet ~ fourier(K = 1, period = "year"))) |>
#'   forecast(times = 10)
#' @export
forecast.BINNET <- function(object, new_data, specials = NULL, simulate = TRUE, times = 5000, ...) {
  require_package("nnet")

  # Prepare xreg
  xreg <- specials$xreg[[1]]

  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    if (!is.null(object$scales$xreg)) {
      xreg <- scale(xreg, center = object$scales$xreg$center, scale = object$scales$xreg$scale)
    }
  }

  # Compute means of future periods
  pred <- lapply(object$model, stats::predict, newdata = xreg) |>
    unlist() |>
    matrix(ncol = length(object$model)) |>
    rowMeans()

  # Compute forecast distributions
  if (!simulate) {
    warn("Analytical forecast distributions are not available for BINNET.")
  }
  if(times == 0L)
    simulate <- FALSE
  if(simulate) {
    sim <- purrr::map(pred, function(x) {
      rbinom(n = times, size = 1, prob = x)
    })
    distributional::dist_sample(sim)
  } else {
    # Return mean as degenerate distribution
    distributional::dist_degenerate(pred)
  }
}

#' Generate new data from a fable model
#'
#'  Simulates future paths from a dataset using a fitted model.
#'
#' @inheritParams generics::generate
#' @param specials (passed by [`fabletools::generate.mdl_df()`]).
#' @param new_data A tsibble containing the time points and exogenous regressors to produce forecasts for.
#'
#' @examples
#' melb_rain |>
#'   model(nn = BINNET(Wet ~ fourier(K = 1, period = "year"))) |>
#'   generate()
#' @export
generate.BINNET <- function(x, new_data, specials = NULL, ...) {
  # Prepare xreg
  xreg <- specials$xreg[[1]]
  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    if (!is.null(x$scales$xreg)) {
      xreg <- scale(xreg, center = x$scales$xreg$center, scale = x$scales$xreg$scale)
    }
  }
  # Compute means of future periods
  pred <- lapply(x$model, predict, newdata = xreg) |>
    unlist() |>
    matrix(ncol = length(x$model)) |>
    rowMeans()
  # Generate sample
  transmute(new_data,
    .sim = rbinom(n = NROW(new_data), size = 1, prob = pred))
}

#' Extract fitted values from a fable model
#'
#' Extracts the fitted values.
#'
#' @param object Fitted model
#' @param ... Other arguments ignored
#'
#' @examples
#' melb_rain |>
#'   model(nn = BINNET(Wet ~ fourier(K = 1, period = "year"))) |>
#'   fitted()
#' @export
fitted.BINNET <- function(object, ...) {
  object$est[[".fitted"]]
}


#' Extract residuals from a fable model.
#'
#' Extracts the residuals.
#'
#' @param object Fitted model
#' @param ... Other arguments ignored
#'
#' @examples
#' melb_rain |>
#'   model(nn = BINNET(Wet ~ fourier(K = 1, period = "year"))) |>
#'   residuals()
#' @export
residuals.BINNET <- function(object, ...) {
  object$est[[".resid"]]
}

#' Glance a BINNET model
#'
#' Construct a single row summary of the BINNET model.
#' Contains the variance of residuals (`sigma2`).
#'
#' @inheritParams generics::glance
#' @param ... Other arguments ignored
#'
#' @return A one row tibble summarising the model's fit.
#'
#' @examples
#' melb_rain |>
#'   model(nn = BINNET(Wet ~ fourier(K = 1, period = "year"))) |>
#'   glance()
#' @export
glance.BINNET <- function(x, ...) {
  tibble(inputs = x$model[[1]]$n[1L],
         hidden_nodes = x$model[[1]]$n[2L],
         weights = length(x$model[[1]]$wts),
         repeats = length(x$model),
         sigma2 = x$fit$sigma2)

}

#' Tidy a fable model
#'
#' Returns the coefficients from the model in a `tibble` format.
#'
#' @inheritParams generics::tidy
#'
#' @examples
#' melb_rain |>
#'   model(nn = BINNET(Wet ~ fourier(K = 1, period = "year"))) |>
#'   tidy()
#' @export
tidy.BINNET <- function(x, ...) {
  tibble::tibble(term = character(0), estimate = numeric(0))
}

#' @export
report.BINNET <- function(object, ...) {
  cat(paste("\nAverage of", length(object$model), "networks, each of which is\n"))
  print(object$model[[1]])
  cat(
    "\nsigma^2 estimated as ",
    format(mean(residuals(object)^2, na.rm = TRUE), digits = 4),
    "\n",
    sep = ""
  )
  invisible(object)
}

#' @importFrom stats coef
#' @export
model_sum.BINNET <- function(x) {
  sprintf("BINNET: %s", x$spec$size)
}

#' Refit a BINNET model
#'
#' Applies a fitted BINNET model to a new dataset.
#'
#' @inheritParams forecast.BINNET
#' @param reestimate If `TRUE`, the networks will be initialized with random
#' starting weights to suit the new data. If `FALSE`, for every network the best
#' individual set of weights found in the pre-estimation process is used as the
#' starting weight vector.
#'
#' @return A refitted model.
#'
#' @importFrom stats formula residuals
#' @export
refit.BINNET <- function(object, new_data, specials = NULL, reestimate = FALSE, ...) {
  # Update data for re-evaluation
  # update specials and size:

  size <- object$spec[["size"]]

  # extract number of networks used:
  n_nets <- length(object$model)

  # check for scale_inputs:
  scale_in <- if(is_empty(object$scales)) FALSE else object$scales

  # return for reestimate = TRUE; i.e random assignment of weights:
  if (reestimate) {
    return(train_nnet(new_data, specials, n_nodes = size, n_networks = n_nets,
                        scale_inputs = scale_in, ...))
  }

  # extract best set of weights for every network:
  wts_list <- lapply(object$model, `[[`, "wts")

  out <- train_nnet(new_data, specials, n_nodes = size, n_networks = n_nets,
                      scale_inputs = scale_in, wts = wts_list, maxit = 0, ...)
  out
}

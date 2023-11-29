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
  y_scale <- xreg_scale <- NULL
  if (is.list(scale_inputs)) {
    if(!is.null(xreg)){
      xreg <- sweep(xreg, 2, scale_inputs$xreg$center, "-")
      xreg <- sweep(xreg, 2, scale_inputs$xreg$scale, "/")
    }
    scales <- scale_inputs <- TRUE
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
  j <- complete.cases(xreg, y)
  xreg <- xreg[j, , drop = FALSE]
  y <- y[j]
  ## Stop if there's no data to fit
  if (NROW(xreg) == 0) {
    abort("No data to fit (possibly due to missing values)")
  }

  # Fit the nnet and consider the Wts argument for nnet::nnet() if provided:
  if (is.null(wts)) {
    nn_models <- map(
      seq_len(n_networks),
      function(.) wrap_nnet(xreg, y, size = n_nodes, ...)
    )
  } else {
    maxnwts <- max(lengths(wts), na.rm = TRUE)
    nn_models <- map(
      wts,
      function(i) {
        wrap_nnet(x = xreg, y = y, size = n_nodes, MaxNWts = maxnwts, Wts = i, ...)
      })
  }

  # Calculate fitted values
  pred <- map_dbl(transpose(map(nn_models, predict)), function(x) mean(unlist(x)))
  fits <- pred
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
#' as_tsibble(USAccDeaths) |>
#'   dplyr::mutate(y = value > mean(value)) |>
#'   model(nn = BINNET(y ~ season()))
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
#' @param bootstrap If `TRUE`, then forecast distributions are computed using simulation with resampled errors.
#' @param times The number of sample paths to use in estimating the forecast distribution when `bootstrap = TRUE`.
#' @param simulate If `TRUE`, then forecast distributions are computed using simulation with Gaussian errors.
#' @param new_data A tsibble containing the time points and exogenous regressors to produce forecasts for.
#'
#' @examples
#' as_tsibble(airmiles) %>%
#'   model(nn = BINNET(box_cox(value, 0.15))) %>%
#'   forecast(times = 10)
#' @export
forecast.BINNET <- function(object, new_data, specials = NULL, simulate = TRUE, bootstrap = FALSE, times = 1000, ...) {
  require_package("nnet")

  # Prepare xreg
  xreg <- specials$xreg[[1]]

  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    if (!is.null(object$scales$xreg)) {
      xreg <- scale(xreg, center = object$scales$xreg$center, scale = object$scales$xreg$scale)
    }
  }

  # Compute forecast intervals
  if (!simulate) {
    warn("Analytical forecast distributions are not available for BINNET.")
    times <- 0
  }
  sim <- map(seq_len(times), function(x) {
    generate(object, new_data, specials = specials, bootstrap = bootstrap)[[".sim"]]
  })
  if (length(sim) > 0) {
    sim <- sim %>%
      transpose() %>%
      map(as.numeric)
    distributional::dist_sample(sim)
  }
  else {
    # Compute forecasts
    h <- NROW(new_data)
    fc <- mean(map_dbl(object$model, predict, newdata = new_data))
    distributional::dist_degenerate(fc)
  }
}

#' Generate new data from a fable model
#'
#'  Simulates future paths from a dataset using a fitted model.
#'  Innovations are sampled by the model's assumed error distribution.
#'  If bootstrap is TRUE, innovations will be sampled from the model's residuals.
#'  If new_data contains the .innov column, those values will be treated as innovations.
#'
#' @inheritParams generics::generate
#' @param specials (passed by [`fabletools::generate.mdl_df()`]).
#' @param bootstrap If `TRUE`, then innovations are sampled from the model's residuals.
#' @param new_data A tsibble containing the time points and exogenous regressors to produce forecasts for.
#'
#' @examples
#' as_tsibble(airmiles) %>%
#'   model(nn = BINNET(box_cox(value, 0.15))) %>%
#'   generate()
#' @export
generate.BINNET <- function(x, new_data, specials = NULL, bootstrap = FALSE, ...) {
  # Prepare xreg
  xreg <- specials$xreg[[1]]

  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    if (!is.null(x$scales$xreg)) {
      xreg <- scale(xreg, center = x$scales$xreg$center, scale = x$scales$xreg$scale)
    }
  }

  if (!(".innov" %in% names(new_data))) {
    if (bootstrap) {
      res <- stats::na.omit(x$est[[".resid"]] - mean(x$est[[".resid"]], na.rm = TRUE))
      if (!is.null(x$scales$y)) {
        res <- res / x$scales$y$scale
      }
      new_data$.innov <- sample(res, NROW(new_data), replace = TRUE)
    }
    else {
      if (!is.null(x$scales$y)) {
        sigma <- sd(x$est[[".resid"]] / x$scales$y$scale, na.rm = TRUE)
      }
      else {
        sigma <- sqrt(x$fit$sigma2)
      }
      new_data$.innov <- stats::rnorm(NROW(new_data), sd = sigma)
    }
  }
  else {
    if (!is.null(x$scales$y)) {
      new_data[[".innov"]] <- new_data[[".innov"]] / x$scales$y$scale
    }
  }

  sim_nnet <- function(e) {
    mean(map_dbl(x$model, predict, newdata = new_data))
  }

  transmute(group_by_key(new_data), ".sim" := sim_nnet(!!sym(".innov")))
}

#' Extract fitted values from a fable model
#'
#' Extracts the fitted values.
#'
#' @param object Fitted model
#' @param ... Other arguments ignored
#'
#' @examples
#' as_tsibble(airmiles) %>%
#'   model(nn = BINNET(box_cox(value, 0.15))) %>%
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
#' as_tsibble(airmiles) %>%
#'   model(nn = BINNET(box_cox(value, 0.15))) %>%
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
#' as_tsibble(airmiles) %>%
#'   model(nn = BINNET(box_cox(value, 0.15))) %>%
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
#' as_tsibble(USAccDeaths) |>
#'   dplyr::mutate(y = value > mean(value)) |>
#'   model(nn = BINNET(y ~ season())) |>
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

glm_glance_measures <- function(fit) {
  # Set up fit measures
  n <- NROW(na.omit(fit$residuals))
  edf <- n - fit$df.residual
  k <- edf - 1
  list(
    df = edf, log_lik = edf - 0.5*fit$aic,
    AIC = fit$aic, AICc = fit$aic + 2 * (k + 1) * (k + 2) / (n - k - 2),
    BIC = fit$aic + (k + 1) * (log(n) - 2),
    deviance = fit$deviance, df.residual = fit$df.residual, rank = fit$rank,
    null_deviance = fit$null.deviance, df_null = fit$df.null, nobs = n
  )
}

train_logistic <- function(.data, specials, ...) {
  y <- invoke(cbind, unclass(.data)[measured_vars(.data)])
  xreg <- specials$xreg[[1]]

  keep <- complete.cases(xreg) & complete.cases(y)
  fit <- stats::glm.fit(xreg[keep, , drop = FALSE], y[keep, , drop = FALSE],
                        family = stats::binomial())
  resid <- fits <- matrix(nrow = nrow(y), ncol = ncol(y))
  resid[keep, ] <- as.matrix(fit$residuals)
  fit$residuals <- resid
  fits[keep, ] <- as.matrix(fit$fitted.values)
  fit$fitted.values <- fits

  if (is_empty(fit$coefficients)) {
    fit$coefficients <- matrix(nrow = 0, ncol = NCOL(y))
  }
  else {
    fit$coefficients <- as.matrix(fit$coefficients)
  }
  colnames(fit$coefficients) <- colnames(y)

  # Remove unused structure
  fit$effects <- NULL
  #fit$sigma2 <- sum(resid^2, na.rm = TRUE)/fit$df.residual

  structure(fit, class = "LOGISTIC")
}

specials_logistic <- new_specials(
  common_xregs,
  xreg = special_xreg(),
  .required_specials = "xreg",
  .xreg_specials = names(common_xregs),
)

#' Fit a linear model with time series components
#'
#' The model formula will be handled using [`stats::model.matrix()`], and so
#' the the same approach to include interactions in [`stats::lm()`] applies when
#' specifying the `formula`. In addition to [`stats::lm()`], it is possible to
#' include [`common_xregs`] in the model formula, such as `trend()`, `season()`,
#' and `fourier()`.
#'
#' @aliases report.LOGISTIC
#'
#' @param formula Model specification.
#'
#' @section Specials:
#'
#' \subsection{xreg}{
#' Exogenous regressors can be included in a LOGISTIC model without explicitly
#' using the `xreg()` special. Common exogenous regressor specials as specified
#' in [`common_xregs`] can also be used. These regressors are handled using
#' [stats::model.frame()], and so interactions and other functionality behaves
#' similarly to [stats::lm()].
#' \preformatted{
#' xreg(...)
#' }
#'
#' \tabular{ll}{
#'   `...`      \tab Bare expressions for the exogenous regressors (such as `log(x)`)
#' }
#' }
#'
#' @return A model specification.
#'
#' @seealso
#' [`stats::lm()`], [`stats::model.matrix()`]
#' [Forecasting: Principles and Practices, Time series regression models (chapter 6)](https://otexts.com/fpp3/regression.html)
#'
#' @examples
#' melb_rain |>
#'   model(logistic = LOGISTIC(Wet ~ fourier(K = 5, period = "year")))
#'
#' @export
LOGISTIC <- function(formula) {
  logistic_model <- new_model_class("LOGISTIC",
    train = train_logistic,
    specials = specials_logistic, origin = NULL
  )
  new_model_definition(logistic_model, !!enquo(formula))
}

#' @inherit fitted.BINNET
#'
#' @examples
#' melb_rain |>
#'   model(logistic = LOGISTIC(Wet ~ fourier(K = 5, period = "year"))) |>
#'   fitted()
#' @export
fitted.LOGISTIC <- function(object, ...) {
  object$fitted
}

#' @inherit residuals.BINNET
#' @param type the type of residuals which should be returned.
#' alternatives are: "deviance" (default), "pearson", "working", "response",
#' and "partial". Can be abbreviated.
#' @examples
#' melb_rain |>
#'   model(logistic = LOGISTIC(Wet ~ fourier(K = 5, period = "year"))) |>
#'   residuals(type = "deviance")
#' @export
residuals.LOGISTIC <- function(object,
    type = c("deviance", "innovation", "pearson", "working", "response", "partial"), 
    ...) {
  type <- match.arg(type)
  if (type == "innovation") {
    type <- "deviance"
  }
  y <- object$y
  r <- object$residuals
  mu <- object$fitted.values
  wts <- object$prior.weights
  switch(type,
    deviance = ,
    pearson = ,
    response = if (is.null(y)) {
      mu.eta <- object$family$mu.eta
      eta <- object$linear.predictors
      y <- mu + r * mu.eta(eta)
    }
  )
  res <- switch(type,
    deviance = if (object$df.residual > 0) {
      d.res <- sqrt(pmax((object$family$dev.resids)(y, mu, wts), 0))
      ifelse(y > mu, d.res, -d.res)
    } else {
      rep.int(0, length(mu))
    },
    pearson = (y - mu) * sqrt(wts) / sqrt(object$family$variance(mu)),
    working = r,
    response = y - mu,
    partial = r
  )
  if (type == "partial") {
    res <- res + predict(object, type = "terms")
  }
  res
}

#' Glance a LOGISTIC
#'
#' Construct a single row summary of the LOGISTIC model.
#'
#' Contains the R squared (`r_squared`), variance of residuals (`sigma2`),
#' the log-likelihood (`log_lik`), and information criterion (`AIC`, `AICc`, `BIC`).
#'
#' @inheritParams generics::glance
#'
#' @return A one row tibble summarising the model's fit.
#'
#' @examples
#' melb_rain |>
#'   model(logistic = LOGISTIC(Wet ~ fourier(K = 5, period = "year"))) |>
#'   glance()
#' @export
glance.LOGISTIC <- function(x, ...) {
  as_tibble(glm_glance_measures(x))
}

#' @inherit tidy.BINNET
#'
#' @examples
#' melb_rain |>
#'   model(logistic = LOGISTIC(Wet ~ fourier(K = 5, period = "year"))) |>
#'   tidy()
#' @export
tidy.LOGISTIC <- function(x, ...) {
  rdf <- x$df.residual
  coef <- x$coefficients
  rank <- x$rank
  
  if(rank > 0) {
    p1 <- seq(rank)
    Qr <- x$qr
    coef.p <- coef[Qr$pivot[p1]]
    covmat.unscaled <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    dimnames(covmat.unscaled) <- list(names(coef.p), names(coef.p))
    covmat <- covmat.unscaled
    var.cf <- diag(covmat)
    se <- sqrt(var.cf)
    tvalue <- coef.p/se
  }

  out <- tidyr::gather(
    dplyr::as_tibble(coef, rownames = "term"),
    ".response", "estimate", !!!syms(colnames(coef))
  )
  if (NCOL(coef) == 1) out[[".response"]] <- NULL
  dplyr::mutate(
    out,
    std.error = unlist(se),
    statistic = !!sym("estimate") / !!sym("std.error"),
    p.value = 2 * stats::pnorm(abs(!!sym("statistic")), lower.tail = FALSE)
  )
}

#' @export
report.LOGISTIC <- function(object, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCoefficients:\n")
  coef <- tidy(object)
  coef_mat <- as.matrix(coef[ncol(coef) - c(3:0)])
  colnames(coef_mat) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(coef_mat) <- coef$term
  stats::printCoefmat(coef_mat,
    digits = digits,
    signif.stars = getOption("show.signif.stars"), ...
  )
  cat("\n")
  glance(object) |> print()
  invisible(object)
}

#' @inherit forecast.BINNET
#' @importFrom stats predict
#'
#' @examples
#' melb_rain |>
#'   model(logistic = LOGISTIC(Wet ~ fourier(K = 5, period = "year"))) |>
#'   forecast(h = "2 years")
#' @export
forecast.LOGISTIC <- function(object, new_data,
              specials = NULL, simulate = FALSE, times = 5000, ...) {
  coef <- object$coefficients
  rank <- object$rank
  qr <- object$qr
  piv <- qr$pivot[seq_len(rank)]

  # Get xreg
  xreg <- specials$xreg[[1]]

  if (rank < ncol(xreg)) {
    warn("prediction from a rank-deficient fit may be misleading")
  }
  # Forecast distributions
  fc <- drop(xreg[, piv, drop = FALSE] %*% coef[piv])
  fc <- exp(fc)/(1+exp(fc))

  if (simulate) { # Compute prediction intervals using simulations
    if(times == 0L) {
      output <- distributional::dist_degenerate(fc)
    } else {
      sim <- map(fc, function(x) {
        rbinom(n = times, size = 1, prob = x)
      })
      output <- distributional::dist_sample(sim)
    }
  } else {
    output <- distributional::dist_binomial(1, fc)
  }
  return(output)
}

#' @inherit generate.BINNET
#'
#' @examples
#' melb_rain |>
#'   model(logistic = LOGISTIC(Wet ~ fourier(K = 5, period = "year"))) |>
#'   generate()
#' @export
generate.LOGISTIC <- function(x, new_data, specials, ...) {
  xreg <- specials$xreg[[1]]
  coef <- x$coefficients
  piv <- x$qr$pivot[seq_len(x$rank)]
  pred <- xreg[, piv, drop = FALSE] %*% coef[piv]
  pred <- exp(pred)/(1+exp(pred))
  transmute(new_data,
    .sim = rbinom(n = NROW(new_data), size = 1, prob = pred))
}

#' Refit a `LOGISTIC`
#'
#' Applies a fitted `LOGISTIC` to a new dataset.
#'
#' @inheritParams generics::refit
#' @param new_data A tsibble containing the time points and exogenous regressors
#' for which a refit is required.
#' @param specials A list of special functions used in the model, (passed by
#' `fabletools::forecast.mdl_df`).
#' @param reestimate If TRUE, the networks will be initialized with random
#' starting weights to suit the new data. If FALSE, for every network the best
#' individual set of weights found in the pre-estimation process is used as the
#' starting weight vector.
#'
#' @export
refit.LOGISTIC <- function(object, new_data, specials = NULL, reestimate = FALSE, ...) {
  # Update data for re-evaluation
  if (reestimate) {
    return(train_logistic(new_data, specials, ...))
  }

  # Get inputs
  y <- invoke(cbind, unclass(new_data)[measured_vars(new_data)])
  xreg <- specials$xreg[[1]]

  fit <- object
  coef <- object$coefficients
  fit$qr <- qr(xreg)
  piv <- fit$qr$pivot[seq_len(fit$rank)]
  pred <- xreg[, piv, drop = FALSE] %*% coef[piv]
  # Transform back to probability
  fit$fitted.values <- exp(pred)/(1+exp(pred))
  fit$residuals <- y - fit$fitted.values

  structure(fit, class = "LOGISTIC")
}

#' @export
model_sum.LOGISTIC <- function(x) {
  "LOGISTIC"
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nnet.R
\name{forecast.BINNET}
\alias{forecast.BINNET}
\title{Forecast a model from the fable package}
\usage{
\method{forecast}{BINNET}(object, new_data, specials = NULL, simulate = TRUE, times = 5000, ...)
}
\arguments{
\item{object}{A model for which forecasts are required.}

\item{new_data}{A tsibble containing the time points and exogenous regressors to produce forecasts for.}

\item{specials}{(passed by \code{\link[fabletools:forecast]{fabletools::forecast.mdl_df()}}).}

\item{simulate}{If \code{TRUE}, then forecast distributions are computed using simulation from a Bernoulli model.}

\item{times}{The number of sample paths to use in estimating the forecast distribution when \code{simulate = TRUE}.}

\item{...}{Other arguments passed to methods}
}
\description{
Produces forecasts from a trained model.
}
\examples{
melb_rain |>
  model(nn = BINNET(Wet ~ fourier(K = 1, period = "year"))) |>
  forecast(times = 10)
}

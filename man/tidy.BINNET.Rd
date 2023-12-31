% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nnet.R
\name{tidy.BINNET}
\alias{tidy.BINNET}
\title{Tidy a fable model}
\usage{
\method{tidy}{BINNET}(x, ...)
}
\arguments{
\item{x}{An object to be converted into a tidy \code{\link[tibble:tibble]{tibble::tibble()}}.}

\item{...}{Additional arguments to tidying method.}
}
\description{
Returns the coefficients from the model in a \code{tibble} format.
}
\examples{
melb_rain |>
  model(nn = BINNET(Wet ~ fourier(K = 1, period = "year"))) |>
  tidy()
}

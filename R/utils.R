is.constant <- function(x) {
  x <- as.numeric(x)
  y <- rep(x[1], length(x))
  return(isTRUE(all.equal(x, y)))
}

assignSpecials <- function(x, env = caller_env()) {
  imap(x, function(.x, nm) {
    if (length(.x) > 1) warn(sprintf("Only one special for `%s` is allowed, defaulting to the first usage", nm))
    imap(.x[[1]], function(.x, .y) assign(.y, .x, envir = env))
  })
}

require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    abort(
      sprintf('The `%s` package must be installed to use this functionality. It can be installed with install.packages("%s")', pkg, pkg)
    )
  }
}

`%||%` <- function(x, y) if (is_null(x)) y else x


transpose <- function (.l)  {
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  }
  else {
    fields <- set_names(inner_names)
  }
  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}

invoke <- function (.f, .x, ..., .env = NULL) {
  .env <- .env %||% parent.frame()
  args <- c(as.list(.x), list(...))
  do.call(.f, args, envir = .env)
}

compose <- function (...) 
{
  fs <- lapply(list(...), match.fun)
  n <- length(fs)
  last <- fs[[n]]
  rest <- fs[-n]
  function(...) {
    out <- last(...)
    for (f in rev(rest)) {
      out <- f(out)
    }
    out
  }
}

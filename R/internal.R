#' Throws an error if a condition isn't met.
#'
#' The workhorse of the package.  If a condition isn't met, then an error
#' is thrown.
#'
#' @param x Input to check.  If missing, pass no args to \code{predicate}.
#' @param predicate Function that returns a logical value (possibly 
#' a vector).
#' @param msg The error message, in the event of failure.
#' @param what Either 'all' or 'any', to reduce vectorised tests to a 
#' single value.
#' @param ... Passed to the \code{predicate} function.
#' @return \code{FALSE} with the attribute \code{message}, as provided
#' in the input.
assert_engine <- function(x, predicate, msg, what = c("all", "any"), ...)
{
  handler <- match.fun(match.arg(
    getOption("assertive.severity"),
    c("stop", "warning", "message")
  ))
  what <- match.fun(match.arg(what))
  #Some functions, e.g., is.R take no args
  ok <- what(if(missing(x)) predicate() else predicate(x, ...))
  if(!ok)
  {
    handler(msg, call. = FALSE)
  }
}

#' FALSE, with a message.
#'
#' Always returns the value \code{FALSE}, with a message attribute.
#'
#' @param msg A string to provide a message.
#' @return \code{FALSE} with the attribute \code{message}, as provided
#' in the input.
false <- function(msg. = "")
{
  x <- FALSE
  msg(x) <- msg.
  x
}

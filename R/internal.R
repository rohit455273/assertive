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
assert_engine <- function(x, predicate, msg., what = c("all", "any"), ...)
{
  handler <- match.fun(match.arg(
    getOption("assertive.severity"),
    c("stop", "warning", "message")
    ))
  what <- match.fun(match.arg(what))
  #Some functions, e.g., is.R take no args
  ok <- if(missing(x)) predicate() else predicate(x, ...)
  if(!what(ok))
  {
    if(missing(msg.)) 
    {
      if(is_scalar(ok))
      {
        msg. <- msg(ok)
      } else
      {
        stop("Bug in assertive; error message is missing")
      }
    }
    handler(msg., call. = FALSE)
  }
}

#' FALSE, with a message.
#'
#' Always returns the value \code{FALSE}, with a message attribute.
#'
#' @param ... Passed to sprintf to create a message.
#' @return \code{FALSE} with the attribute \code{message}, as provided
#' in the input.
false <- function(...)
{
  msg. <- if(length(list(...)) > 0L) sprintf(...) else ""
  x <- FALSE
  msg(x) <- msg.
  x
}

#' Alternative version of is.
#' 
#' If a function named \code{is.class} exists, call \code{is.class(x)}.
#' If not, call \code{is(x, class)}.
#' @param x Input to check.
#' @param class Target class that \code{x} maybe belong to.
#' @return \code{TRUE} if x belongs to the class and \code{FALSE} 
#' otherwise.
is2 <- function(x, class)
{  
  fn <- try(match.fun(paste0("is.", class)), silent = TRUE)
  condn <- if(inherits(fn, "try-error"))
  {
    is(x, class)
  } else
  {
    fn(x)
  }
}

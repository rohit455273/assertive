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
  ok <- if(missing(x)) predicate() else predicate(x, ...)
  if(!what(ok))
  {
    if(missing(msg)) 
    {
      if(is_scalar(ok))
      {
        msg <- cause(ok)
      } else
      {
        stop("Bug in assertive; error message is missing")
      }
    }
    handler(msg, call. = FALSE)
  }
}

#' Wrapper to vapply that returns booleans.
#' 
#' Wrapper to \code{\link{vapply}} for functions that return a boolean (logical scalar) value.
#' 
#' @param x A vector (atomic or list).
#' @param predicate A predicate (function that returns a bool) to apply elementwise to \code{x}.
#' @param USE.NAMES Passed to \code{vapply}.
#' @param ... Passed to \code{vapply}.
#' @return A logical vector.
#' @seealso \code{\link{vapply}}.
bapply <- function(x, predicate, ..., USE.NAMES = TRUE)
{
  vapply(x, predicate, logical(1L), ..., USE.NAMES = TRUE)
}

#' Call a function, and give the result names.
#'
#' Calls a function, and names the result with the first argument.
#'
#' @param fn A function to call.  See note below.
#' @param x The first input to \code{fn}.
#' @param ... Optional additional inputs to \code{fn}.
#' @return The result of \code{fn(x, ...)}, with names given by the
#' argument \code{x}.
#' @note The function, \code{fn}, should return an object with the 
#' same length as the input \code{x}.
#' @examples
#' \dontrun{
#' call_and_name(is.finite, c(1, Inf, Na))
#' }
#' @seealso \code{\link{cause}} and \code{\link{na}}.
call_and_name <- function(fn, x, ...)
{
  y <- fn(x, ...)
  if(!is_identical_to_true(length(y) == length(x)))
  {
    warning("Vector of names is different length to results.  Trying to resize.")
    length(x) <- length(y)
  }
  names(y) <- x
  y
}

#' Convert a character vector to a list of numeric vectors.
#'
#' Split strings by character, then convert to numeric.
#' @param x Input to convert.
#' @return A list of numeric vectors.
#' @seealso \code{\link[base{strsplit}} and \code{\link[base]{as.numeric}}.
character_to_list_of_numeric_vectors <- function(x)
{
  lapply(strsplit(x, ""), as.numeric)
}

#' FALSE, with a cause of failure.
#'
#' Always returns the value \code{FALSE}, with a cause attribute.
#'
#' @param ... Passed to sprintf to create a cause of failure message.
#' @return \code{FALSE} with the attribute \code{cause}, as provided
#' in the input.
#' @seealso \code{\link{cause}} and \code{\link{na}}.
false <- function(...)
{
  msg <- if(length(list(...)) > 0L) sprintf(...) else ""
  x <- FALSE
  cause(x) <- msg
  x
}

#' Does the input match the regular expression?
#' 
#' Checks that the input matches the regular expression.
#'
#' @param x Inout to check.
#' @param rx A regular expression.
#' @param ... Passed to \code{\link{grepl}}.
#' @return A logical vector that is \code{TRUE} when the input matches the regular expression.
#' @seealso \code{\link{regex}} and \code{\link{regexpr}}.
matches_regex <- function(x, rx, ...)
{
  call_and_name(function(x) grepl(rx, x, ...), x)
}

#' NA, with a cause of failure.
#'
#' Always returns the value (logical) \code{NA}, with a cause attribute.
#'
#' @param ... Passed to sprintf to create a cause of failure message.
#' @return \code{NA} with the attribute \code{cause}, as provided
#' in the input.
#' @seealso \code{\link{cause}} and \code{\link{false}}.
na <- function(...)
{
  msg <- if(length(list(...)) > 0L) sprintf(...) else ""
  x <- NA
  cause(x) <- msg
  x
}

#' @rdname strip_non_numeric
strip_non_alphanumeric <- function(x)
{
  x <- coerce_to(x, "character")
  invalid_chars <- "[^[:alnum:]]+" 
  if(any(grepl(invalid_chars, x)))
  {
    warning("Removing non-alphanumeric characters from input.")
    x <- gsub(invalid_chars, "", x)
  }
  x
}

#' Removes non-numeric characters from a string.
#' 
#' Removes non-numeric characters from a string, leaving only digits.
#' @param x Input to strip.
#' @param allow_X If \code{TRUE}, the letter "X" is also allowed.  (Useful for some 
#' check digits.)
#' @return A character vector of the same length as \code{x}, consisting of strings 
#' of digits in the case of \code{strip_non_numeric}, and strings of digits and numbers
#' in the case of \code{strip_non_alphanumeric}.
strip_non_numeric <- function(x, allow_X = FALSE)
{
  x <- coerce_to(x, "character")
  invalid_chars <- if(allow_X) "[^[:digit:]X]+" else "[^[:digit:]]+"
  if(any(grepl(invalid_chars, x)))
  {
    warning("Removing non-digit characters from input.")
    x <- gsub(invalid_chars, "", x)
  }
  x
}

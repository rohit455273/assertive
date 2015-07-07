#' Get or set the \code{"cause"} attribute.
#'
#' Gets or sets the \code{"cause"} (of failure) attribute of a variable.
#'
#' @param x Any variable.
#' @param value Passed to \code{gettextf} and stored in the \code{"cause"}
#' attribute.
#' @return The get method returns the \code{"cause"} attribute.
#' @examples
#' yn <- is_a_bool(123)
#' cause(yn)
#' @export
cause <- function(x)
{
  attr(x, "cause")
}

#' @rdname cause
#' @export
`cause<-` <- function(x, value)
{
  # Can't use is_scalar here due to dependency on this
  if(length(value) != 1 && length(value) != length(x))
  {
    stop(
      gettextf(
        "The length of value should be 1 or the length of x (%d) but is %d.", 
        length(x),
        length(value)
      )
    )
  }
  attr(x, "cause") <- noquote(as.character(value))
  x
}

#' Set a cause and return the input
#' 
#' Sets the cause attribute of an object and returns that object.
#' @param x A variable.
#' @param value A character vector to set the cause to, where \code{x} is
#' not \code{TRUE}.
#' @details If \code{x} is \code{TRUE} everywhere, this returns the input 
#' without setting a cause.  Otherwise, the cause is an empty string where 
#' \code{x} is \code{TRUE}, and \code{value} elsewhere.
#' @return \code{x}, with a new cause attribute.
#' @seealso \code{\link{cause}} , \code{\link[stats]{setNames}}
set_cause <- function(x, value)
{
  if(all(!is.na(x) & x)) return(x)
  cause(x) <- ifelse(
    is.na(x), 
    "missing", 
    ifelse(x, "", value)
  )
  x
}

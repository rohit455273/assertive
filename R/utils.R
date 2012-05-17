#' Get the name of a variable in the parent frame.
#'
#' Gets the name of the input in the parent frame.
#'
#' @param x Variable to get the name of.
#' @return A string giving the name of the input in the parent frame.
#' @export
get_name_in_parent <- function(x)
{  
  deparse(do.call(
    substitute, 
    list(substitute(x), parent.frame())
  ))
}

#' Get or set the message attribute.
#'
#' Gets or sets the message attribute of a variable.
#'
#' @param x Any variable.
#' @return The get method returns the \code{"message"} attribute.
#' @examples
#' yn <- is_a_bool(123)
#' msg(yn)
#' @export
msg <- function(x)
{
  attr(x, "message")
}

#' @param value The value to set the message attribute to.
#' @rdname msg
#' @export
`msg<-` <- function(x, value)
{
  attr(x, "message") <- as.character(value)
  x
}

#' Only use the first element of a vector.
#'
#' If the input is not scalar, then only the first element is returned, 
#' with a warning.
#'
#' @param x Input that should be scalar
#' @return If \code{x} is scalar, it is returned unchanged, otherwise
#' only the first element is returned, with a warning.
#' @export
use_first <- function(x)
{
  assert_is_vector(x)
  assert_is_non_empty(x)
  if(!is_scalar(x))
  {
    warning(
      "Only the first value of ", sQuote(deparse(substitute(x))), " will be used.",
      call. = FALSE
      )
    x <- x[[1]]
  }
  x
}

#' Is the input (not) NaN?
#'
#' Checks to see if the input is a number that is(n't) NaN.
#'
#' @param x Input to check.
#' @return \code{is_nan} wraps \code{is.nan}, coercing the input to
#' numeric if necessary.  \code{is_not_nan} works similarly, but returns
#' the negation.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns
#' \code{FALSE}.
#' @seealso \code{\link[base]{is.nan}}
#' @examples
#' assert_is_not_nan(1:10)
#' @export
is_nan <- function(x)
{
  x <- coerce_to(x, "numeric")
  is.nan(x)
}

#' Is the input present?
#'
#' Checks to see if the input is not NA.
#'
#' @param x Input to check.
#' @return \code{is_not_na} is the negation of \code{is.na}. 
#' \code{assert_is_not_na} returns nothing but throws an error if 
#' \code{is_not_na} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.na}}
#' @examples
#' assert_is_not_na(1:10)
#' @export
is_not_na <- function(x)
{
  !is.na(x)
}

#' @rdname is_nan
#' @export
is_not_nan <- function(x)
{
  x <- coerce_to(x, "numeric")
  !is.nan(x)
}

#' @rdname is_null
#' export
is_not_null <- function(x, .xname = get_name_in_parent(x))
{
  if(is.null(x))
  {
    return(false("%s is NULL.", .xname))
  }
  TRUE
}

#' Is the input (not) null?
#'
#' Checks to see if the input is (not) null.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_null} wraps \code{is.null}, providing more 
#' information on failure. \code{is_not_null} returns \code{TRUE} in
#' the opposite case.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{is.null}}.
#' @examples
#' assert_is_null(NULL)
#' assert_is_null(c())
#' assert_is_not_null(NA
#' @export
is_null <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.null(x))
  {
    return(false("%s is not NULL.", .xname))
  }
  TRUE
}

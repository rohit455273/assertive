#' @rdname is_true
#' @export
is_identical_to_false <- function(x, allow_attributes = FALSE, 
  .xname = get_name_in_parent(x))
{
  if(allow_attributes) 
  {
    x <- strip_attributes(x)
  }
  if(!identical(FALSE, x)) 
  {
    return(false("%s is not identical to FALSE.", .xname))
  }
  TRUE
}                  

#' @rdname is_true
#' @export
is_identical_to_na <- function(x, allow_attributes = FALSE, 
  .xname = get_name_in_parent(x))
{
  if(allow_attributes) 
  {
    x <- strip_attributes(x)
  }
  if(!identical(NA, x) && 
     !identical(NA_real_, x) && 
     !identical(NA_character_, x) && 
     !identical(NA_integer_, x) && 
     !identical(NA_complex_, x))
  {
    return(false("%s is not identical to NA.", .xname))
  }
  TRUE
}

#' @rdname is_true
#' @export
is_identical_to_true <- function(x, allow_attributes = FALSE, 
                                 .xname = get_name_in_parent(x))
{
  if(allow_attributes) 
  {
    x <- strip_attributes(x)
  }
  if(!identical(TRUE, x))
  {
    return(false("%s is not identical to TRUE.", .xname))
  }
  TRUE
}

#' Is suitable to be used as an if condition
#' 
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_if_condition} returns \code{TRUE} if the input is 
#' scalar \code{TRUE} or \code{FALSE}.
#' @examples
#' is_if_condition(TRUE)
#' is_if_condition(FALSE)
#' is_if_condition(NA)
#' is_if_condition(c(TRUE, FALSE))
#' is_if_condition("the truth")
#' # You can pass a number as a logical condition, but you shouldn't,
#' # so the next line returns FALSE.
#' is_if_condition(1)
#' dont_stop(assert_is_if_condition(raw(1)))
#' @export
is_if_condition <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_logical(x, .xname)))
  {
    return(ok)
  }
  if(!(ok <- is_scalar(x, "length", .xname)))
  {
    return(ok)
  }
  if(!is_not_na(x))
  {
    return(false("%s is NA.", .xname))
  }
  TRUE
}

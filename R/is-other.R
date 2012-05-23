# ' Is the input generic?
# '
# ' Checks to see if the input is a generic function.
# '
# ' @param x Input to check.
#' @param .xname Not intended to be used directly.
# ' @return \code{TRUE} if the input is a generic function. 
# ' \code{assert_is_generic} functions return nothing but throws an error
# ' if \code{is_generic} returns \code{FALSE}.
# ' @seealso \code{\link[methods]{GenericFunctions}}.
# ' @examples
# ' 
# ' @export
# is_generic <- function(x)
# {       
#   x <- use_first(x)  
#   if(!is.function(x)) return(false("Input is not a function"))  
#   fn_name <- get_name_in_parent(x)
#   if(fn_name %in% utils:::getKnownS3generics()) return(TRUE)      
#   where <- find(fn_name, mode = "function")
#   gen <- utils:::findGeneric(fn_name, envir = as.environment(where))
#   if(!nzchar(gen)) return(false("Input is not generic."))
#   TRUE
# }     

#' Is the input DLL loaded?
#'
#' Checks to see if the input DLL (a.k.a. shared object) is loaded.
#'
#' @param x Input to check.
#' @param PACKAGE Passed to \code{is.loaded}.
#' @param type Passed to \code{is.loaded}.
#' @param .xname Not intended to be used directly.
#' @return \code{is_loaded} wraps \code{is.loaded}, providing more 
#' information on failure.
#' @seealso \code{\link[base]{is.loaded}}.
is_loaded <- function(x, PACKAGE = "", type = "", .xname = get_name_in_parent(x))
{
  if(!is.loaded(x, PACKAGE = PACKAGE, type = type))
  {
    return(false("%s is not loaded.", .xname))
  }
}

#' Are you running R?
#'
#' Checks to see you are running R.
#'
#' @return \code{is_R} wraps \code{is.R}, providing more 
#' information on failure.  \code{assert_is_R} returns nothing but
#' throws an error if \code{is_R} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.R}}.
#' @examples
#' assert_is_R()
#' @export
is_R <- function()
{
  if(!is.R())
  {
    return(false("You are not running R."))
  } 
  TRUE
}

#' Is the input a symmetric matrix?
#'
#' Checks that the input is a symmetric matrix.
#' 
#' @param x Input to check.
#' @param tol Differences smaller than \code{tol} are not considered.
#' @param .xname Not intended to be used directly.
#' @param ... Passed to \code{all.equal}.
#' @return \code{TRUE} if the input is symmetrix (after coersion to be a matrix).
#' @examples
#' m <- diag(3); m[3, 1] <- 1e-100
#' assert_is_symmetric_matrix(m)
#' \dontrun{
#' assert_is_symmetric_matrix(m, tol = 0)
#'}
#' @export
is_symmetric_matrix <- function(x, tol = 100 * .Machine$double.eps, .xname = get_name_in_parent(x), ...)
{
  x <- coerce_to(x, "matrix")
  dimx <- dim(x)
  if(dimx[1L] != dimx[2L])
  {
    return(false("%s is not a square matrix.", .xname))
  }
  symmetry_test <- if(is.complex(x)) 
  {
    all.equal.numeric(x, Conj(t(x)), tolerance = tol, ...)
  } else 
  {
    all.equal(x, t(x), tolerance = tol, ...)
  }
  if(!is_true(symmetry_test))
  {
    return(false("%s is not a symmetric matrix.", .xname))
  }
  TRUE
}

#' Is the input unsorted?
#' 
#' Checks to see if the input is unsorted (without the cost of sorting it).
#'
#' @param x Input to check.
#' @param na.rm If \code{TRUE}, remove \code{NA}s before checking.
#' @param strictly If \code{TRUE}, equal values count as unsorted.
#' @param .xname Not intended to be used directly.
#' @return \code{is_unsorted} reimplements \code{is.unsorted}, providing
#' more information on failure.  \code{assert_is_unsorted} returns nothing 
#' but throws an error if \code{is_unsorted} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.unsorted}}.
#' @examples
#' assert_is_unsorted(c(1, 3, 2))
#' assert_is_unsorted(c(1, 1, 2), strictly = TRUE)
#' \dontrun{
#' #These tests should fail:
#' assert_is_unsorted(c(1, 1, 2))
#' assert_is_unsorted(c(2, 1, 0))
#' }
#' @export
is_unsorted <- function(x, na.rm = FALSE, strictly = FALSE, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_not_null(x))) return(ok)
  if(!is.atomic(x) && length(x) > 1)
  {
    #See notes in Value section of ?is.unsorted.
    return(na("Sortability is not tested for recursive objects of length greater than one."))
  }
  nas <- is.na(x)
  if(any(nas))
  {
    if(!na.rm) 
    {
      return(na("%s contains NA values.", .xname))
    }
    x <- x[!nas]
  }
  if(!.Internal(is.unsorted(x, strictly)))
  {
    return(false("%s is sorted.", .xname))
  }
  TRUE
}

#' Is the input a whole number?
#'
#' Checks that the (probably floating point) input is a whole number.
#' 
#' @param x Input to check.
#' @param tol Differences smaller than \code{tol} are not considered.
#' @note The term whole number is used to distinguish from integer in
#' that the input \code{x} need not have type \code{integer}.  In fact
#' it is expected that \code{x} will be \code{numeric}.
#' @return \code{TRUE} if the input is a whole number.
is_whole_number <- function(x, tol = 100 * .Machine$double.eps)
{
  x <- coerce_to(x, "numeric")
  abs(x - floor(x)) < tol
}

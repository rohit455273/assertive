#' Is the input a symmetric matrix?
#'
#' Checks that the input is a symmetric matrix.
#' 
#' @param x Input to check.
#' @param tol Differences smaller than \code{tol} are not considered.
#' @param .xname Not intended to be used directly.
#' @param ... Passed to \code{all.equal}.
#' @return \code{TRUE} if the input is symmetrix (after coersion to be a 
#' matrix).
#' @examples
#' m <- diag(3); m[3, 1] <- 1e-100
#' assert_is_symmetric_matrix(m)
#' #These examples should fail.
#' dont_stop(assert_is_symmetric_matrix(m, tol = 0))
#' @export
is_symmetric_matrix <- function(x, tol = 100 * .Machine$double.eps, 
  .xname = get_name_in_parent(x), ...)
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
  if(!is_identical_to_true(symmetry_test))
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
#' @note The builtin function \code{is.unsorted} usually returns \code{NA}
#' when the input is recursive and has length 2, though for some
#' classes (particularly data.frames) it returns a \code{TRUE} or
#' \code{FALSE} value.  The logic behind those is difficult to
#' interpret, and gives odd results, so \code{is_unsorted} always
#' returns \code{NA} in this case.
#' @seealso \code{\link[base]{is.unsorted}}.
#' @examples
#' assert_is_unsorted(c(1, 3, 2))
#' assert_is_unsorted(c(1, 1, 2), strictly = TRUE)
#' #These tests should fail.
#' dont_stop(assert_is_unsorted(c(1, 1, 2)))
#' dont_stop(assert_is_unsorted(c(2, 1, 0)))
#' @export
is_unsorted <- function(x, na.rm = FALSE, strictly = FALSE, 
  .xname = get_name_in_parent(x))
{
  if(!(ok <- is_not_null(x))) return(ok)
  if(!is.atomic(x) && length(x) > 1)
  {
    #See notes in Value section of ?is.unsorted.
    return(na(
      "Sortability is not tested for recursive objects of length greater than one."
    ))
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
  if(!is.unsorted(x, strictly = strictly))
  {
    return(false("%s is sorted.", .xname))
  }
  TRUE
}


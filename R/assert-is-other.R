#' @rdname is_symmetric_matrix
#' @export
assert_is_symmetric_matrix <- function(x, tol = 100 * .Machine$double.eps, ...)
{                                                         
  assert_engine(
    is_symmetric_matrix, 
    x, 
    tol = tol, 
    .xname = get_name_in_parent(x),
    ...
  )       
}

#' @rdname is_unsorted
#' @export
assert_is_unsorted <- function(x, na.rm = FALSE, strictly = FALSE)
{                                                         
  assert_engine(
    is_unsorted, 
    x,
    na.rm = na.rm,
    strictly = strictly, 
    .xname = get_name_in_parent(x)
  )       
}

#' @rdname is_debugged
#' @export
assert_is_debugged <- function(x)
{                                                         
  assert_engine(x, is_debugged, .xname = get_name_in_parent(x))       
}

# ' @rdname is_generic
# ' @export
# assert_is_generic <- function(x)
# {                                                     
#   msg <- sprintf("%s is not a generic function.", get_name_in_parent(x))
#   assert_engine(x, is_generic, msg)        
# }

#' @rdname is_loaded
#' @export
assert_is_loaded <- function(x)
{                                                         
  assert_engine(x, is_loaded, .xname = get_name_in_parent(x))       
}

#' @rdname is_R
#' @export
assert_is_R <- function()
{                                                         
  assert_engine(predicate = is_R)        
}

#' @rdname is_symmetric_matrix
#' @export
assert_is_symmetric_matrix <- function(x, tol = 100 * .Machine$double.eps, ...)
{                                                         
  assert_engine(
    x, 
    is_symmetric_matrix, 
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
    x, 
    is_unsorted, 
    .xname = get_name_in_parent(x),
    na.rm = na.rm,
    strictly = strictly
  )       
}

#' @rdname is_whole_number
#' @export
assert_all_numbers_whole_numbers <- function(x, tol = .Machine$double.eps)
{                                                       
  msg <- sprintf("%s are not all whole numbers.", get_name_in_parent(x))
  assert_engine(x, is_whole_number, msg, tol = tol)
}

#' @rdname is_whole_number
#' @export
assert_any_numbers_whole_numbers <- function(x, tol = .Machine$double.eps)
{                                                      
  msg <- sprintf("%s are all not whole numbers.", get_name_in_parent(x))
  assert_engine(x, is_whole_number, msg, what = "any", tol = tol)
}

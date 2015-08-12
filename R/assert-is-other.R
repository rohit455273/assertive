#' @rdname is_existing
#' @export
assert_all_are_existing <- function(
  x, 
  envir = parent.frame(),
  inherits = TRUE
)
{    
  msg <- gettextf("%s do not all exist.", get_name_in_parent(x))
  assert_engine(
    is_existing, 
    x,
    envir = envir,
    inherits = inherits, 
    msg = msg
  )       
}

#' @rdname is_existing
#' @export
assert_any_are_existing <- function(
  x, 
  envir = parent.frame(), 
  inherits = TRUE
)
{    
  msg <- gettextf("%s all do not exist.", get_name_in_parent(x))
  assert_engine(
    is_existing, 
    x, 
    envir = envir,
    inherits = inherits,
    msg = msg,
    what = "any"
  )       
}

#' @rdname is_debugged
#' @export
assert_is_debugged <- function(x)
{                                                         
  assert_engine(is_debugged, x, .xname = get_name_in_parent(x))       
}

# ' @rdname is_generic
# ' @export
# assert_is_generic <- function(x)
# {                                                     
#   msg <- gettextf("%s is not a generic function.", get_name_in_parent(x))
#   assert_engine(is_generic, x, msg = msg)        
# }

#' @rdname is_if_condition
#' @export
assert_is_if_condition <- function(x)
{
  assert_engine(is_if_condition, x, .xname = get_name_in_parent(x))
}

#' @rdname is_loaded
#' @export
assert_is_loaded <- function(x)
{                                                         
  assert_engine(is_loaded, x, .xname = get_name_in_parent(x))       
}

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

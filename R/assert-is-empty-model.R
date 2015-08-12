#' @rdname is_empty_model
#' @export
assert_is_empty_model <- function(x)
{                                                     
  assert_engine(is_empty_model, x, .xname = get_name_in_parent(x))     
}

' @rdname is_empty_model
#' @export
assert_is_non_empty_model <- function(x)
{                                                     
  assert_engine(is_non_empty_model, x, .xname = get_name_in_parent(x))    
}

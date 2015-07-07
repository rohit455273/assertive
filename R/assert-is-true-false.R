#' @rdname is_true
#' @export
assert_all_are_false <- function(x)
{                                                     
  msg <- gettextf("%s are not all false.", get_name_in_parent(x))
  assert_engine(is_false, x, msg = msg)        
}

#' @rdname is_true
#' @export
assert_any_are_false <- function(x)
{                                                     
  msg <- gettextf("%s are all not false.", get_name_in_parent(x))
  assert_engine(is_false, x, msg = msg, what = "any")        
}

#' @rdname is_true
#' @export
assert_is_identical_to_false <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    is_identical_to_false,
    x, 
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )      
}

#' @rdname is_true
#' @export
assert_is_identical_to_true <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    is_identical_to_true,
    x,
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )    
}

#' @rdname is_if_condition
#' @export
assert_is_if_condition <- function(x)
{
  assert_engine(is_if_condition, x, .xname = get_name_in_parent(x))
}

#' @rdname is_true
#' @export
assert_all_are_true <- function(x)
{                                                     
  msg <- gettextf("%s are not all true.", get_name_in_parent(x))
  assert_engine(is_true, x, msg = msg)        
}

#' @rdname is_true
#' @export
assert_any_are_true <- function(x)
{                                                     
  msg <- gettextf("%s are all not true.", get_name_in_parent(x))
  assert_engine(is_true, x, msg = msg, what = "any")        
}

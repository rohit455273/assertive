#' @rdname is_empty_character
#' @export
assert_all_are_empty_character <- function(x)
{                        
  msg <- gettextf(
    "%s are not all empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(is_empty_character, x, msg = msg)   
}

#' @rdname is_empty_character
#' @export
assert_any_are_empty_character <- function(x)
{                        
  msg <- gettextf(
    "%s are all not empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(is_empty_character, x, msg = msg, what = "any")   
}

#' @rdname is_empty_character
#' @export
assert_all_are_non_empty_character <- function(x)
{                        
  msg <- gettextf(
    "%s are not all non-empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(is_non_empty_character, x, msg = msg)   
}

#' @rdname is_empty_character
#' @export
assert_any_are_non_empty_character <- function(x)
{                        
  msg <- gettextf(
    "%s are all not non-empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(is_non_empty_character, x, msg = msg, what = "any")   
}

#' @rdname is_empty_character
#' @export
assert_all_are_missing_or_empty_character <- function(x)
{                        
  msg <- gettextf(
    "%s are not all missing or empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(is_missing_or_empty_character, x, msg = msg)   
}

#' @rdname is_empty_character
#' @export
assert_any_are_missing_or_empty_character <- function(x)
{                        
  msg <- gettextf(
    "%s are all not missing or empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(is_missing_or_empty_character, x, msg = msg, what = "any")   
}

#' @rdname is_empty_character
#' @export
assert_all_are_non_missing_nor_empty_character <- function(x)
{                        
  msg <- gettextf(
    "%s are not all non-missing nor non-empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(is_non_missing_nor_empty_character, x, msg = msg)   
}

#' @rdname is_empty_character
#' @export
assert_any_are_non_missing_nor_empty_character <- function(x)
{                        
  msg <- gettextf(
    "%s are all not non-missing nor non-empty strings.", 
    get_name_in_parent(x)
  )
  assert_engine(is_non_missing_nor_empty_character, x, msg = msg, what = "any")   
}


#' @rdname is_empty_character
#' @export
assert_is_an_empty_string <- function(x)
{                                                  
  assert_engine(is_an_empty_string, x, .xname = get_name_in_parent(x))     
}

#' @rdname is_empty_character
#' @export
assert_is_a_non_empty_string <- function(x)
{                                                     
  assert_engine(is_a_non_empty_string, x, .xname = get_name_in_parent(x))    
}  

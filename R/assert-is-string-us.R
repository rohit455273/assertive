#' @rdname is_us_zip_code
#' @export
assert_all_are_us_zip_codes <- function(x)
{                                                     
  msg <- sprintf("%s are not all US zip codes.", get_name_in_parent(x))
  assert_engine(x, is_us_zip_code, msg)        
}

#' @rdname is_us_zip_code
#' @export
assert_any_are_us_zip_codes <- function(x)
{                                                     
  msg <- sprintf("%s are all not US zip codes.", get_name_in_parent(x))
  assert_engine(x, is_us_zip_code, msg, what = "any")        
}

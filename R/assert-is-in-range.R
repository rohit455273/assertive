#' @rdname is_in_range
#' @export
assert_all_are_in_closed_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- gettextf(
    "%s are not all in the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, FALSE, FALSE)
  )
  assert_engine(x, is_in_closed_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_closed_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- gettextf(
    "%s are all out of the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, FALSE, FALSE)
  )
  assert_engine(
    x, 
    is_in_closed_range, 
    msg, 
    what = "any", 
    lower = lower, 
    upper = upper
  )  
}

#' @rdname is_in_range
#' @export
assert_all_are_in_left_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- gettextf(
    "%s are not all in the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, TRUE, FALSE)
  )
  assert_engine(x, is_in_left_open_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_left_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- gettextf(
    "%s are all out of the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, TRUE, FALSE)
  )
  assert_engine(
    x, 
    is_in_left_open_range, 
    msg, 
    what = "any", 
    lower = lower, 
    upper = upper
  )  
}

#' @rdname is_in_range
#' @export
assert_all_are_in_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- gettextf(
    "%s are not all in the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, TRUE, TRUE)
  )
  assert_engine(x, is_in_open_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <-   msg <- gettextf(
    "%s are all out of the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, TRUE, TRUE)
  )
  assert_engine(
    x, 
    is_in_open_range, 
    msg, 
    what = "any", 
    lower = lower, 
    upper = upper
  )  
}

#' @rdname is_in_range
#' @export
assert_all_are_in_range <- function(x, lower = -Inf, upper = Inf, 
  lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                     
  msg <- gettextf(
    "%s are not all in the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, lower_is_strict, upper_is_strict)
  )
  assert_engine(
    x, 
    is_in_range, 
    msg, 
    lower = lower, 
    upper = upper, 
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
  ) 
}

#' @rdname is_in_range
#' @export
assert_any_are_in_range <- function(x, lower = -Inf, upper = Inf, 
  lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                     
  msg <- gettextf(
    "%s are all out of the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, lower_is_strict, upper_is_strict)
  )
  assert_engine(
    x, 
    is_in_range, 
    msg, 
    what = "any",
    lower = lower, 
    upper = upper, 
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
  )
}

#' @rdname is_in_range
#' @export
assert_all_are_in_right_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- gettextf(
    "%s are not all in the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, FALSE, TRUE)
  )
  assert_engine(x, is_in_right_open_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_right_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- gettextf(
    "%s are all out of the range %s.", 
    get_name_in_parent(x),
    make_range_string(lower, upper, FALSE, TRUE)
  )
  assert_engine(
    x, 
    is_in_right_open_range, 
    msg, 
    what = "any", 
    lower = lower, 
    upper = upper
  )  
}

#' @rdname is_in_range
#' @export
assert_all_are_negative <- function(x)
{                                                                
  msg <- gettextf("%s are not all negative.", get_name_in_parent(x))
  assert_engine(x, is_negative, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_negative <- function(x)
{                                                        
  msg <- gettextf("%s are all not negative.", get_name_in_parent(x))
  assert_engine(x, is_negative, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_non_negative <- function(x)
{                                                       
  msg <- gettextf("%s are not all non-negative.", get_name_in_parent(x))
  assert_engine(x, is_non_negative, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_non_negative <- function(x)
{                                                      
  msg <- gettextf("%s are all not non-negative.", get_name_in_parent(x))
  assert_engine(x, is_non_negative, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_non_positive <- function(x)
{                                                       
  msg <- gettextf("%s contains positive values.", get_name_in_parent(x))
  assert_engine(x, is_non_positive, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_non_positive <- function(x)
{                                                      
  msg <- gettextf("%s are all positive.", get_name_in_parent(x))
  assert_engine(x, is_non_positive, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_percentages <- function(x, lower_is_strict = FALSE, 
  upper_is_strict = FALSE)
{                                                       
  msg <- gettextf(
    "%s are not all in the range %s.", 
    get_name_in_parent(x),
    make_range_string(0, 100, lower_is_strict, upper_is_strict)
  )
  assert_engine(
    x, 
    is_percentage, 
    msg, 
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
  )
}

#' @rdname is_in_range
#' @export
assert_any_are_percentages <- function(x, lower_is_strict = FALSE, 
  upper_is_strict = FALSE)
{                                                       
  msg <- gettextf(
    "%s are all out of the range %s.", 
    get_name_in_parent(x),
    make_range_string(0, 100, lower_is_strict, upper_is_strict)
  )
  assert_engine(
    x, 
    is_percentage, 
    msg, 
    what = "any",
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
    )
}  

#' @rdname is_in_range
#' @export
assert_all_are_positive <- function(x)
{                                                       
  msg <- gettextf("%s contains non-positive values.", get_name_in_parent(x))
  assert_engine(x, is_positive, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_positive <- function(x)
{                                                      
  msg <- gettextf("%s are all non-positive.", get_name_in_parent(x))
  assert_engine(x, is_positive, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_proportions <- function(x, lower_is_strict = FALSE, 
  upper_is_strict = FALSE)
{                                                       
  msg <- gettextf(
    "%s are not all in the range %s.", 
    get_name_in_parent(x),
    make_range_string(0, 1, lower_is_strict, upper_is_strict)
  )
  assert_engine(
    x, 
    is_proportion, 
    msg, 
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
    )
}

#' @rdname is_in_range
#' @export
assert_any_are_proportions <- function(x, lower_is_strict = FALSE, 
  upper_is_strict = FALSE)
{                                                       
  msg <- gettextf(
    "%s are all out of the range %s.", 
    get_name_in_parent(x),
    make_range_string(0, 1, lower_is_strict, upper_is_strict)
  )
  assert_engine(
    x, 
    is_proportion, 
    msg, 
    what = "any",
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
  )
}  

#' Make a range string
#' 
#' Makes a range string using mathematical notation.
#' @param lower A number for the lower bound.
#' @param upper A number for the upper bound.
#' @param lower_is_strict Should the lower bound be included?
#' @param upper_is_strict Should the upper bound be included?
#' @return A string denoting the range.
#' @note Not vectorized across the \code{lower_is_strict} and 
#' \code{upper_is_strict} args for speed.
#' @examples 
#' \donttest{
#' make_range_string(-1.2345, 6.7890, TRUE, FALSE)
#' }
make_range_string <- function(lower, upper, lower_is_strict, upper_is_strict)
{
  left <- if(lower_is_strict) "(" else "["
  right <- if(upper_is_strict) ")" else "]"
  paste0(left, lower, ",", upper, right)  
}

#' @rdname is2  
#' @export
assert_is <- function(x, class, .xname = get_name_in_parent(x))
{  
  assert_engine(x, is2, class = class, .xname = .xname)
}

#' @rdname is_logical
#' @export
assert_is_a_bool <- function(x)
{      
  assert_engine(x, is_a_bool, .xname = get_name_in_parent(x))    
}

#' @rdname is_numeric
#' @export
assert_is_a_number <- function(x)
{                                                          
  assert_engine(x, is_a_number, .xname = get_name_in_parent(x))   
}

#' @rdname is_character
#' @export
assert_is_a_non_empty_string <- function(x)
{                                                     
  assert_engine(x, is_a_non_empty_string, .xname = get_name_in_parent(x))    
}  

#' @rdname is_raw
#' @export
assert_is_a_raw <- function(x)
{                                                          
  assert_engine(x, is_a_raw, .xname = get_name_in_parent(x))      
}

#' @rdname is_character
#' @export
assert_is_a_string <- function(x)
{                                                         
  assert_engine(x, is_a_string, .xname = get_name_in_parent(x))   
}

#' @rdname is_character
#' @export
assert_is_an_empty_string <- function(x)
{                                                  
  assert_engine(x, is_an_empty_string, .xname = get_name_in_parent(x))     
}

#' @rdname is_integer
#' @export
assert_is_an_integer <- function(x)
{
  assert_engine(x, is_an_integer, .xname = get_name_in_parent(x)) 
}

#' @rdname is_array
#' @export
assert_is_array <- function(x)
{                                                         
  assert_engine(x, is_array, .xname = get_name_in_parent(x))       
}

#' @rdname is_atomic
#' @export
assert_is_atomic <- function(x)
{                                                         
  assert_engine(x, is_atomic, .xname = get_name_in_parent(x)) 
}

#' @rdname is_language
#' @export
assert_is_call <- function(x)
{                                                         
  assert_engine(x, is_call, .xname = get_name_in_parent(x))       
}

#' @rdname is_character
#' @export
assert_is_character <- function(x)
{                                                         
  assert_engine(x, is_character, .xname = get_name_in_parent(x))   
}

#' @rdname is_complex
#' @export
assert_is_complex <- function(x)
{                                                         
  assert_engine(x, is_complex, .xname = get_name_in_parent(x))   
}

#' @rdname is_data.frame
#' @export
assert_is_data.frame <- function(x)
{                                                         
  assert_engine(x, is_data.frame, .xname = get_name_in_parent(x))
}

#' @rdname is_empty
#' @export
assert_is_empty <- function(x)
{                                                  
  assert_engine(x, is_empty, .xname = get_name_in_parent(x))      
}

#' @rdname is_empty_model
#' @export
assert_is_empty_model <- function(x)
{                                                     
  assert_engine(x, is_empty_model, .xname = get_name_in_parent(x))     
}

#' @rdname is_environment
#' @export
assert_is_environment <- function(x)
{                                                         
  assert_engine(x, is_environment, .xname = get_name_in_parent(x))     
}

#' @rdname is_language
#' @export
assert_is_expression <- function(x)
{                                                         
  assert_engine(x, is_expression, .xname = get_name_in_parent(x))     
}

#' @rdname is_factor
#' @export
assert_is_factor <- function(x)
{                                                         
  assert_engine(x, is_factor, .xname = get_name_in_parent(x))  
}

#' @rdname is_true
#' @export
assert_is_false <- function(x, allow_attributes = FALSE)
{                                                  
  assert_engine(
    x, 
    assertive::is_false,  #avoid conflict with testthat::is_false
    allow_attributes = allow_attributes, 
    .xname = get_name_in_parent(x)
  )      
}

#' Are the inputs (in)finite?
#'
#' Checks to see if the inputs are (in)finite.
#'
#' @param x Input to check.
#' @return \code{assert_all_are_finite} and \code{assert_any_are_finite}
#' return nothing but throw an error if the inputs to \code{is.finite}
#' are not \code{TRUE}. \code{assert_all_are_infinite} and 
#' \code{assert_any_are_infinite} work likewise for \code{is.infinite}.
#' @note Note that there are no corresponding \code{is_finite} and 
#' \code{is_finite} functions in the package.  Use \code{is.finite}
#' and \code{is.infinite} instead.
#' @seealso \code{\link[base]{is.finite}}
#' @examples
#' assert_all_are_finite(1:10)
#' assert_any_are_finite(c(1, Inf))
#' @export
assert_all_are_finite <- function(x)
{                                                     
  msg <- sprintf("%s are not all finite.", get_name_in_parent(x))
  assert_engine(x, is.finite, msg)        
}

#' @rdname assert_all_are_finite
#' @export
assert_any_are_finite <- function(x)
{                                                     
  msg <- sprintf("%s are all not finite.", get_name_in_parent(x))
  assert_engine(x, is.finite, msg, what = "any")        
}

#' @rdname is_function
#' @export
assert_is_function <- function(x)
{                                                         
  assert_engine(x, is_function, .xname = get_name_in_parent(x))     
}

# ' @rdname is_generic
# ' @export
# assert_is_generic <- function(x)
# {                                                     
#   msg <- sprintf("%s is not a generic function.", get_name_in_parent(x))
#   assert_engine(x, is_generic, msg)        
# }

#' @rdname is_real
#' @export
assert_all_are_imaginary <- function(x)
{                                                     
  msg <- sprintf("%s are not all imaginary.", get_name_in_parent(x))
  assert_engine(x, is_imaginary, msg)        
}

#' @rdname is_real
#' @export
assert_any_are_imaginary <- function(x)
{                                                     
  msg <- sprintf("%s are all not imaginary.", get_name_in_parent(x))
  assert_engine(x, is_imaginary, msg, what = "any")        
}

#' @rdname assert_all_are_finite
#' @export
assert_all_are_infinite <- function(x)
{                                                     
  msg <- sprintf("%s are not all infinite.", get_name_in_parent(x))
  assert_engine(x, is.infinite, msg)        
}

#' @rdname assert_all_are_finite
#' @export
assert_any_are_infinite <- function(x)
{                                                     
  msg <- sprintf("%s are all not infinite.", get_name_in_parent(x))
  assert_engine(x, is.infinite, msg, what = "any")        
}

#' @rdname is_in_range
#' @export
assert_all_are_in_closed_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
  assert_engine(x, is_in_closed_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_closed_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
  assert_engine(x, is_in_closed_range, msg, what = "any", lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_all_are_in_left_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
  assert_engine(x, is_in_left_open_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_left_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
  assert_engine(x, is_in_left_open_range, msg, what = "any", lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_all_are_in_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
  assert_engine(x, is_in_open_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
  assert_engine(x, is_in_open_range, msg, what = "any", lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_all_are_in_range <- function(x, lower = -Inf, upper = Inf, lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                     
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
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
assert_any_are_in_range <- function(x, lower = -Inf, upper = Inf, lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
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
  msg <- sprintf("%s are not all in range.", get_name_in_parent(x))
  assert_engine(x, is_in_right_open_range, msg, lower = lower, upper = upper)  
}

#' @rdname is_in_range
#' @export
assert_any_are_in_right_open_range <- function(x, lower = -Inf, upper = Inf)
{                                                     
  msg <- sprintf("%s are all out of range.", get_name_in_parent(x))
  assert_engine(x, is_in_right_open_range, msg, what = "any", lower = lower, upper = upper)  
}

#' @rdname is_integer
#' @export
assert_is_integer <- function(x)
{                                                         
  assert_engine(x, is_integer, .xname = get_name_in_parent(x))
}

#' @rdname is_language
#' @export
assert_is_language <- function(x)
{                                                         
  assert_engine(x, is_language, .xname = get_name_in_parent(x))    
}

#' @rdname is_leaf
#' @export
assert_is_leaf <- function(x)
{                                                         
  assert_engine(x, is_leaf, .xname = get_name_in_parent(x))       
}

#' @rdname is_list
#' @export
assert_is_list <- function(x)
{                                                         
  assert_engine(x, is_list, .xname = get_name_in_parent(x))       
}

#' @rdname is_loaded
#' @export
assert_is_loaded <- function(x)
{                                                         
  assert_engine(x, is_loaded, .xname = get_name_in_parent(x))       
}

#' @rdname is_logical
#' @export
assert_is_logical <- function(x)
{                                                         
  assert_engine(x, is_logical, .xname = get_name_in_parent(x))       
}

#' @rdname is_array
#' @export
assert_is_matrix <- function(x)
{                                                         
  assert_engine(x, is_matrix, .xname = get_name_in_parent(x))      
}

#' @rdname is_ts
#' @export
assert_is_mts <- function(x)
{                                                         
  assert_engine(x, is_mts, .xname = get_name_in_parent(x))       
}

#' @rdname is_language
#' @export
assert_is_name <- function(x)
{                                                         
  assert_engine(x, is_name, .xname = get_name_in_parent(x))     
}

#' @rdname is_nan
#' @export
assert_all_are_nan <- function(x)
{                                                                
  msg <- sprintf("%s are not all NaN.", get_name_in_parent(x))
  assert_engine(x, is_nan, msg)
}

#' @rdname is_nan
#' @export
assert_any_are_nan <- function(x)
{                                                                
  msg <- sprintf("%s are all not NaN.", get_name_in_parent(x))
  assert_engine(x, is_nan, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_negative <- function(x)
{                                                                
  msg <- sprintf("%s are not all negative.", get_name_in_parent(x))
  assert_engine(x, is_negative, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_negative <- function(x)
{                                                        
  msg <- sprintf("%s are all not negative.", get_name_in_parent(x))
  assert_engine(x, is_negative, msg, what = "any")
}

#' @rdname is_empty
#' @export
assert_is_non_empty <- function(x)
{                                                     
  assert_engine(x, is_non_empty, .xname = get_name_in_parent(x))    
}

#' @rdname is_empty_model
#' @export
assert_is_non_empty_model <- function(x)
{                                                     
  assert_engine(x, is_non_empty_model, .xname = get_name_in_parent(x))    
}

#' @rdname is_in_range
#' @export
assert_all_are_non_negative <- function(x)
{                                                       
  msg <- sprintf("%s are not all non-negative.", get_name_in_parent(x))
  assert_engine(x, is_non_negative, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_non_negative <- function(x)
{                                                      
  msg <- sprintf("%s are all not non-negative.", get_name_in_parent(x))
  assert_engine(x, is_non_negative, msg, what = "any")
}

#' @rdname is_in_range
#' @export
assert_all_are_non_positive <- function(x)
{                                                       
  msg <- sprintf("%s contains positive values.", get_name_in_parent(x))
  assert_engine(x, is_non_positive, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_non_positive <- function(x)
{                                                      
  msg <- sprintf("%s are all positive.", get_name_in_parent(x))
  assert_engine(x, is_non_positive, msg, what = "any")
}

#' @rdname is_not_na
#' @export
assert_all_are_not_na <- function(x)
{                                                      
  msg <- sprintf("%s contains NAs.", get_name_in_parent(x))
  assert_engine(x, is_not_na, msg)
}

#' @rdname is_not_na
#' @export
assert_any_are_not_na <- function(x)
{                                                      
  msg <- sprintf("%s are all NA.", get_name_in_parent(x))
  assert_engine(x, is_not_na, msg, what = "any")
}

#' @rdname is_nan
#' @export
assert_all_are_not_nan <- function(x)
{                                                      
  msg <- sprintf("%s contains NaNs.", get_name_in_parent(x))
  assert_engine(x, is_not_nan, msg)
}

#' @rdname is_nan
#' @export
assert_any_are_not_nan <- function(x)
{                                                      
  msg <- sprintf("%s are all NaN.", get_name_in_parent(x))
  assert_engine(x, is_not_nan, msg, what = "any")
}

#' @rdname is_null
#' @export
assert_is_not_null <- function(x)
{                                                      
  assert_engine(x, is_null, .xname = get_name_in_parent(x))   
}

#' @rdname is_null
#' @export
assert_is_null <- function(x)
{                                                         
  
  assert_engine(x, is_null, .xname = get_name_in_parent(x))       
}

#' @rdname is_numeric
#' @export
assert_is_numeric <- function(x)
{                                                         
  assert_engine(x, is_numeric, .xname = get_name_in_parent(x)) 
}

#' @rdname is_character
#' @export
assert_all_are_numeric_strings <- function(x)
{                                                     
  msg <- sprintf("%s is not a character of numbers.", get_name_in_parent(x))
  assert_engine(x, is_numeric_string, msg)        
}
  
#' @rdname is_character
#' @export
assert_any_are_numeric_strings <- function(x)
{                                                     
  msg <- sprintf("%s is not a character of numbers.", get_name_in_parent(x))
  assert_engine(x, is_numeric_string, msg, what = "any")        
}

#' @rdname is_factor
#' @export
assert_is_ordered <- function(x)
{                                                         
  assert_engine(x, is_ordered, .xname = get_name_in_parent(x))
}

#' @rdname is_in_range
#' @export
assert_all_are_percentages <- function(x, lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                       
  msg <- sprintf("%s are not all percentages.", get_name_in_parent(x))
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
assert_any_are_percentages <- function(x, lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                       
  msg <- sprintf("%s are all not percentages.", get_name_in_parent(x))
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
  msg <- sprintf("%s contains non-positive values.", get_name_in_parent(x))
  assert_engine(x, is_positive, msg)
}

#' @rdname is_in_range
#' @export
assert_any_are_positive <- function(x)
{                                                      
  msg <- sprintf("%s are all non-positive.", get_name_in_parent(x))
  assert_engine(x, is_positive, msg, what = "any")
}

#' @rdname is_function
#' @export
assert_is_primitive <- function(x)
{                                                         
  assert_engine(x, is_primitive, .xname = get_name_in_parent(x))
}

#' @rdname is_in_range
#' @export
assert_all_are_proportions <- function(x, lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                       
  msg <- sprintf("%s are not all proportions.", get_name_in_parent(x))
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
assert_any_are_proportions <- function(x, lower_is_strict = FALSE, upper_is_strict = FALSE)
{                                                       
  msg <- sprintf("%s are all not proportions.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_proportion, 
    msg, 
    what = "any",
    lower_is_strict = lower_is_strict, 
    upper_is_strict = upper_is_strict
  )
}  

#' @rdname is_qr
#' @export
assert_is_qr <- function(x)
{                                                         
  assert_engine(x, is_qr, .xname = get_name_in_parent(x))       
}

#' @rdname is_R
#' @export
assert_is_R <- function()
{                                                         
  assert_engine(predicate = is_R)        
}

#' @rdname is_raster
#' @export
assert_is_raster <- function(x)
{                                                         
  assert_engine(x, is_raster, .xname = get_name_in_parent(x))
}

#' @rdname is_raw
#' @export
assert_is_raw <- function(x)
{                                                         
  assert_engine(x, is_raw, .xname = get_name_in_parent(x))
}

#' @rdname is_real
#' @export
assert_all_are_real <- function(x)
{                                                     
  msg <- sprintf("%s are not all real.", get_name_in_parent(x))
  assert_engine(x, is_real, msg)        
}

#' @rdname is_real
#' @export
assert_any_are_real <- function(x)
{                                                     
  msg <- sprintf("%s are all not real.", get_name_in_parent(x))
  assert_engine(x, is_real, msg, what = "any")        
}

#' @rdname is_atomic
#' @export
assert_is_recursive <- function(x)
{                                                         
  assert_engine(x, is_recursive, .xname = get_name_in_parent(x)) 
}

#' @rdname is_empty
#' @export
assert_is_scalar <- function(x)
{                                                     
  assert_engine(x, is_scalar, .xname = get_name_in_parent(x))    
}

#' @rdname is_function
#' @export
assert_is_stepfun <- function(x)
{                                                         
  assert_engine(x, is_stepfun, .xname = get_name_in_parent(x))
}

#' @rdname is_character
#' @export
assert_all_strings_are_missing_or_empty <- function(x)
{                                                       
  msg <- sprintf("%s are not all missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_missing_or_empty_character, msg)
}

#' @rdname is_character
#' @export
assert_any_strings_are_missing_or_empty <- function(x)
{                                                      
  msg <- sprintf("%s are all not missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_missing_or_empty_character, msg, what = "any")
}
#' @rdname is_language
#' @export
assert_is_symbol <- assert_is_name

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

#' @rdname is_table
#' @export
assert_is_table <- function(x)
{                                                         
  assert_engine(x, is_table, .xname = get_name_in_parent(x))       
}

#' @rdname is_ts
#' @export
assert_is_ts <- function(x)
{                                                         
  assert_engine(x, is_ts, .xname = get_name_in_parent(x))       
}

#' @rdname is_ts
#' @export
assert_is_tskernel <- function(x)
{                                                         
  assert_engine(x, is_tskernel, .xname = get_name_in_parent(x))       
}

#' @rdname is_true
#' @export
assert_is_true <- function(x, allow_attributes = FALSE)
{                                                  
 assert_engine(
   x, 
   assertive::is_true,  #avoid conflict with testthat::is_true
   allow_attributes = allow_attributes, 
   .xname = get_name_in_parent(x)
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

#' @rdname is_valid_variable_name
#' @export
assert_all_are_valid_variable_names <- function(x, allow_reserved = TRUE, allow_duplicates = TRUE)
{                                                       
  msg <- sprintf("%s are not all valid variable names.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_valid_variable_name, 
    msg,
    allow_reserved = allow_reserved,
    allow_duplicates = allow_duplicates
  )
}

#' @rdname is_valid_variable_name
#' @export
assert_any_are_valid_variable_names <- function(x, allow_reserved = TRUE, allow_duplicates = TRUE)
{                                                      
  msg <- sprintf("%s are all not valid variable names.", get_name_in_parent(x))
  assert_engine(
    x, 
    is_valid_variable_name, 
    msg,
    what = "any",
    allow_reserved = allow_reserved,
    allow_duplicates = allow_duplicates
  )
}

#' @rdname is_atomic
#' @export
assert_is_vector <- function(x)
{                                                    
  assert_engine(x, is_vector, .xname = get_name_in_parent(x))
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
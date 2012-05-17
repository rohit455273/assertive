#' @rdname is_logical
#' @export
assert_is_a_bool <- function(x)
{      
  msg <- sprintf("%s is not a bool.", get_name_in_parent(x))
  assert_engine(x, is_a_bool, msg)    
}

#' @rdname is_numeric
#' @export
assert_is_a_number <- function(x)
{                                                          
  msg <- sprintf("%s is not a number.", get_name_in_parent(x))
  assert_engine(x, is_a_number, msg)        
}

#' @rdname is_raw
#' @export
assert_is_a_raw <- function(x)
{                                                          
  msg <- sprintf("%s is not a raw.", get_name_in_parent(x))
  assert_engine(x, is_a_raw, msg)        
}

#' @rdname is_character
#' @export
assert_is_a_string <- function(x)
{                                                         
  msg <- sprintf("%s is not a string.", get_name_in_parent(x))
  assert_engine(x, is_a_string, msg)        
}

#' @rdname is_integer
#' @export
assert_is_an_integer <- function(x)
{
  msg <- sprintf("%s is not an integer.", get_name_in_parent(x))
  assert_engine(x, is_an_integer, msg)    
}

#' @rdname is_array
#' @export
assert_is_array <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'array'.", get_name_in_parent(x))
  assert_engine(x, is_array, msg)        
}

#' @rdname is_atomic
#' @export
assert_is_atomic <- function(x)
{                                                         
  msg <- sprintf("%s is not atomic.", get_name_in_parent(x))
  assert_engine(x, is_atomic, msg)        
}

#' @rdname is_call
#' @export
assert_is_call <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'call'.", get_name_in_parent(x))
  assert_engine(x, is_call, msg)        
}

#' @rdname is_character
#' @export
assert_is_character <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'character'.", get_name_in_parent(x))
  assert_engine(x, is_character, msg)        
}

#' @rdname is_data.frame
#' @export
assert_is_data.frame <- function(x)
{                                                         
  msg <- sprintf("%s is not a data.frame.", get_name_in_parent(x))
  assert_engine(x, is_data.frame, msg)        
}

#' @rdname is_empty
#' @export
assert_is_empty <- function(x)
{                                                  
  msg <- sprintf("%s is not empty.", get_name_in_parent(x))
  assert_engine(x, is_empty, msg)        
}

#' @rdname is_empty_model
#' @export
assert_is_empty_model <- function(x)
{                                                     
  msg <- sprintf("%s is not scalar.", get_name_in_parent(x))
  assert_engine(x, is_empty_model, msg)        
}

#' @rdname is_character
#' @export
assert_is_empty_string <- function(x)
{                                                  
  msg <- sprintf("%s is not the empty string.", get_name_in_parent(x))
  assert_engine(x, is_empty_string, msg)        
}

#' @rdname is_environment
#' @export
assert_is_environment <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'environment'.", get_name_in_parent(x))
  assert_engine(x, is_environment, msg)        
}

#' @rdname is_expression
#' @export
assert_is_expression <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'expression'.", get_name_in_parent(x))
  assert_engine(x, is_expression, msg)        
}

#' @rdname is_factor
#' @export
assert_is_factor <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'factor'.", get_name_in_parent(x))
  assert_engine(x, is_factor, msg)        
}

#' @rdname is_function
#' @export
assert_is_function <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'function'.", get_name_in_parent(x))
  assert_engine(x, is_function, msg)        
}

#' @rdname is_true
#' @export
assert_is_false <- function(x)
{                                                  
  msg <- sprintf("%s is not identical to FALSE.", get_name_in_parent(x))
  assert_engine(x, is_false, msg)        
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
assert_all_are_in_range <- function(x, lower = -Inf, upper = Inf)
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
assert_any_are_in_range <- function(x, lower = -Inf, upper = Inf)
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
  msg <- sprintf("%s is not of type 'integer'.", get_name_in_parent(x))
  assert_engine(x, is_integer, msg)        
}

#' @rdname is_logical
#' @export
assert_is_logical <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'logical'.", get_name_in_parent(x))
  assert_engine(x, is_logical, msg)        
}

#' @rdname is_array
#' @export
assert_is_matrix <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'matrix'.", get_name_in_parent(x))
  assert_engine(x, is_matrix, msg)        
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

#' @rdname is_character
#' @export
assert_is_non_empty_string <- function(x)
{                                                     
  msg <- sprintf("%s is not a non-empty string.", get_name_in_parent(x))
  assert_engine(x, is_non_empty_string, msg)        
}  
  
#' @rdname is_empty_model
#' @export
assert_is_non_empty_model <- function(x)
{                                                     
  msg <- sprintf("%s is not scalar.", get_name_in_parent(x))
  assert_engine(x, is_non_empty_model, msg)        
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

#' @rdname is_not_nan
#' @export
assert_all_are_not_nan <- function(x)
{                                                      
  msg <- sprintf("%s contains NaNs.", get_name_in_parent(x))
  assert_engine(x, is_not_nan, msg)
}

#' @rdname is_not_nan
#' @export
assert_any_are_not_nan <- function(x)
{                                                      
  msg <- sprintf("%s are all NaN.", get_name_in_parent(x))
  assert_engine(x, is_not_nan, msg, what = "any")
}

#' @rdname is_numeric
#' @export
assert_is_numeric <- function(x)
{                                                         
  msg <- sprintf("%s is not of type 'numeric'.", get_name_in_parent(x))
  assert_engine(x, is_numeric, msg)        
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
assert_any_are_percentages <- function(x)
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
  msg <- sprintf("%s is not of type 'primitive'.", get_name_in_parent(x))
  assert_engine(x, is_primitive, msg)        
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
assert_any_are_proportions <- function(x)
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

#' @rdname is_R
#' @export
assert_is_R <- function()
{                                                         
  msg <- sprintf("You are not running R.", get_name_in_parent(x))
  assert_engine(prediate = is_R, msg = msg)        
}

#' @rdname is_raw
#' @export
assert_is_raw <- function()
{                                                         
  msg <- sprintf("%s is not of type 'raw'.", get_name_in_parent(x))
  assert_engine(x, is_raw, msg)        
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
  msg <- sprintf("%s is not recursive.", get_name_in_parent(x))
  assert_engine(x, is_recursive, msg)        
}

#' @rdname is_empty
#' @export
assert_is_scalar <- function(x)
{                                                     
  msg <- sprintf("%s is not scalar.", get_name_in_parent(x))
  assert_engine(x, is_scalar, msg)        
}

#' @rdname is_character
#' @export
assert_all_strings_are_missing_or_empty <- function(x)
{                                                       
  msg <- sprintf("%s are not all missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_string_missing_or_empty, msg)
}

#' @rdname is_character
#' @export
assert_any_strings_are_missing_or_empty <- function(x)
{                                                      
  msg <- sprintf("%s are all not missing or empty strings.", get_name_in_parent(x))
  assert_engine(x, is_string_missing_or_empty, msg, what = "any")
}

#' @rdname is_true
#' @export
assert_is_true <- function(x)
{                                                  
  msg <- sprintf("%s is not identical to TRUE.", get_name_in_parent(x))
  assert_engine(x, is_true, msg)        
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
  msg <- sprintf("%s is not a vector.", get_name_in_parent(x))
  assert_engine(x, is_vector, msg)        
}

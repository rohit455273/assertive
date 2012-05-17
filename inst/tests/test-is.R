test_is_a_bool_true_returns_true <- function()
{
  checkTrue(is_a_bool(TRUE))
}

test_is_a_bool_false_returns_true <- function()
{
  checkTrue(is_a_bool(FALSE))
}

test_is_a_bool_na_returns_false <- function()
{
  checkTrue(!is_a_bool(NA))
}
                               
test_is_a_bool_na_with_na_allowed_returns_true <- function()
{
  checkTrue(is_a_bool(NA, TRUE))
}

test_is_a_bool_a_vector_returns_false <- function()
{
  checkTrue(!is_a_bool(c(TRUE, FALSE)))
}

test_is_a_bool_empty_logical_returns_false <- function()
{
  checkTrue(!is_a_bool(logical()))
}


test_is_an_integer_1_returns_true <- function()
{
  checkTrue(is_an_integer(1L))
}

test_is_an_integer_minus_1_returns_true <- function()
{
  checkTrue(is_an_integer(-1L))
}

test_is_an_integer_na_returns_false <- function()
{
  checkTrue(!is_an_integer(NA_integer_))
}
                               
test_is_an_integer_na_with_na_allowed_returns_true <- function()
{
  checkTrue(is_an_integer(NA_integer_, TRUE))
}

test_is_an_integer_a_vector_returns_false <- function()
{
  checkTrue(!is_an_integer(1L:2L))
}

test_is_an_integer_empty_integer_returns_false <- function()
{
  checkTrue(!is_an_integer(integer()))
}


test_is_a_number_1_returns_true <- function()
{
  checkTrue(is_a_number(1))
}

test_is_a_number_minus_1_returns_true <- function()
{
  checkTrue(is_a_number(-1))
}
    
test_is_a_number_nan_returns_false <- function()
{
  checkTrue(!is_a_number(NaN))
}
                               
test_is_a_number_nan_with_nan_allowed_returns_true <- function()
{
  checkTrue(is_a_number(NaN, TRUE))
}
                                 
test_is_a_number_nan_with_nan_allowed_but_not_na_returns_false <- function()
{
  checkTrue(!is_a_number(NaN, TRUE, FALSE))
}

test_is_a_number_na_returns_false <- function()
{
  checkTrue(!is_a_number(NA_real_))
}
                               
test_is_a_number_na_with_na_allowed_returns_true <- function()
{
  checkTrue(is_a_number(NA_real_, TRUE))
}
    
test_is_a_number_a_vector_returns_false <- function()
{
  checkTrue(!is_a_number(1:2))
}

test_is_a_number_empty_numeric_returns_false <- function()
{
  checkTrue(!is_a_number(numeric()))
}



test_is_a_string_foo_returns_true <- function()
{
  checkTrue(is_a_string("foo"))
}

test_is_a_string_na_returns_false <- function()
{
  checkTrue(!is_a_string(NA_character_))
}
                               
test_is_a_string_na_with_na_allowed_returns_true <- function()
{
  checkTrue(is_a_string(NA_character_, TRUE))
}
       
test_is_a_string_empty_returns_false <- function()
{
  checkTrue(!is_a_string(""))
}               
                  
test_is_a_string_empty_with_empty_allowed_returns_false <- function()
{
  checkTrue(!is_a_string("", TRUE))
}               
    
test_is_a_string_a_vector_returns_false <- function()
{
  checkTrue(!is_a_string(c("foo", "bar")))
}

test_is_a_string_empty_character_returns_false <- function()
{
  checkTrue(!is_a_string(character()))
}


test_is_empty_empty_vector_returns_true <- function()
{
  checkTrue(is_empty(numeric()))
}

test_is_empty_empty_list_returns_true <- function()
{
  checkTrue(is_empty(list()))
}
           
test_is_empty_null_returns_true <- function()
{
  checkTrue(is_empty(NULL))
}

test_is_empty_non_empty_vector_returns_false <- function()
{
  checkTrue(!is_empty(1))
}


test_is_in_range_1_in_0_to_2_returns_true <- function()
{
  checkTrue(is_in_range(1, 0, 2))
}

test_is_in_range_0_in_0_to_2_returns_true <- function()
{
  checkTrue(is_in_range(0, 0, 2))
}
        
test_is_in_range_0_in_0_to_2_with_strict_lower_bound_returns_false <- function()
{
  checkTrue(!is_in_range(0, 0, 2, lower.is.strict = TRUE))
}
        
test_is_in_range_2_in_0_to_2_with_strict_upper_bound_returns_false <- function()
{
  checkTrue(!is_in_range(2, 0, 2, upper.is.strict = TRUE))
}


test_is_negative_all_minus_1_to_minus_10_returns_true <- function()
{
  checkTrue(all(is_negative(-1:-10)))
}
 
test_is_negative_all_1_to_10_returns_false <- function()
{
  checkTrue(!all(is_negative(1:10)))
}

     
test_is_non_negative_all_0_to_10_returns_true <- function()
{
  checkTrue(all(is_non_negative(0:10)))
}
 
test_is_non_negative_all_minus_1_to_minus_10_returns_false <- function()
{
  checkTrue(!all(is_non_negative(-1:-10)))
}
      
      
test_is_non_positive_all_0_to_minus_10_returns_true <- function()
{
  checkTrue(all(is_non_positive(0:-10)))
}
 
test_is_non_positive_all_1_to_10_returns_false <- function()
{
  checkTrue(!all(is_non_positive(1:10)))
}


test_is_percentage_all_0_to_100_returns_true <- function()
{
  checkTrue(all(is_percentage(0:100)))
}
 
test_is_percentage_0_with_strict_lower_bound_returns_false <- function()
{
  checkTrue(!is_percentage(0, lower.is.strict = TRUE))
}
                  
test_is_percentage_100_with_strict_upper_bound_returns_false <- function()
{
  checkTrue(!is_percentage(100, upper.is.strict = TRUE))
}

     
test_is_proportion_all_0_to_1_returns_true <- function()
{
  checkTrue(all(is_proportion(seq.int(0, 1, 0.1))))
}
 
test_is_proportion_0_with_strict_lower_bound_returns_false <- function()
{
  checkTrue(!is_proportion(0, lower.is.strict = TRUE))
}
                  
test_is_proportion_1_with_strict_upper_bound_returns_false <- function()
{
  checkTrue(!is_proportion(1, upper.is.strict = TRUE))
}


test_is_scalar_a_scalar_returns_true <- function()
{
  checkTrue(is_scalar(1))
} 
   
test_is_scalar_a_vector_returns_false <- function()
{
  checkTrue(!is_scalar(1:2))
} 

test_is_scalar_empty_returns_false <- function()
{
  checkTrue(!is_scalar(numeric()))
} 

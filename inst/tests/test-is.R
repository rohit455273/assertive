test.is_a_bool.true.returns_true <- function()
{
  checkTrue(is_a_bool(TRUE))
}

test.is_a_bool.false.returns_true <- function()
{
  checkTrue(is_a_bool(FALSE))
}

test.is_a_bool.na.returns_true <- function()
{
  checkTrue(is_a_bool(NA))
}
           
test.is_a_bool.a_vector.returns_false <- function()
{
  checkTrue(!is_a_bool(c(TRUE, FALSE)))
}

test.is_a_bool.empty_logical.returns_false <- function()
{
  checkTrue(!is_a_bool(logical()))
}


test.is_an_integer.1.returns_true <- function()
{
  checkTrue(is_an_integer(1L))
}

test.is_an_integer.minus_1.returns_true <- function()
{
  checkTrue(is_an_integer(-1L))
}

test.is_an_integer.na.returns_false <- function()
{
  checkTrue(!is_an_integer(NA_integer_))
}
                               
test.is_an_integer.na_with_na_allowed.returns_true <- function()
{
  checkTrue(is_an_integer(NA_integer_, TRUE))
}

test.is_an_integer.a_vector.returns_false <- function()
{
  checkTrue(!is_an_integer(1L:2L))
}

test.is_an_integer.empty_integer.returns_false <- function()
{
  checkTrue(!is_an_integer(integer()))
}


test.is_a_number.1.returns_true <- function()
{
  checkTrue(is_a_number(1))
}

test.is_a_number.minus_1.returns_true <- function()
{
  checkTrue(is_a_number(-1))
}
    
test.is_a_number.nan.returns_false <- function()
{
  checkTrue(!is_a_number(NaN))
}
                               
test.is_a_number.nan_with_nan_allowed.returns_true <- function()
{
  checkTrue(is_a_number(NaN, TRUE))
}
                                 
test.is_a_number.nan_with_nan_allowed_but_not_na.returns_false <- function()
{
  checkTrue(!is_a_number(NaN, TRUE, FALSE))
}

test.is_a_number.na.returns_false <- function()
{
  checkTrue(!is_a_number(NA_real_))
}
                               
test.is_a_number.na_with_na_allowed.returns_true <- function()
{
  checkTrue(is_a_number(NA_real_, TRUE))
}
    
test.is_a_number.a_vector.returns_false <- function()
{
  checkTrue(!is_a_number(1:2))
}

test.is_a_number.empty_numeric.returns_false <- function()
{
  checkTrue(!is_a_number(numeric()))
}



test.is_a_string.foo.returns_true <- function()
{
  checkTrue(is_a_string("foo"))
}

test.is_a_string.na.returns_false <- function()
{
  checkTrue(!is_a_string(NA_character_))
}
                               
test.is_a_string.na_with_na_allowed.returns_true <- function()
{
  checkTrue(is_a_string(NA_character_, TRUE))
}
       
test.is_a_string.empty.returns_false <- function()
{
  checkTrue(!is_a_string(""))
}               
                  
test.is_a_string.empty_with_empty_allowed.returns_false <- function()
{
  checkTrue(!is_a_string("", TRUE))
}               
    
test.is_a_string.a_vector.returns_false <- function()
{
  checkTrue(!is_a_string(c("foo", "bar")))
}

test.is_a_string.empty_character.returns_false <- function()
{
  checkTrue(!is_a_string(character()))
}


test.is_empty.empty_vector.returns_true <- function()
{
  checkTrue(is_empty(numeric()))
}

test.is_empty.empty_list.returns_true <- function()
{
  checkTrue(is_empty(list()))
}
           
test.is_empty.null.returns_true <- function()
{
  checkTrue(is_empty(NULL))
}

test.is_empty.non_empty_vector.returns_false <- function()
{
  checkTrue(!is_empty(1))
}


test.is_in_range.1_in_0_to_2.returns_true <- function()
{
  checkTrue(is_in_range(1, 0, 2))
}

test.is_in_range.0_in_0_to_2.returns_true <- function()
{
  checkTrue(is_in_range(0, 0, 2))
}
        
test.is_in_range.0_in_0_to_2_with_strict_lower_bound.returns_false <- function()
{
  checkTrue(!is_in_range(0, 0, 2, lower.is.strict = TRUE))
}
        
test.is_in_range.2_in_0_to_2_with_strict_upper_bound.returns_false <- function()
{
  checkTrue(!is_in_range(2, 0, 2, upper.is.strict = TRUE))
}


test.is_negative.all_minus_1_to_minus_10.returns_true <- function()
{
  checkTrue(all(is_negative(-1:-10)))
}
 
test.is_negative.all_1_to_10.returns_false <- function()
{
  checkTrue(!all(is_negative(1:10)))
}

     
test.is_non_negative.all_0_to_10.returns_true <- function()
{
  checkTrue(all(is_non_negative(0:10)))
}
 
test.is_non_negative.all_minus_1_to_minus_10.returns_false <- function()
{
  checkTrue(!all(is_non_negative(-1:-10)))
}
      
      
test.is_non_positive.all_0_to_minus_10.returns_true <- function()
{
  checkTrue(all(is_non_positive(0:-10)))
}
 
test.is_non_positive.all_1_to_10.returns_false <- function()
{
  checkTrue(!all(is_non_positive(1:10)))
}


test.is_percentage.all_0_to_100.returns_true <- function()
{
  checkTrue(all(is_percentage(0:100)))
}
 
test.is_percentage.0_with_strict_lower_bound.returns_false <- function()
{
  checkTrue(!is_percentage(0, lower.is.strict = TRUE))
}
                  
test.is_percentage.100_with_strict_upper_bound.returns_false <- function()
{
  checkTrue(!is_percentage(100, upper.is.strict = TRUE))
}

     
test.is_proportion.all_0_to_1.returns_true <- function()
{
  checkTrue(all(is_proportion(seq.int(0, 1, 0.1))))
}
 
test.is_proportion.0_with_strict_lower_bound.returns_false <- function()
{
  checkTrue(!is_proportion(0, lower.is.strict = TRUE))
}
                  
test.is_proportion.1_with_strict_upper_bound.returns_false <- function()
{
  checkTrue(!is_proportion(1, upper.is.strict = TRUE))
}


test.is_scalar.a_scalar.returns_true <- function()
{
  checkTrue(is_scalar(1))
} 
   
test.is_scalar.a_vector.returns_false <- function()
{
  checkTrue(!is_scalar(1:2))
} 

test.is_scalar.empty.returns_false <- function()
{
  checkTrue(!is_scalar(numeric()))
} 

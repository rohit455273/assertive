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


test.is_empty_model.an_empty_model.returns_true <- function()
{
  checkTrue(is_empty_model(lm(y ~ 0, data.frame(y = 1:5))))
}

test.is_empty_model.a_model_with_intercept.returns_false <- function()
{
  checkTrue(!is_empty_model(lm(y ~ 1, data.frame(y = 1:5))))
}

test.is_empty_model.a_model_with_factors.returns_false <- function()
{
  checkTrue(!is_empty_model(lm(y ~ x + 0, data.frame(y = 1:5, x = 1:5))))
}

test.is_empty_model.not_a_model.returns_false <- function()
{
  checkTrue(!is_empty_model(1:10))
}


test.is_false.false.returns_true <- function()
{
  checkTrue(is_false(FALSE))
} 

test.is_false.false_vector.returns_false <- function()
{
  checkTrue(!is_false(logical(2)))
} 

test.is_false.NA.returns_false <- function()
{
  checkTrue(!is_false(NA))
} 

test.is_false.true_with_attr.returns_allow_attributes <- function()
{
  x <- false("This has an attribute.")
  checkTrue(!is_false(x))
  checkTrue(is_false(x, allow_attributes = TRUE))
} 


test.is_imaginary.imaginary_numbers.returns_true_when_purely_imaginary <- function()
{
  x <- c(1i, 1 + 0i, 0i, 1 + 1i, Inf, NA_complex_)
  checkEquals(
    c(TRUE, FALSE, TRUE, FALSE, FALSE, NA),
    is_imaginary(x)
  )
} 

test.is_imaginary.real_numbers.returns_true_when_0 <- function()
{
  x <- c(1, 0, -1, Inf, NA_real_)
  checkEquals(
    c(FALSE, TRUE, FALSE, FALSE, NA),
    is_imaginary(x)
  )
} 


test.is_non_empty.non_empty_vector.returns_true <- function()
{
  checkTrue(is_non_empty(1))
}

test.is_non_empty.empty_vector.returns_false <- function()
{
  checkTrue(!is_non_empty(numeric()))
}

test.is_non_empty.empty_list.returns_false <- function()
{
  checkTrue(!is_non_empty(list()))
}

test.is_non_empty.null.returns_false <- function()
{
  checkTrue(!is_non_empty(NULL))
}


test.is_non_empty_model.a_model_with_intercept.returns_true <- function()
{
  checkTrue(is_non_empty_model(lm(y ~ 1, data.frame(y = 1:5))))
}

test.is_non_empty_model.a_model_with_factors.returns_true <- function()
{
  checkTrue(is_non_empty_model(lm(y ~ x + 0, data.frame(y = 1:5, x = 1:5))))
}

test.is_non_empty_model.an_empty_model.returns_false <- function()
{
  checkTrue(!is_non_empty_model(lm(y ~ 0, data.frame(y = 1:5))))
}

test.is_non_empty_model.not_a_model.returns_false <- function()
{
  checkTrue(!is_non_empty_model(1:10))
}


test.is_not_na.not_na.returns_true <- function()
{
  checkTrue(is_not_na(1))
} 

test.is_not_na.na.returns_false <- function()
{
  checkTrue(!is_not_na(NA))
} 


test.is_not_nan.not_na.returns_true <- function()
{
  checkTrue(is_not_nan(1))
} 

test.is_not_nan.nan.returns_false <- function()
{
  checkTrue(!is_not_nan(NaN))
} 


test.is_null.null.returns_true <- function()
{
  checkTrue(is_null(NULL))
} 

test.is_null.na.returns_false <- function()
{
  checkTrue(!is_null(NA))
} 

test.is_null.nan.returns_false <- function()
{
  checkTrue(!is_null(NaN))
} 


test.is_numeric_string.a_character_vector.returns_true_when_string_contains_a_number <- function()
{
  x <- c("1", "-2.3e4", "Inf", "one", "NA")
  checkEquals(
    c(TRUE, TRUE, TRUE, FALSE, FALSE),
    is_numeric_string(x)
  )
} 


test.is_real.imaginary_numbers.returns_true_when_purely_real <- function()
{
  x <- c(1i, 1 + 0i, 0i, 1 + 1i, Inf, NA_complex_)
  checkEquals(
    c(FALSE, TRUE, TRUE, FALSE, TRUE, NA),
    is_real(x)
  )
} 

test.is_real.real_numbers.returns_true_always <- function()
{
  x <- c(1, 0, -1, Inf, NA_real_)
  checkEquals(
    rep.int(TRUE, 5),
    is_real(x)
  )
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


test.is_missing_or_empty_character.a_scalar.returns_logical <- function()
{
  x <- c("foo", "", NA_character_, " ")
  checkEquals(
    c(FALSE, TRUE, TRUE, FALSE),
    is_missing_or_empty_character(x)
  )
} 


test.is_not_missing_nor_empty_character.a_scalar.returns_logical <- function()
{
  x <- c("foo", "", NA_character_, " ")
  checkEquals(
    c(TRUE, FALSE, FALSE, TRUE),
    is_not_missing_nor_empty_character(x)
  )
} 


test.is_true.true.returns_true <- function()
{
  checkTrue(is_true(TRUE))
} 

test.is_true.true_vector.returns_false <- function()
{
  checkTrue(!is_true(rep.int(TRUE, 2)))
} 

test.is_true.NA.returns_false <- function()
{
  checkTrue(!is_true(NA))
}

test.is_true.true_with_attr.returns_allow_attributes <- function()
{
  x <- c(truth = TRUE)
  checkTrue(!is_true(x))
  checkTrue(is_true(x, allow_attributes = TRUE))
} 


test.is_whole_number.NA.returns_false <- function()
{
  x <- c(1, -1.5, 1 + .Machine$double.eps, Inf, NA)
  checkEquals(
    c(TRUE, FALSE, FALSE, NA, NA),
    is_whole_number(x)
  )
}

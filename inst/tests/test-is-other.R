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


test.is_imaginary.imaginary_numbers.returns_true_when_purely_imaginary <- function()
{
  x <- c(1i, 0i, i + 1i, Inf, NA_complex_)
  checkEquals(
    c(TRUE, TRUE, FALSE, FALSE, NA)
    is_imaginary(X)
  )
} 

test.is_imaginary.real_numbers.returns_true_when_0 <- function()
{
  x <- c(1, 0, -1, Inf, NA_real_)
  checkEquals(
    c(FALSE, TRUE, FALSE, FALSE, NA)
    is_imaginary(X)
  )
} 


test.is_non_empty.non_empty_vector.returns_true <- function()
{
  checkTrue(is_empty(1))
}

test.is_non_empty.empty_vector.returns_false <- function()
{
  checkTrue(!is_empty(numeric()))
}

test.is_non_empty.empty_list.returns_false <- function()
{
  checkTrue(!is_empty(list()))
}

test.is_non_empty.null.returns_false <- function()
{
  checkTrue(!is_empty(NULL))
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

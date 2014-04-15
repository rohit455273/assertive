test.is_debugged.a_function.returns_true_when_debugged <- function()
{
  x <- function() {}
  checkTrue(!is_debugged(x))
  debug(x)
  checkTrue(is_debugged(x))
  undebug(x)
  checkTrue(!is_debugged(x))  
}

test.is_existing.some_variables.returns_true_when_they_exist <- function()
{
  a_variable <- 1
  x <- c("a_variable", "not_a_variable")
  expected <- c(TRUE, FALSE)
  names(expected) <- x
  this_env <- sys.frame(sys.nframe())
  checkEquals(
    expected,
    is_existing(x, envir = this_env, inherits = FALSE)
  )
}

test.is_symmetric_matrix.a_symmetric_matrix.returns_true <- function()
{
  x <- diag(3)
  checkTrue(is_symmetric_matrix(x))
}

test.is_symmetric_matrix.a_symmetric_matrix.returns_logical <- function()
{
  x <- diag(3); x[3, 1] <- 1e-100
  checkTrue(is_symmetric_matrix(x))
  checkTrue(!is_symmetric_matrix(x, tol = 0))
}

test.is_symmetric_matrix.an_assymmetric_matrix.returns_false <- function()
{
  x <- matrix(rnorm(9), nrow = 3)
  checkTrue(!is_symmetric_matrix(x))
}

test.is_symmetric_matrix.not_coercible_to_matrix.throws_error <- function()
{
  suppressWarnings(checkException(is_symmetric_matrix(sqrt), silent = TRUE))
}


test.is_unsorted.an_unsorted_vector.returns_true <- function()
{
  checkTrue(is_unsorted(c(1, 3, 2)))
} 

test.is_unsorted.an_weakly_unsorted_vector.returns_strictly <- function()
{
  checkTrue(!is_unsorted(c(1, 1, 2)))
  checkTrue(is_unsorted(c(1, 1, 2), strictly = TRUE))
} 

test.is_unsorted.a_sorted_vector.returns_false <- function()
{
  checkTrue(!is_unsorted(1:3))
} 


test.is_whole_number.a_numeric_vector.returns_true_for_whole_numbers <- function()
{
  x <- c(
    0, 1, -0.5, 
    100 * .Machine$double.eps, 101 * .Machine$double.eps, 
    100 * -.Machine$double.eps, -101 * .Machine$double.eps, #Not double.neg.eps
    Inf, -Inf, NaN, NA
  )
  expected <- c(
    TRUE, TRUE, FALSE, 
    TRUE, FALSE, 
    TRUE, FALSE, 
    NA, NA, NA, NA
  )
  names(expected) <- x
  checkEquals(
    expected,
    is_whole_number(x)
  )
}

test.is_whole_number.no_tolerance.returns_true_for_exactly_whole_numbers <- function()
{
  x <- c(
    0, 1, -0.5, 
    100 * .Machine$double.eps, 101 * .Machine$double.eps, 
    100 * -.Machine$double.eps, -101 * .Machine$double.eps, #Not double.neg.eps
    Inf, -Inf, NaN, NA
  )
  expected <- c(
    TRUE, TRUE, FALSE, 
    FALSE, FALSE, 
    FALSE, FALSE, 
    NA, NA, NA, NA
  )
  names(expected) <- x
  checkEquals(
    expected,
    is_whole_number(x, 0)
  )
}

test_that("test.is_debugged.a_function.returns_true_when_debugged", {
  x <- function()
  {
  }
  expect_false(is_debugged(x))
  debug(x)
  expect_true(is_debugged(x))
  undebug(x)
  expect_false(is_debugged(x))
})

test_that("test.is_existing.some_variables.returns_true_when_they_exist", {
  a_variable <- 1
  x <- c("a_variable", "not_a_variable")
  expected <- c(TRUE, FALSE)
  names(expected) <- x
  this_env <- sys.frame(sys.nframe())
  expect_equal(is_existing(x, envir = this_env, inherits = FALSE), expected)
})

test_that("test.is_symmetric_matrix.a_symmetric_matrix.returns_logical", {
  x <- diag(3)
  x[3, 1] <- 1e-100
  expect_true(is_symmetric_matrix(x))
  expect_false(is_symmetric_matrix(x, tol = 0))
})

test_that("test.is_symmetric_matrix.a_symmetric_matrix.returns_true", {
  x <- diag(3)
  expect_true(is_symmetric_matrix(x))
})

test_that("test.is_symmetric_matrix.an_assymmetric_matrix.returns_false", {
  x <- matrix(rnorm(9), nrow = 3)
  expect_false(is_symmetric_matrix(x))
})

test_that("test.is_symmetric_matrix.not_coercible_to_matrix.throws_error", {
  suppressWarnings(checkException(is_symmetric_matrix(sqrt), silent = TRUE))
})

test_that("test.is_unsorted.a_sorted_vector.returns_false", {
  expect_false(is_unsorted(1:3))
})

test_that("test.is_unsorted.an_unsorted_vector.returns_true", {
  expect_true(is_unsorted(c(1, 3, 2)))
})

test_that("test.is_unsorted.an_weakly_unsorted_vector.returns_strictly", {
  expect_false(is_unsorted(c(1, 1, 2)))
  expect_true(is_unsorted(c(1, 1, 2), strictly = TRUE))
})

test_that("test.is_whole_number.a_numeric_vector.returns_true_for_whole_numbers", {
  x <- c(0, 1, -0.5, 100 * .Machine$double.eps, 101 * .Machine$double.eps, 100 * -.Machine$double.eps, -101 * 
    .Machine$double.eps, Inf, -Inf, NaN, NA)
  expected <- c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, NA, NA, NA, NA)
  names(expected) <- x
  expect_equal(is_whole_number(x), expected)
})

test_that("test.is_whole_number.no_tolerance.returns_true_for_exactly_whole_numbers", {
  x <- c(0, 1, -0.5, 100 * .Machine$double.eps, 101 * .Machine$double.eps, 100 * -.Machine$double.eps, -101 * 
    .Machine$double.eps, Inf, -Inf, NaN, NA)
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA, NA, NA, NA)
  names(expected) <- x
  expect_equal(is_whole_number(x, 0), expected)
})
 

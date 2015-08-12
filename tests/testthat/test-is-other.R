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

test_that("test.is_symmetric_matrix.not_coercible_to_matrix.throws_error", 
  {
    suppressWarnings(expect_error(is_symmetric_matrix(sqrt)))
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


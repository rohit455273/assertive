test_that("test.is_false.logical_vector.returns_true_when_false", {
  x <- c(TRUE, FALSE, NA)
  expected <- c(FALSE, TRUE, FALSE)
  names(expected) <- x
  expect_equal(assertive::is_false(x), expected)
})

test_that("test.is_identical_to_false.false.returns_true", {
  expect_true(is_identical_to_false(FALSE))
})

test_that("test.is_identical_to_false.false_vector.returns_false", {
  expect_false(is_identical_to_false(logical(2)))
})

test_that("test.is_identical_to_false.NA.returns_false", {
  expect_false(is_identical_to_false(NA))
})

test_that("test.is_identical_to_false.true_with_attr.returns_allow_attributes", 
  {
    x <- false("This has an attribute.")
    expect_false(is_identical_to_false(x))
    expect_true(is_identical_to_false(x, allow_attributes = TRUE))
  })

test_that("test.is_identical_to_true.NA.returns_false", {
  expect_false(is_identical_to_true(NA))
})

test_that("test.is_identical_to_true.true.returns_true", {
  expect_true(is_identical_to_true(TRUE))
})

test_that("test.is_identical_to_true.true_vector.returns_false", {
  expect_false(is_identical_to_true(rep.int(TRUE, 2)))
})

test_that("test.is_identical_to_true.true_with_attr.returns_allow_attributes", 
  {
    x <- c(truth = TRUE)
    expect_false(is_identical_to_true(x))
    expect_true(is_identical_to_true(x, allow_attributes = TRUE))
  })

test_that("test.is_true.logical_vector.returns_true_when_true", {
  x <- c(TRUE, FALSE, NA)
  expected <- c(TRUE, FALSE, FALSE)
  names(expected) <- x
  expect_equal(assertive::is_true(x), expected)
}) 

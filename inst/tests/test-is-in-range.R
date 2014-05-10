test_that("test.is_in_closed_range.0_to_4_in_1_to_3.returns_true_inside_bounds", 
  {
    x <- 0:4
    expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE)
    names(expected) <- x
    expect_equal(is_in_closed_range(x, 1, 3), expected)
  })

test_that("test.is_in_left_open_range.0_to_4_in_1_to_3.returns_true_inside_bounds", 
  {
    x <- 0:4
    expected <- c(FALSE, FALSE, TRUE, TRUE, FALSE)
    names(expected) <- x
    expect_equal(is_in_left_open_range(x, 1, 3), expected)
  })

test_that("test.is_in_open_range.0_to_4_in_1_to_3.returns_true_inside_bounds", 
  {
    x <- 0:4
    expected <- c(FALSE, FALSE, TRUE, FALSE, FALSE)
    names(expected) <- x
    expect_equal(is_in_open_range(x, 1, 3), expected)
  })

test_that("test.is_in_range.0_to_4_in_1_to_3.returns_true_inside_bounds", {
  x <- 0:4
  expected <- c(FALSE, TRUE, TRUE, TRUE, FALSE)
  names(expected) <- x
  expect_equal(is_in_range(x, 1, 3), expected)
})

test_that("test.is_in_right_open_range.0_to_4_in_1_to_3.returns_true_inside_bounds", 
  {
    x <- 0:4
    expected <- c(FALSE, TRUE, TRUE, FALSE, FALSE)
    names(expected) <- x
    expect_equal(is_in_right_open_range(x, 1, 3), expected)
  })

test_that("test.is_negative.minus_2_to_2.returns_true_when_negative", {
  x <- -2:2
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected) <- x
  expect_equal(is_negative(x), expected)
})

test_that("test.is_non_negative.minus_2_to_2.returns_true_when_non_negative", 
  {
    x <- -2:2
    expected <- c(FALSE, FALSE, TRUE, TRUE, TRUE)
    names(expected) <- x
    expect_equal(is_non_negative(x), expected)
  })

test_that("test.is_non_positive.minus_2_to_2.returns_true_when_non_positive", 
  {
    x <- -2:2
    expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
    names(expected) <- x
    expect_equal(is_non_positive(x), expected)
  })

test_that("test.is_percentage.minus_minus_1_to_101.returns_true_when_percentage", 
  {
    x <- -1:101
    expected <- c(FALSE, rep.int(TRUE, 101), FALSE)
    names(expected) <- x
    expect_equal(is_percentage(x), expected)
  })

test_that("test.is_positive.minus_2_to_2.returns_true_when_positive", {
  x <- -2:2
  expected <- c(FALSE, FALSE, FALSE, TRUE, TRUE)
  names(expected) <- x
  expect_equal(is_positive(x), expected)
})

test_that("test.is_proportion.minus_minus_point_01_to_1_point_01.returns_true_when_percentage", 
  {
    x <- seq.int(-0.01, 1.01, 0.01)
    expected <- c(FALSE, rep.int(TRUE, 101), FALSE)
    names(expected) <- x
    expect_equal(is_proportion(x), expected)
  }) 

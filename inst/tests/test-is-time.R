test_that("test.is_in_future.then_now_soon.returns_true_in_past", {
  x <- Sys.time() + c(-1, 5)
  expected <- c(FALSE, TRUE)
  names(expected) <- x
  expect_equal(is_in_future(x), expected)
})

test_that("test.is_in_past.then_now_soon.returns_true_in_past", {
  x <- Sys.time() + c(-1, 5)
  expected <- c(TRUE, FALSE)
  names(expected) <- x
  expect_equal(is_in_past(x), expected)
}) 

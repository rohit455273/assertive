test_that("test.is_imaginary.imaginary_numbers.returns_true_when_purely_imaginary", 
  {
    x <- c(0 + (0 + (0 + (0+1i))), 1 + (0 + (0 + (0 + (0+0i)))), 0 + (0 + 
      (0 + (0+0i))), 1 + (0 + (0 + (0 + (0+1i)))), Inf, NA_complex_)
    expect_equal(is_imaginary(x), c(TRUE, FALSE, TRUE, FALSE, FALSE, NA))
  })

test_that("test.is_imaginary.real_numbers.returns_true_when_0", {
  x <- c(1, 0, -1, Inf, NA_real_)
  expect_equal(is_imaginary(x), c(FALSE, TRUE, FALSE, FALSE, NA))
})

test_that("test.is_real.imaginary_numbers.returns_true_when_purely_real", {
  x <- c(0 + (0 + (0 + (0+1i))), 1 + (0 + (0 + (0 + (0+0i)))), 0 + (0 + (0 + 
    (0+0i))), 1 + (0 + (0 + (0 + (0+1i)))), Inf, NA_complex_)
  expect_equal(is_real(x), c(FALSE, TRUE, TRUE, FALSE, TRUE, NA))
})

test_that("test.is_real.real_numbers.returns_true_always", {
  x <- c(1, 0, -1, Inf, NA_real_)
  expect_equal(is_real(x), rep.int(TRUE, 5))
}) 

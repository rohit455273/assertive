test_that("test.coerce_to.numeric_vector_to_data_frame.returns_data_frame", 
  {
    x <- 1:5
    expected <- data.frame(x = x)
    expect_equal(suppressWarnings(coerce_to(x, "data.frame")), expected)
    old_ops <- options(warn = 2)
    on.exit(options(old_ops))
    expect_error(coerce_to(x, "data.frame"))
  })

test_that("test.use_first.a_list_double_indexing.returns_contents_of_first_element", 
  {
    x <- as.list(letters)
    expected <- "a"
    expect_identical(expected, suppressWarnings(use_first(x)))
    old_ops <- options(warn = 2)
    on.exit(options(old_ops))
    expect_error(use_first(x))
  })

test_that("test.use_first.a_list_single_indexing.returns_first_element", {
  x <- as.list(letters)
  expected <- list("a")
  expect_identical(expected, suppressWarnings(use_first(x, "[")))
  old_ops <- options(warn = 2)
  on.exit(options(old_ops))
  expect_error(use_first(x, "["))
})

test_that("test.use_first.a_scalar.returns_x", {
  x <- "a"
  expected <- x
  expect_identical(expected, use_first(x))
})

test_that("test.use_first.a_vector_double_indexing.returns_first_element", 
  {
    x <- letters
    expected <- "a"
    expect_identical(expected, suppressWarnings(use_first(x)))
    old_ops <- options(warn = 2)
    on.exit(options(old_ops))
    expect_error(use_first(x))
  })

test_that("test.use_first.a_vector_single_indexing.returns_first_element", 
  {
    x <- letters
    expected <- "a"
    expect_identical(expected, suppressWarnings(use_first(x, "[")))
    old_ops <- options(warn = 2)
    on.exit(options(old_ops))
    expect_error(use_first(x, "["))
  })

test_that("test.use_first.empty.throws_error", {
  x <- NULL
  expect_error(use_first(x))
}) 

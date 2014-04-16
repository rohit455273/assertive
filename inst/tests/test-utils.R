test.coerce_to.numeric_vector_to_data_frame.returns_data_frame <- function()
{
  x <- 1:5
  expected <- data.frame(x = x)
  checkEquals(expected, coerce_to(x, "data.frame"))
}

test.use_first.a_vector_double_indexing.returns_first_element <- function()
{
  x <- letters
  expected <- "a"
  checkIdentical(suppressWarnings(use_first(x)), expected)
  old_ops <- options(warn = 2)
  on.exit(options(old_ops))
  checkException(use_first(x))
}

test.use_first.a_vector_single_indexing.returns_first_element <- function()
{
  x <- letters
  expected <- "a"
  checkIdentical(suppressWarnings(use_first(x, "[")), expected)
  old_ops <- options(warn = 2)
  on.exit(options(old_ops))
  checkException(use_first(x, "["), silent = TRUE)  
}

test.use_first.a_list_double_indexing.returns_contents_of_first_element <- function()
{
  x <- as.list(letters)
  expected <- "a"
  checkIdentical(suppressWarnings(use_first(x)), expected)
  old_ops <- options(warn = 2)
  on.exit(options(old_ops))
  checkException(use_first(x), silent = TRUE)  
}

test.use_first.a_list_single_indexing.returns_first_element <- function()
{
  x <- as.list(letters)
  expected <- list("a")
  checkIdentical(suppressWarnings(use_first(x, "[")), expected)
  old_ops <- options(warn = 2)
  on.exit(options(old_ops))
  checkException(use_first(x, "["), silent = TRUE)  
}

test.use_first.a_scalar.returns_x <- function()
{
  x <- "a"
  expected <- x
  checkIdentical(use_first(x), expected)
}

test.use_first.empty.throws_error <- function()
{
  x <- NULL
  checkException(use_first(x), silent = TRUE)
}

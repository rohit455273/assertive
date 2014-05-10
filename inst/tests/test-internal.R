test_that("test.character_to_list_of_integer_vectors.strings.returns_list_of_integer_vectors", 
{
  x <- c("12345", "1b3d5", "abcde", NA, "", " ", " 2 4 ")
  expected <- list(1:5, c(1L, NA, 3L, NA, 5L), rep.int(NA_integer_, 5L), 
    NA_integer_, integer(), NA_integer_, c(NA, 2L, NA, 4L, NA))
  names(expected) <- x
  expect_identical(suppressWarnings(character_to_list_of_integer_vectors(x)), 
    expected)
  old_ops <- options(warn = 2)
  on.exit(options(old_ops))
  expect_error(character_to_list_of_integer_vectors(x))
})

test_that("test.matches_regex.strings.returns_true_when_string_matches_regex", 
{
  rx <- "foo"
  x <- c("foo", "fooo", "fo", "", "FOO", NA)
  expected <- c(TRUE, TRUE, FALSE, FALSE, TRUE, NA)
  names(expected) <- x
  expect_equal(matches_regex(x, rx), expected)
}) 

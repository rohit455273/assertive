test.character_to_list_of_integer_vectors.strings.returns_list_of_integer_vectors <- function()
{
  x <- c("12345", "1b3d5", "abcde", NA, "", " ", " 2 4 ") 
  expected <- list(
    1:5,
    c(1L, NA, 3L, NA, 5L),
    rep.int(NA_integer_, 5L),
    NA_integer_,
    integer(),
    NA_integer_,
    c(NA, 2L, NA, 4L, NA)
  )
  names(expected) <- x
  checkIdentical(
    expected,
    suppressWarnings(character_to_list_of_integer_vectors(x))
  )
  old_ops <- options(warn = 2)
  on.exit(options(old_ops))
  checkException(character_to_list_of_integer_vectors(x), silent = TRUE)
}


test.matches_regex.strings.returns_true_when_string_matches_regex <- function()
{
  rx <- "foo"
  x <- c("foo", "fooo", "fo", "", "FOO", NA)  
  expected <- c(TRUE, TRUE, FALSE, FALSE, TRUE, NA)
  names(expected) <- x
  checkEquals(
    expected,
    matches_regex(x, rx)
  )
}

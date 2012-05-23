test.is_numeric_string.a_character_vector.returns_true_when_string_contains_a_number <- function()
{
  x <- c("1", "-2.3e4", "Inf", "one", "NA")
  checkEquals(
    c(TRUE, TRUE, TRUE, FALSE, FALSE),
    is_numeric_string(x)
    )
} 


test.is_missing_or_empty_character.a_scalar.returns_logical <- function()
{
  x <- c("foo", "", NA_character_, " ")
  checkEquals(
    c(FALSE, TRUE, TRUE, FALSE),
    is_missing_or_empty_character(x)
    )
} 


test.is_not_missing_nor_empty_character.a_scalar.returns_logical <- function()
{
  x <- c("foo", "", NA_character_, " ")
  checkEquals(
    c(TRUE, FALSE, FALSE, TRUE),
    is_not_missing_nor_empty_character(x)
    )
} 

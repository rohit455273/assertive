test.is_cas_number.a_character_vector.returns_true_when_string_contains_a_cas_number <- function()
{
  x <- c(
    water = "7732-18-5", 
    d_glucose = "50-99-7",
    l_glucose = "921-60-8",
    no_hyphens = "7732185", 
    two_check_digits = "7732-18-55",
    bad_check_digit = "7732-18-4"
  )
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_cas_number(x)
  )
} 


# test.is_credit_card_number.a_character_vector.returns_true_when_string_contains_a_credit_card_number <- function()
# {
#   x <- TODO!!!
#   expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
#   names(expected) <- x
#   checkEquals(
#     expected,
#     is_credit_card_number(x)
#   )
# }


test.is_date_string.a_character_vector.returns_true_when_string_contains_a_date <- function()
{
  x <- c("1999-12-31 23:59:59", "1979-08-01 01:00:00", "31 Dec 1999 11:59:59PM", "not a date", "NA")
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_date_string(x)
  )
} 


test.is_date_string.a_character_vector.returns_true_when_string_contains_a_date <- function()
{
  x <- c("1999-12-31 23:59:59", "1979-08-01 01:00:00", "31 Dec 1999 11:59:59PM", "not a date", "NA")
  format <- "%d %b %Y %I:%M:%S%p"
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_date_string(x)
  )
} 


test.is_numeric_string.a_character_vector.returns_true_when_string_contains_a_number <- function()
{
  x <- c("1", "-2.3e4", "Inf", "one", "NA")
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_numeric_string(x)
  )
} 


test.is_missing_or_empty_character.a_scalar.returns_logical <- function()
{
  x <- c("foo", "", NA_character_, " ")
  expected <- c(FALSE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_missing_or_empty_character(x)
  )
} 


test.is_not_missing_nor_empty_character.a_scalar.returns_logical <- function()
{
  x <- c("foo", "", NA_character_, " ")
  expected <- c(TRUE, FALSE, FALSE, TRUE)
  names(expected) <- x
  checkEquals(
    expected,
    is_not_missing_nor_empty_character(x)
    )
} 

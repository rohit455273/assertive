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


test.is_credit_card_number.valid_card_numbers.returns_true_for_all <- function()
{
  x <- c(
    #visa
    "4111 1111 1111 1111",
    "4012888888881881",
    #mastercard
    "5555 5555 5555 4444",
    "5105 1051 0510 5100",
    #amex
    "3782 822463 10005",
    "3714 496353 98431",
    "3787 344936 71000", 
    #diners
    "3056 930902 5904",
    "3852 000002 3237",
    #discover
    "6011 1111 1111 1117",
    "6011 0009 9013 9424",
    #jcb
    "3530 1113 3330 0000",
    "3566 0020 2036 0505"
  )  
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_credit_card_number(x)
  )
}

test.is_credit_card_number.invalid_card_numbers.returns_false_for_all <- function()
{
  x <- c(
    #visa
    "4111 1111 1111 11111",  #too many digits
    "4012888888881882",      #bad check digit
    #mastercard
    "5655 5555 5555 4443",   #starts 56
    "51051 051 0510 5100",   #bad spacing
    #amex
    "3782 822463 1005"       #not enough digits
  )  
  expected <- rep.int(FALSE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_credit_card_number(x)
  )
}


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


test.is_email_address.a_character_vector_simple_match.returns_true_when_string_contains_an_email_address <- function()
{
  x <- c("foo@bar.com", "foo@@bar.com", "@bar.com", "foo@bar", "foo@bar.comma", "foo!@bar.com", "NA")
  expected <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_email_address(x)
  )
} 

test.is_email_address.a_character_vector_rfc2822_match.returns_true_when_string_contains_an_email_address <- function()
{
  x <- c("foo@bar.com", "foo@@bar.com", "@bar.com", "foo@bar", "foo@bar.comma", "foo!@bar.com", "NA")
  expected <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_email_address(x, method = "rfc2822")
  )
} 


test.is_ip_address.a_character_vector.returns_true_when_string_contains_an_ip_address <- function()
{
  x <- c(   
    localhost     = "localhost", 
    valid_address = "255.0.255.0", 
    out_of_range  = "1.2.3.256",
    five_blocks   = "1.2.3.4.5",
    non_numeric   = "1.2.3.Z",
    missing_block = "1.2.3.NA"
  )
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_ip_address(x)
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

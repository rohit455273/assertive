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


test.is_isbn_code.a_character_vector_type_10.returns_true_when_string_contains_an_isbn10_code <- function()
{
  x <- c(
    hyphens             = "0-387-98503-4",
    spaces              = "0 387 98503 4",
    just_numbers        = "0387985034",
    too_long            = "00-387-98503-4",
    too_short           = "0-387-9850-4",
    non_numeric         = "Z-387-98503-4",
    invalid_check_digit = "0-387-98503-5"
  )
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_isbn_code(x, type = "10")
  )
} 

test.is_isbn_code.a_character_vector_type_13.returns_true_when_string_contains_an_isbn13_code <- function()
{
  x <- c(
    hyphens             = "978-0-387-98503-9",
    spaces              = "978 0 387 98503 9",
    just_numbers        = "9780387985039",
    too_long            = "9978-0-387-98503-9",
    too_short           = "978-0-387-9850-9",
    non_numeric         = "Z78-0-387-9850-9",
    invalid_check_digit = "978-0-387-98503-8"
  )
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_isbn_code(x, type = "13")
  )
} 


test.is_missing_or_empty_character.a_character_vector.returns_true_when_string_is_missing_or_empty <- function()
{
  x <- c(
    missing      = NA_character_,
    empty        = "",
    non_empty    = "a",
    space        = " ",
    not_missing1 = "NA",
    not_missing2 = "<NA>"
  )
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_missing_or_empty_character(x)
  )
  checkEquals(
    !expected,
    is_not_missing_nor_empty_character(x)
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


test.is_uk_car_licence.a_character_vector.returns_true_when_string_contains_a_uk_car_licence <- function()
{
  x <- c(
    #1903 to 1931
    "A 1", "AA 9999",                       #ok
    "A 01",                                 #zero prefix on number
    "S0", "G0", "RG0", "LM0",               #ok, special plates
    #1931 to 1963
    "AAA 1", "AAA 999",                     #ok
    "III 1", "QQQ 1", "ZZZ 1",              #disallowed letters
    "AAA 01",                               #zero prefix on number
    #1931 to 1963 alt
    "1 AAA", "9999 AAA",                    #ok
    "1 III", "1 QQQ", "1 ZZZ",              #disallowed letters
    "01 AAA",                               #zero prefix on number
    #1963 to 1982
    "AAA 1A", "AAA 999A",                   #ok
    "AAA 1I", "AAA 1O", "AAA 1Q",           #disallowed letters
    "AAA 1U", "AAA 1Z", 
    "AAA 01A",                              #zero prefix on number
    #1982 to 2001
    "A1 AAA", "A999 AAA",                   #ok    
    "I1 AAA", "O1 AAA",                     #disallowed letters
    "U1 AAA", "Z1 AAA",
    "A01 AAA",                              #zero prefix on number
    #2001 to 2051
    "AA00 AAA", "AA99 AAA",                 #ok
    "II00 AAA", "QQ00 AAA", "ZZ00 AAA",     #disallowed letters
    "AA00 III", "AA00 QQQ"
  )
  expected <- c(
    TRUE, TRUE, 
    FALSE,
    TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE,
    FALSE, FALSE, FALSE,
    FALSE,
    TRUE, TRUE,
    FALSE, FALSE, FALSE,
    FALSE,
    TRUE, TRUE,
    FALSE, FALSE, FALSE,
    FALSE, FALSE,
    FALSE,
    TRUE, TRUE,
    FALSE, FALSE,
    FALSE, FALSE,
    FALSE,
    TRUE, TRUE,
    FALSE, FALSE, FALSE,
    FALSE, FALSE
  )
  names(expected) <- x
  checkEquals(
    expected,
    is_uk_car_licence(x)
  )
} 


test.is_uk_national_insurance_number.a_character_vector.returns_true_when_string_contains_a_uk_national_insurnce_number <- function()
{
  x <- c(
    "AA 00 00 00 A", "AA 00 00 00", "AA000000A",                #ok
    "ZZ 99 99 99 M", "ZZ 99 99 99", "ZZ999999M",                
    "DA 00 00 00", "FA 00 00 00", "IA 00 00 00",                #bad first letter
    "QA 00 00 00", "UA 00 00 00", "VA 00 00 00",
    "AD 00 00 00", "AF 00 00 00", "AI 00 00 00", "AO 00 00 00", #bad second letter
    "AQ 00 00 00", "AU 00 00 00", "AV 00 00 00",
    "AA 00 00 00 E", "AA 00 00 00 G", "AA 00 00 00 H",          #bad final letter
    "AA 00 00 00 I", "AA 00 00 00 J", "AA 00 00 00 K",
    "AA 00 00 00 L", "AA 00 00 00 N", "AA 00 00 00 O",
    "AA 00 00 00 P", "AA 00 00 00 Q", "AA 00 00 00 R",
    "AA 00 00 00 S", "AA 00 00 00 T", "AA 00 00 00 U",
    "AA 00 00 00 V", "AA 00 00 00 W", "AA 00 00 00 X",
    "AA 00 00 00 Y", "AA 00 00 00 Z"    
  )
  expected <- rep(c(TRUE, FALSE), times = c(6, 33))
  names(expected) <- x
  checkEquals(
    expected,
    is_uk_national_insurance_number(x)
  )
} 

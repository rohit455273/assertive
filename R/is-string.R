#' Does the character vector contain CAS registry numbers? 
#' 
#' Checks that the input contains Chemical Abstract Service registry numbers.
#' 
#' @param x Input to check.
#' @param .xname Not intended to be called directly.
#' @note CAS numbers take the form of 1 to 7 digits followed by a hyphen,  
#' followed by 2 digits, another hyphen and a final check digit.
#' @return A logical vector that is \code{TRUE} when the input contains valid  
#' CAS registry numbers.
#' @examples
#' x <- c(
#'   water = "7732-18-5", 
#'   d_glucose = "50-99-7",
#'   l_glucose = "921-60-8",
#'   no_hyphens = "7732185", 
#'   two_check_digits = "7732-18-55",
#'   bad_check_digit = "7732-18-4"
#' )
#' is_cas_number(x)
#' assert_any_are_cas_numbers(x)
#' \dontrun{
#' #These examples should fail.
#' assert_all_are_cas_numbers(x)
#' }
#' @references Chemspider is a good service for looking up CAS numbers.
#' @export
is_cas_number <- function(x, .xname = get_name_in_parent(x))
{
  #Check format
  rx <- c(d(1, 7), d(2), d(1))
  rx <- create_regex(rx, sep = "\\-")
  
  ok <- matches_regex(x, rx)
  
  #Check checkdigit
  x[ok] <- suppressWarnings(strip_non_numeric(x[ok]))
  ok[ok] <- bapply(
    character_to_list_of_numeric_vectors(x[ok]), 
    function(x)
    {
      lenx <- length(x)
      actual_check_digit <- x[lenx]
      x <- x[-lenx]
      expected_check_digit <- sum(rev(x) * seq_along(x)) %% 10L
      expected_check_digit == actual_check_digit
    }
  )
  ok
}

#' Does the character vector contain credit card numbers? 
#' 
#' Checks that the input contains credit card numbers.
#' 
#' @param x Input to check.
#' @param type Type of credit card.  Multiple types can be selected.
#' @param .xname Not intended to be called directly.
#' @note Legacy card numbers, for example 13 digit Visa numbers and 15 digits JCB 
#' numbers are not supported.
#' @return A logical vector that is \code{TRUE} when the input contains valid credit 
#' card numbers.
#' @examples
#' x <- c(
#'   #visa
#'   "4111 1111 1111 1111",    #spaces are allowed where they 
#'                             #would occur on the card
#'   "4012888888881881",       #though they can be omitted
#'   "4111 1111 1111 11111",   #too many digits
#'   "4012888888881882",       #bad check digit
#'   #mastercard
#'   "5555 5555 5555 4444",
#'   "5105 1051 0510 5100",
#'   "5655 5555 5555 4443",    #starts 56
#'   "51051 051 0510 5100",    #bad spacing
#'   #amex
#'   "3782 822463 10005",
#'   "3714 496353 98431",
#'   "3787 344936 71000", 
#'   "3782 822463 1005",       #not enough digits
#'   #diners
#'   "3056 930902 5904",
#'   "3852 000002 3237",
#'   #discover
#'   "6011 1111 1111 1117",
#'   "6011 0009 9013 9424",
#'   #jcb
#'   "3530 1113 3330 0000",
#'   "3566 0020 2036 0505"
#' )
#' is_credit_card_number(x)
#' assert_any_are_credit_card_numbers(x)
#' \dontrun{
#' assert_all_are_credit_card_numbers(x)
#' }
#' @references \url{http://www.regular-expressions.info/creditcard.html} contains the regexes
#' used by this function.
#' The example valid card numbers are from
#' \url{http://www.paypalobjects.com/en_US/vhelp/paypalmanager_help/credit_card_numbers.htm}
#' @export
is_credit_card_number <- function(x, type = c("visa", "mastercard", "amex", "diners", "discover", "jcb"), .xname = get_name_in_parent(x))
{
  #Check format
  type <- match.arg(type, several.ok = TRUE)
  
  rx <- list(
    visa       = c(paste0("4", d(3)), rep.int(d(4), 3)),
    mastercard = c(paste0("5[1-5]", d(2)), rep.int(d(4), 3)),
    amex       = c(paste0("3[47]", d(2)), d(6), d(5)),
    diners     = c("3(0[0-5]|[68][[:digit:]])[[:digit:]]", d(6), d(4)),
    discover   = c(paste("6011", paste0("65", d(2)), sep = "|"), rep.int(d(4), 3)),
    jcb        = c(paste0("35", d(2)), rep.int(d(4), 3))
  )
  rx <- create_regex(l = rx[type], sep = " ?")
  ok <- matches_regex(x, rx)
  
  x[ok] <- suppressWarnings(strip_non_numeric(x[ok]))
  
  #Check check digit with Luhn algorithm
  ok[ok] <- bapply(
    character_to_list_of_numeric_vectors(x[ok]),
    function(x)
    {
      lenx <- length(x)
      actual_check_digit <- x[lenx]
      x <- rev(x[-lenx])
      doubled <- suppressWarnings(x * 2:1L)
      total <- sum(doubled) - 9 * sum(doubled > 9)
      expected_check_digit <- (9 * total) %% 10L
      expected_check_digit == actual_check_digit
    }   
  )  
  ok
}

#' Does the character vector contain dates? 
#' 
#' Checks that the input contains dates or times.
#' 
#' @param x Input to check.
#' @param format Expected format of the dates.  See \code{\link[base]{strptime}}.
#' @param .xname Not intended to be called directly.
#' @return A logical vector that is \code{TRUE} when the input contains valid dates or times.
#' @examples
#' assert_all_are_date_strings("01Aug1979", format = "%d%b%Y") #My DOB!
#' @seealso \code{\link[base]{strptime}} for specifying formats, and the \code{lubridate}
#' package for automatic guessing of date formats (and other date manipulation functions).
#' @export
is_date_string <- function(x, format = "%F %T", .xname = get_name_in_parent(x))
{
  x <- coerce_to(x, "character")
  format <- use_first(format)
  f <- function(x) !is.na(strptime(x, format))
  call_and_name(f, x)  
}

#' Does the character vector contain email addresses?
#' 
#' Checks that the input contains email addresses.  (It does not check the the address exists, 
#' merely that the string is in a suitable format.)
#' 
#' @param x Input to check.
#' @param method Name of method to check for validity.  See notes below.
#' @param .xname Not intended to be called directly.
#' @note Each method specifies a regular expression (see \code{\link[base]{regex}}) to match 
#' against. The \code{simple} method matches most email addresses in use, and is quite good at
#' filtering out typos and nonsense.  It won't match \emph{every} email address however.  For 
#' example, emails from a top level domain longer than 4 characters won't pass.  The \code{rfc822}
#' method implements the offical standard for emails.  Thus all genuine emails will pass, but since
#' the spec is very broad, it isn't as good at filtering out nonsense.
#' @return A logical vector that is \code{TRUE} when the input contains valid email addresses.
#' @examples
#' addresses <- c("a@@b.com", "a_at_b.com", "a@@bcom", "a@@b.comma", "a!$&@@b.com")
#' is_email_address(addresses)
#' is_email_address(addresses, method = "rfc2822")
#' @references \url{http://www.regular-expressions.info/email.html} contains the regexes used
#' by this function and a good discussion of the pros and cons of each.
#' @export
is_email_address <- function(x, method = c("simple", "rfc2822"), .xname = get_name_in_parent(x))
{
  method <- match.arg(method)
  x <- coerce_to(x, "character")
  rx <- switch(
    method,
    simple = "^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}$",
    rfc2822 = "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"
  )
  matches_regex(x, rx, perl = TRUE)
}

#' Does the character vector contain IP addresses?
#' 
#' Checks that the input contains IP addresses.  (It does not check the the address exists, 
#' merely that the string is in a suitable format.)
#' 
#' @param x Input to check.
#' @param .xname Not intended to be called directly.
#' @note Valid IP addresses are considered to be four integers in the range 0 to 255, separated
#' by dots, or the string "localhost".
#' @return A logical vector that is \code{TRUE} when the input contains valid IP addresses.
#' @examples
#' x <- c(
#'   localhost     = "localhost", 
#'   valid_address = "255.0.255.0", 
#'   out_of_range  = "1.2.3.256",
#'   five_blocks   = "1.2.3.4.5",
#'   non_numeric   = "1.2.3.Z",
#'   missing_block = "1.2.3.NA"
#' )
#' is_ip_address(x)
#' assert_any_are_ip_addresses(x)
#' \dontrun{
#' #These examples should fail.
#' assert_all_are_ip_addresses(x)
#' }
#' @export
is_ip_address <- function(x, .xname = get_name_in_parent(x))
{
  x <- coerce_to(x, "character")  
  rx <- create_regex(rep.int(d(1, 3), 4), sep = "\\.")
  format_ok <- matches_regex(x, rx)
  
  blocks <- strsplit(x, ".", fixed = TRUE)
  range_ok <- bapply(
    blocks,
    function(b) all(suppressWarnings(as.integer(b) %in% 0:255))
  )
  
  (format_ok & range_ok) | (x == "localhost")
}

#' @rdname is_isbn_code
is_isbn10_code <- function(x, .xname = get_name_in_parent(x))
{
  #Check basic format
  digits <- "[[:digit:]]+"
  sep <- "[- ]?"
  rx <- create_regex(c(rep.int(digits, 3), "[[:digit:]X]"))
  format_ok <- matches_regex(x, rx)
  
  #Check correct amount of numbers
  x <- suppressWarnings(strip_non_numeric(x, allow_x = TRUE))
  n_digits_ok <- nchar(x) == 10L
  
  #Check checkdigit
  check_digit_ok <- bapply(
    character_to_list_of_numeric_vectors(x),
    function(x)
    {
      actual_check_digit <- x[10L]
      x <- x[1:9L]
      expected_check_digit <- (sum(x * 1:9L) %% 11L)
      if(expected_check_digit == 10L) return(is.na(actual_check_digit))
      expected_check_digit == actual_check_digit
    }
  )
  
  format_ok & n_digits_ok & check_digit_ok
}

#' @rdname is_isbn_code
is_isbn13_code <- function(x, .xname = get_name_in_parent(x))
{
  #Check basic format
  digits <- "[[:digit:]]+"
  rx <- create_regex(c(rep.int(digits, 4), "[[:digit:]X]"))
  format_ok <- matches_regex(x, rx)
  
  #Check correct amount of numbers
  x <- suppressWarnings(strip_non_numeric(x))
  n_digits_ok <- nchar(x) == 13L
  
  #Check checkdigit
  check_digit_ok <- bapply(
    character_to_list_of_numeric_vectors(x),
    function(x)
    {
      (sum(suppressWarnings(x * c(1, 3))) %% 10L) == 0L
    }
  )
  
  format_ok & n_digits_ok & check_digit_ok
}

#' Does the character vector contain ISBN book codes?
#' 
#' Checks that the input contains ISBN-10 or ISBN-13 book codes.
#' 
#' @param x Input to check.
#' @param type Either "isbn10", "isbn13" or both (for matching either type).
#' @param .xname Not intended to be called directly.
#' @return  A logical vector that is \code{TRUE} when the input contains valid ISBN book codes.
#' @examples
#' x10 <- c(
#'   hyphens             = "0-387-98503-4",
#'   spaces              = "0 387 98503 4",
#'   just_numbers        = "0387985034",
#'   too_long            = "00-387-98503-4",
#'   too_short           = "0-387-9850-4",
#'   non_numeric         = "Z-387-98503-4",
#'   invalid_check_digit = "0-387-98503-5"
#' )
#' x13 <- c(
#'   hyphens             = "978-0-387-98503-9",
#'   spaces              = "978 0 387 98503 9",
#'   just_numbers        = "9780387985039",
#'   too_long            = "9978-0-387-98503-9",
#'   too_short           = "978-0-387-9850-9",
#'   non_numeric         = "Z78-0-387-9850-9",
#'   invalid_check_digit = "978-0-387-98503-8"
#' )
#' is_isbn_code(x10, type = "10")
#' assert_any_are_isbn_codes(x10, type = "10")
#' is_isbn_code(x13, type = "13")
#' assert_any_are_isbn_codes(x13, type = "13")
#' \dontrun{
#' #These tests should fail.
#' assert_all_are_isbn_codes(x10, type = "10")
#' assert_all_are_isbn_codes(x13, type = "13")
#' }
#' @export
is_isbn_code <- function(x, type = c("10", "13"), .xname = get_name_in_parent(x))
{
  type <- match.arg(type, several.ok = TRUE)
  ok <- lapply(
    type, 
    function(isbn) 
    {
      fn <- switch(
        isbn,
        "10" = is_isbn10_code,
        "13" = is_isbn13_code
      )
      fn(x, .xname)
    }
  )
  Reduce(`|`, ok)
}

#' @rdname is_character
#' @export
is_missing_or_empty_character <- function(x)
{ 
  x <- coerce_to(x, "character")
  !nzchar(x) | is_na(x)
}

#' @rdname is_character
#' @export
is_not_missing_nor_empty_character <- function(x)
{ 
  x <- coerce_to(x, "character")
  nzchar(x) & !is_na(x)
}

#' @rdname is_character
#' @export
is_numeric_string <- function(x)
{
  x <- coerce_to(x, "character")
  numx <- suppressWarnings(as.numeric(x))
  ans <- is_not_na(numx)
  names(ans) <- x   #need to take names from x, not numx
  ans
}

#' Is the string a valid UK car licence plate number?
#'
#' Checks that the input contains UK car licence plate numbers.
#'
#' @param x Input to check.
#' @note A single space, in the appropriate place, is allowed but not compulsory.
#' @return \code{is_uk_national_insurance_number} returns \code{TRUE} if the input
#' string contains a valid UK car licence plate number The {assert_*} function returns nothing but 
#' throw an error when the \code{is_*} function returns \code{FALSE}.
#' @examples
#' licences <- c(
#'   #1903 to 1931
#'   "A 1", "AA 9999",                   #ok
#'   "A 01",                             #zero prefix on number
#'   "S0", "G0", "RG0", "LM0",           #ok, special plates
#'   #1931 to 1963
#'   "AAA 1", "AAA 999",                 #ok
#'   "III 1", "QQQ 1", "ZZZ 1",          #disallowed letters
#'   "AAA 01",                           #zero prefix on number
#'   #1931 to 1963 alt
#'   "1 AAA", "9999 AAA",                #ok
#'   "1 III", "1 QQQ", "1 ZZZ",          #disallowed letters
#'   "01 AAA",                           #zero prefix on number
#'   #1963 to 1982
#'   "AAA 1A", "AAA 999A",               #ok
#'   "AAA 1I", "AAA 1O", "AAA 1Q",       #disallowed letters
#'   "AAA 1U", "AAA 1Z", 
#'   "AAA 01A",                          #zero prefix on number
#'   #1982 to 2001
#'   "A1 AAA", "A999 AAA",               #ok    
#'   "I1 AAA", "O1 AAA",                 #disallowed letters
#'   "U1 AAA", "Z1 AAA",
#'   "A01 AAA",                          #zero prefix on number
#'   #2001 to 2051
#'   "AA00 AAA", "AA99 AAA",             #ok
#'   "II00 AAA", "QQ00 AAA", "ZZ00 AAA", #disallowed letters
#'   "AA00 III", "AA00 QQQ"
#' )
#' is_uk_car_licence(licences)
#' assert_any_are_uk_car_licences(licences)
#' \dontrun{
#' #These examples should fail.
#' assert_all_are_uk_car_licences(licences)
#' }
#' @references Regex taken from 
#' \url{http://www.regexlib.com/REDetails.aspx?regexp_id=527}.
#' @export
is_uk_car_licence <- function(x)
{
  #http://regexlib.com/REDetails.aspx?regexp_id=617
  #http://www.dreamincode.net/code/snippet3031.htm
  one_to_999 <- paste0("[1-9]", d(0, 2))
  rx <- create_regex(
    `1903 to 1932`         = c("[A-Z]{1,2}", paste0("[1-9]", d(0, 3))),
    `1903 to 1932 special` = c("S|G|RG|LM", "0"),
    `1932 to 1963`         = c("[A-HJ-PR-Y]{3}", one_to_999),
    `1932 to 1963 alt`     = c(paste0("[1-9]", d(0, 3)), "[A-HJ-PR-Y]{3}"),
    `1963 to 1982`         = c("[A-Z]{3}", one_to_999, "[A-HJ-NPR-TV-Y]"),
    `1983 to 2001`         = c("[A-HJ-NP-TV-Y]", one_to_999, "[A-Z]{3}"),
    `2001 to 2051`         = c(paste0("[A-HJ-PR-Y]{2}", d(2)), "[A-HJ-PR-Z]{3}"),
    sep = " ?"
  )
  matches_regex(x, rx)
}

#' Is the string a valid UK national insurance number?
#'
#' Checks that the input contains UK national insurance numbers.
#'
#' @param x Input to check.
#' @note A single space is allowed at the appropriate points (after the first two
#' letters and after each pair of numbers) but not compulsory
#' @return \code{is_uk_national_insurance_number} returns \code{TRUE} if the input
#' string contains a valid UK national insurance number.  The {assert_*} function 
#' returns nothing but throw an error when the \code{is_*} function returns 
#' \code{FALSE}.
#' @examples
#' ni_numbers <- c("AA 00 00 00 A", "AA000000A", "aa 00 00 00 a", "aa 00 00 00")
#' assert_all_are_uk_national_insurance_numbers(ni_numbers)
#' @references Regex taken from 
#' \url{http://www.regexlib.com/REDetails.aspx?regexp_id=527}.
is_uk_national_insurance_number <- function(x)
{
  rx <- create_regex(
    c(
      "[A-CEGHJ-PR-TW-Z]{1}[A-CEGHJ-NPR-TW-Z]{1}", 
      rep.int(d(2), 3), 
      "[A-DFM]?"
    ),
    sep = " ?"
  )
  matches_regex(x, rx)
}

#' Is the string a valid UK postcode?
#' 
#' Checks that the input contains UK postcodes.
#' 
#' @param x Input to check.
#' @return \code{is_uk_postcode} returns \code{TRUE} if the input string contains
#' a valid UK postcode. The {assert_*} function returns nothing but throws an error 
#' when the \code{is_*} function returns \code{FALSE}.
#' @note The function doesn't guarantee that the postcode actually exists.  It should
#' correctly return \code{TRUE} for genuine postcodes, and will weed out most badly
#' formatted strings and non-existent areas, but some non-existent districts may 
#' incorrectly return \code{TRUE}.  If you need 100% accuracy, check against an up-to-
#' date postcode database.
#' @examples
#' postcodes <- c("SW1A 1AA", "SK11 9DW", "M34FP", "Le45ns", "TS25 2BZ", "gir 0aa")
#' assert_all_are_uk_postcodes(postcodes)
#' @references Regexes taken from 
#' \url{https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom#Validation}.  
is_uk_postcode <- function(x)
{
  #Alternative regex, not used, at 
  #http://www.regexlib.com/REDetails.aspx?regexp_id=1064  
  standard_area <- "(A[BL]|B[ABDHLNRSTX]?|C[ABFHMORTVW]|D[ADEGHLNTY]|E[HNX]?|F[KY]|G[LUY]?|H[ADGPRSUX]|I[GMPV]|JE|K[ATWY]|L[ADELNSU]?|M[EKL]?|N[EGNPRW]?|O[LX]|P[AEHLOR]|R[GHM]|S[AEGKLMNOPRSTY]?|T[ADFNQRSW]|UB|W[ADFNRSV]|YO|ZE)[1-9]?[0-9]"
  london_area <- "((E|N|NW|SE|SW|W)1|EC[1-4]|WC[12])[A-HJKMNPR-Y]|(SW|W)([2-9]|[1-9][0-9])|EC[1-9][0-9]"
  district <- "[0-9][ABD-HJLNP-UW-Z]{2}"
  
  rx <- create_regex(    
    c(standard_area, district),
    c(london_area, district),
    c("GIR", "0AA"),
    sep = " ?"
  )
  matches_regex(x, rx)
}

#' Is the string a valid UK telephone number?
#' 
#' Checks that the input contains UK telephone numbers.
#' 
#' @param x Input to check.
#' @return \code{is_uk_telephone_number} returns \code{TRUE} if the input string contains
#' a valid UK telephone number. The {assert_*} function returns nothing but throws an error 
#' when the \code{is_*} function returns \code{FALSE}.
#' @note The function doesn't guarantee that the phone number is in use, but checks that
#' the format is correct, and that the area code exists.
#' Spaces, hyphens and round brackets are allowed to appear in arbitrary places.  The international UK
#' prefix of 0044 or +44 is allowed.
#' @examples
#' assert_all_are_uk_telephone_numbers(c("+44 207 219 3475", "08457 90 90 90"))
#' @references The regex is adapted from the one at
#' \url{http://www.aa-asterisk.org.uk/index.php/Regular_Expressions_for_Validating_and_Formatting_UK_Telephone_Numbers}
#' with some additional consultation from
#' \url{https://en.wikipedia.org/wiki/List_of_United_Kingdom_dialling_codes}
is_uk_telephone_number <- function(x)
{
  #Spaces and round brackets appear in arbitrary places; ignore them.
  x <- suppressWarnings(strip_invalid_chars(x, invalid_chars="[ -()]"))
  
  #All numbers should begin with 0 or the country code, 0044. Check and remove.
  start <- "(0|0044|\\+44)"
  first_rx <- create_regex(c(start, d(9, 10)), sep = "")
  ok <- matches_regex(x, first_rx)
  x[!ok] <- NA  #quick to reject in second pass
  x <- sub(paste0("^", start), "", x)
  d3 <- d(3)
  d6 <- d(6)
  d7 <- d(7)
  d8 <- d(8)
  d34 <- d(3, 4)
  regional <- paste0("[2-9]", d(4, 5))
  second_rx <- create_regex(
    #new style city
    c("20[01378]", d7),
    c("23[0189]", d7),
    c("24[017]", d7),
    c("28[0-46-9]", d7),
    c("29[012]", d7),
    #city
    c("113[0-48]", d6),
    c("11[46][0-4]", d6),
    c("115[012789]", d6),
    c("117[0-39]", d6),
    c("118[01349]", d6),
    c("121[0-7]", d6),
    c("131[0-8]", d6),
    c("1[459]1[[:digit:]]", d6),
    c("161[0-46-9]", d6),
    #regional
    c("120[024-9]", d6),
    c("122[3-9]", d6),
    c("123[3-79]", d6),
    c("124[1-689]", d6),
    c("12[58][02-9]", d6),
    c("126[0-4789]", d6),
    c("127[013-9]", d6),
    c("129[[:digit:]]", d6),
    c("130[[:digit:]]", d6),
    c("13[25][02-9]", d6),
    c("133[02-579]", d6),
    c("13[468][0-46-9]", d6),
    c("137[1235679]", d6),
    c("139[24578]", d6),
    c("140[03-9]", d6),
    c("142[02-5789]", d6),
    c("14[37][[:digit:]]", d6),
    c("144[02-69]", d6),
    c("145[0-8]", d6),
    c("14[69][0-79]", d6),
    c("150[1235-9]", d6),
    c("152[024-9]", d6),
    c("153[0145689]", d6),
    c("154[02-9]", d6),
    c("155[03-9]", d6),
    c("156[[:digit:]]", d6),
    c("157[0-35-9]", d6),
    c("158[0-468]", d6),
    c("159[0-5789]", d6),
    c("160[034689]", d6),
    c("162[0-689]", d6),
    c("16[38][013-9]", d6),
    c("164[1-467]", d6),
    c("165[0-69]", d6),
    c("166[13-9]", d6),
    c("167[0-8]", d6),
    c("169[0124578]", d6),
    c("170[0246-9]", d6),
    c("172[[:digit:]]|3[023678]", d6),
    c("174[03-9]", d6),
    c("175[0-46-9]", d6),
    c("176[013-9]", d6),
    c("177[0-35-9]", d6),
    c("178[024-9]", d6),
    c("179[02-9]", d6),
    c("180[35-9]", d6),
    c("182[1-5789]", d6),
    c("183[02-578]", d6),
    c("184[0-578]", d6),
    c("185[124-9]", d6),
    c("186[2-69]", d6),
    c("187[[:digit:]]", d6),
    c("188[02-9]", d6),
    c("189[02569]", d6),
    c("190[02-589]", d6),
    c("192[02-689]", d6),
    c("193[1-5789]", d6),
    c("194[2-9]", d6),
    c("195[0-579]", d6),
    c("196[234789]", d6),
    c("197[0124578]", d6),
    c("198[[:digit:]]", d6),
    c("199[2-57]", d6),
    #other regional
    c("12046[1-4]", d3),
    c("12087[2-9]", d3),
    c("12545[1-79]", d3),
    c("12762[[:digit:]]", d3),
    c("12763[1-8]", d3),
    c("12766[1-6]", d3),
    c("12972[0-4]", d3),
    c("12973[2-5]", d3),
    c("12982[2-8]", d3),
    c("12987[0-4789]", d3),
    c("12988[345]", d3),
    c("13638[2-5]", d3),
    c("13647[23]", d3),
    c("13847[04-9]", d3),
    c("13864[015789]", d3),
    c("14044[1-7]", d3),
    c("14202[23]", d3),
    c("14208[[:digit:]]", d3),
    c("146030", d3),
    c("14605[2-57]", d3),
    c("14606[1-8]", d3),
    c("14607[2-8]", d3),
    c("146140", d3),
    c("148052", d3),
    c("14887[123]", d3),
    c("15243[2-79]", d3),
    c("15246[[:digit:]]", d3),
    c("15276[[:digit:]]", d3),
    c("15626[06-9]", d3),
    c("156686", d3),
    c("16064[[:digit:]]", d3),
    c("16067[4-79]", d3),
    c("16295[567]", d3),
    c("1635[34][[:digit:]]", d3),
    c("164724", d3),
    c("164761", d3),
    c("16595[08]", d3),
    c("16596[67]", d3),
    c("165974", d3),
    c("16955[0-4]", d3),
    c("17266[13-9]", d3),
    c("17267[0-7]", d3),
    c("17442[[:digit:]]", d3),
    c("17502[0-3]", d3),
    c("1750[3-68]2", d3),
    c("175076", d3),
    c("1827[56][[:digit:]]", d3),
    c("18375[2-5]", d3),
    c("18378[239]", d3),
    c("18843[2-58]", d3),
    c("19006[1-8]", d3),
    c("190085", d3),
    c("19052[[:digit:]]", d3),
    c("193583", d3),
    c("19466[1-8]", d3),
    c("19492[01]", d3),
    c("194981", d3),
    c("196323", d3),
    c("19633[1-4]", d3),
    c("199561", d3),    
    #special regional
    "176888[234678][[:digit:]]{2}",
    "16977[23][[:digit:]]{3}",  
    #mobiles
    c("7[1-4]", d8),
    c("75([13-9][[:digit:]]|0[0-8]|2[0-35-9])", d6),
    c("7624", d6),
    c(
      "77([1-7][[:digit:]]|0[1-9]|8[02-9]|9[0-689])", 
      d6
    ),
    c("78([014-9][[:digit:]]|[23][0-8])", d6),
    c(
      "79([04-9][[:digit:]]|1[02-9]|2[0-35-9]|3[0-689])", 
      d6
    ),
    #pagers
    c(
      "76(0[012]|2[356]|4[0134]|5[49]|6[0-369]|77|81|9[39])", 
      d6
    ),
    #free
    c("800", "[[:digit:]]{6,7}|1111"),         
    c("808", d7),
    c("500", d6),
    #premium
    c("87[123]|9[01][[:digit:]]|98[123]", d7),
    #shared
    c("84[2345]|870", d7),
    #personal
    c("70", d8),
    #VoIP
    c("56", d8),
    #UAN
    c("55|3[0347]", d8)  
  )
  matches_regex(x, second_rx)
}

#' Is the input valid R code?
#'
#' Check to see if the input is a valid (parseable) R code.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{TRUE} if the input string is valid R code.
#' @examples
#' is_valid_r_code("x <- 1 + sqrt(pi)")
#' is_valid_r_code("x <- ")
#' is_valid_r_code("<- 1 + sqrt(pi)")
#' @seealso \code{\link[base]{parse}}
#' @export
is_valid_r_code <- function(x, .xname = get_name_in_parent(x))
{
  x <- use_first(coerce_to(x, "character"))
  ok <- is_error_free(parse(text = x))
  if(!ok)
  {
    return(false(
      "%s is not valid R code.\n%s.", 
      .xname, 
      cause(ok)
    ))
  }
  TRUE
}

#' Is the string a valid variable name?
#'
#' Checks strings to see if they are valid variable names.
#'
#' @param x Input to check.
#' @param allow_reserved If \code{TRUE} then "..." and "..1", "..2", etc. 
#' are considered valid.
#' @param allow_duplicates If \code{TRUE} then duplicated names are allowed.
#' @return \code{TRUE} if the input is a valid variable name.
#' The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link{make.names}}.
#' @examples
#' assert_all_are_valid_variable_names(c("x", "y_y0.y", ".", "...", "..1"))
#' \dontrun{
#' #These examples should fail.
#' assert_all_are_valid_variable_names(c("...", "..1"), allow_reserved = FALSE) 
#' assert_all_are_valid_variable_names(c("x", "x"), allow_duplicates = FALSE)
#' }
#' @references
#' \url{http://4dpiecharts.com/2011/07/04/testing-for-valid-variable-names/}
#' @export
is_valid_variable_name <- function(x, allow_reserved = TRUE, allow_duplicates = TRUE)
{
  x <- coerce_to(x, "character")
  ok <- rep.int(TRUE, length(x))
  
  #is name too long?
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  ok[nchar(x) > max_name_length] <- FALSE
  
  #is it a reserved variable, i.e.
  #an ellipsis or two dots then a number?
  if(!allow_reserved)
  {
    ok[x == "..."] <- FALSE
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE
  }
  
  #are names valid (and maybe unique)
  ok[x != make.names(x, unique = !allow_duplicates)] <- FALSE
  
  names(ok) <- x
  ok
}

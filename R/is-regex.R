#' Does the character vector contain CAS registry numbers? 
#' 
#' Checks that the input contains Chemical Abstract Service registry numbers.
#' 
#' @param x Input to check.
#' @param .xname Not intended to be called directly.
#' @return A logical vector that is \code{TRUE} when the input contains valid CAS registry numbers.
#' @examples
#' x <- c(
#'   water = "7732-18-5", 
#'   d_glucose = "50-99-7",
#'   l_glucose = "921-60-8",
#'   no_hyphens = "7732185", 
#'   two_check_digits = "7732-18-55",
#'   bad_check_digit = "7732-18-4"
#' )
#' is_cas_character(x)
#' assert_any_are_cas_characters(x)
#' \dontrun{
#' #These examples should fail
#' assert_all_are_cas_characters(x)
#' }
#' @references TODO
#' @export
is_cas_character <- function(x, .xname = get_name_in_parent(x))
{
  #Check format
  rx <- "^[[:digit:]]{1,7}-[[:digit:]]{2}-[[:digit:]]$"
  
  format_ok <- call_and_name(function(x) grepl(rx, x), x)
  
  #Check checkdigit
  x <- suppressWarnings(strip_non_numeric(x))
  check_digit_ok <- bapply(
    character_to_list_of_numeric_vectors(x), 
    function(x)
    {
      lenx <- length(x)
      actual_check_digit <- x[lenx]
      x <- x[-lenx]
      expected_check_digit <- sum(rev(x) * seq_along(x)) %% 10L
      expected_check_digit == actual_check_digit
    }
  )
  format_ok & check_digit_ok
}

#' Does the character vector contain credit card numbers? 
#' 
#' Checks that the input contains credit card numbers.
#' 
#' @param x Input to check.
#' @param type Type of credit card.  Multiple types can be selected.
#' @param .xname Not intended to be called directly.
#' @return A logical vector that is \code{TRUE} when the input contains valid credit card numbers.
#' @examples
#' x <- c(
#'   visa       = "4111 1111 1111 1111",
#'   mastercard = "5111 1111 1111 1118",
#'   amex       = "3411 1111 1111 112", 
#'   diners     = "6011 1111 1111 1115",
#'   discovery  = "6111 1111 1111 1116",
#'   jcb        = "2131 1111 1111 113"
#' )
#' suppressWarnings(is_credit_card_character(x))
#' assert_all_are_credit_card_characters(x)
#' @reference \url{http://www.regular-expressions.info/creditcard.html} contains the regexes
#' used by this function.
#' @export
is_credit_card_character <- function(x, type = c("visa", "mastercard", "amex", "diners", "discover", "jcb"), .xname = get_name_in_parent(x))
{
  #Check format
  type <- match.arg(type, several.ok = TRUE)
  x <- strip_non_numeric(x)
  
  rx <- c(
    visa = "^4[[:digit:]]{12}(?:[[:digit:]]{3})?$",
    mastercard = "^5[1-5][[:digit:]]{14}$",
    amex = "^3[47][[:digit:]]{13}$",
    diners = "^3(?:0[0-5]|[68][[:digit:]])[[:digit:]]{11}$",
    discover = "^6(?:011|5[[:digit:]]{2})[[:digit:]]{12}$",
    jcb = "^(?:2131|1800|35[[:digit:]]{3})[[:digit:]]{11}$"
  )
  rx <- paste(rx[type], collapse = "|")
  format_ok <- matches_regex(x, rx)
  
  #Check check digit with Luhn algorithm
  check_digit_ok <- bapply(
    character_to_list_of_numeric_vectors(x),
    function(x)
    {
      lenx <- length(x)
      actual_check_digit <- x[lenx]
      x <- rev(x[-lenx])
      doubled <- suppressWarnings(x * 1:2L)
      total <- sum(doubled) - 9 * sum(doubled > 9)
      expected_check_digit <- (9 * total) %% 10L
      expected_check_digit == actual_check_digit
    }   
  )
  
  format_ok & check_digit_ok
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
#' assert_all_are_date_characters("01Aug1979", format = "%d%b%Y") #My DOB!
#' @seealso \code{\link[base]{strptime}} for specifying formats, and the \code{lubridate}
#' package for automatic guessing of date formats (and other date manipulation functions).
#' @export
is_date_character <- function(x, format, .xname = get_name_in_parent(x))
{
  x <- coerce_to(x, "character")  
  call_and_name(function(x) !is.na(strptime(x, format)), x)  
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
#' assert_all_are_email_characters("richierocks@gmail.com")
#' @reference \url{http://www.regular-expressions.info/email.html} contains the regexes used
#' by this function and a good discussion of the pros and cons of each.
#' @export
is_email_character <- function(x, method = c("simple", "rfc2822"), .xname = get_name_in_parent(x))
{
  method <- match.arg(method)
  x <- coerce_to(x, "character")  
  rx <- switch(
    method,
    simple = "^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}$",
    rfc2822 = "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"
  )
  matches_regex(x, rx, ignore.case = TRUE, perl = TRUE)
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
#'   non_numeric   = "1.2.3.Z"
#' )
#' is_ip_address_character(x)
#' assert_any_are_ip_address_characters(x)
#' \dontrun{
#' #These examples should fail
#' assert_all_are_ip_address_characters(x)
#' }
#' @reference \url{http://www.regular-expressions.info/email.html} contains the regexes used
#' by this function and a good discussion of the pros and cons of each.
#' @export
is_ip_address_character <- function(x, .xname = get_name_in_parent(x))
{
  x <- coerce_to(x, "character")  
  digits <- "[[:digit:]]{1,3}"
  dot <- "\\."
  rx <- paste0("^", digits, dot, digits, dot, digits, dot, digits, "$")
  format_ok <- matches_regex(x, rx)
  
  blocks <- strsplit(x, ".", fixed = TRUE)
  range_ok <- bapply(
    blocks,
    function(b) all(suppressWarnings(as.numeric(b) %in% 0:255))
  )
  
  (format_ok & range_ok) | (x == "localhost")
}

#' Does the character vector contain ISBN book codes?
#' 
#' Checks that the input contains ISBN-10 or ISBN-13 book codes.
#' 
#' @param x Input to check.
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
#' is_isbn_character(x10, type = "isbn10")
#' assert_any_are_isbn_characters(x10, type = "isbn10")
#' is_isbn_character(x13, type = "isbn13")
#' assert_any_are_isbn_characters(x13, type = "isbn13")
#' \dontrun{
#' #These tests should fail:
#' assert_all_are_isbn_characters(x10, type = "isbn10")
#' assert_all_are_isbn_characters(x13, type = "isbn13")
#' }
#' @export
is_isbn_character <- function(x, type = c("isbn10", "isbn13"), .xname = get_name_in_parent(x))
{
  type <- match.arg(type, several.ok = TRUE)
  ok <- lapply(
    type, 
    function(isbn) 
    {
      match.fun(paste0("is_", isbn, "_character"))(x, .xname)
    }
  )
  Reduce(`|`, ok)
}

#' @rdname is_isbn_character
is_isbn10_character <- function(x, .xname = get_name_in_parent(x))
{
  #Check basic format
  digits <- "[[:digit:]]+"
  sep <- "[- ]?"
  rx <- paste0("^", digits, sep, digits, sep, digits, sep, "[[:digit:]X]", "$")
  format_ok <- matches_regex(x, rx)
  
  #Check correct amount of numbers
  x <- suppressWarnings(strip_non_numeric(x, allow_X = TRUE))
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

#' @rdname is_isbn_character
is_isbn13_character <- function(x, .xname = get_name_in_parent(x))
{
  #Check basic format
  digits <- "[[:digit:]]+"
  sep <- "[- ]?"
  rx <- paste0("^", digits, sep, digits, sep, digits, sep, digits, sep, "[[:digit:]]", "$")
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

is_uk_car_licence_character <- function(x, .xname)
{
  #http://regexlib.com/REDetails.aspx?regexp_id=617
  #http://www.dreamincode.net/code/snippet3031.htm
  message("TODO")
  TRUE
}

is_uk_national_insurance_character <- function(x, .xname)
{
  #http://www.regexlib.com/REDetails.aspx?regexp_id=527
  message("TODO")
  TRUE
}

is_uk_postcode_character <- function(x, .xname)
{
  #http://www.regexlib.com/REDetails.aspx?regexp_id=260
  #http://www.regexlib.com/REDetails.aspx?regexp_id=1064
  message("TODO")
  TRUE
}

is_uk_telephone_character <- function(x, .xname)
{
  #http://www.regexlib.com/REDetails.aspx?regexp_id=684
  message("TODO")
  TRUE
}

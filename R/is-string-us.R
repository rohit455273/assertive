#' Is the string a valid US zip code?
#' 
#' Checks that the input contains US zip codes.
#' 
#' @param x Input to check.
#' @return \code{is_us_zip_code} returns \code{TRUE} if the input string contains
#' a valid US zip code. The {assert_*} functions return nothing but throw an error 
#' when the \code{is_*} function returns \code{FALSE}.
#' @note A valid zip code is considered to be 5 digits, or 5 digits then a hyphen 
#' then 4 digits.  Unused area prefixes return FALSE, but the function doesn't 
#' guarantee that the zip code actually exists.  It should correctly return 
#' \code{TRUE} for genuine zip codes, and will weed out most badly formatted strings 
#' non-existent areas, but some non-existent codes may incorrectly return 
#' \code{TRUE}.  If you need 100% accuracy, check against an up-to-date zip code 
#' base.
#' @examples
#' zip_codes <- c(
#'   "90210", 
#'   "20500", 
#'   "22313-1450",  #5+4 style ok
#'   "223131450",   #fails, no hyphen
#'   "09901"        #fails, invalid area prefix
#'  )
#' is_us_zip_code(zip_codes)
#' assert_all_are_us_zip_codes(zip_codes)
#' @references Regexes inferred from 
#' \url{https://en.wikipedia.org/wiki/ZIP_code} and 
#' \url{https://en.wikipedia.org/wiki/List_of_ZIP_code_prefixes}.
#' @export
is_us_zip_code <- function(x)
{
  prefix <- setdiff(
    0:999,
    c(
      0, 2:4, 99, 213, 269, 343, 345, 348, 353, 419,
      428, 429, 517:519, 529, 533, 536, 552, 568, 578,
      579, 589, 621, 632, 642, 643, 659, 663, 682, 
      694:699, 702, 709, 715, 732, 742, 771, 817, 818, 
      819, 839, 848, 849, 854, 858, 861, 862, 866:869, 
      876, 886:888, 892, 896, 899, 909, 929, 987
    )
  )
  prefix <- paste0(
    "(",
    paste(
      formatC(
        prefix,
        width = 3,
        flag = "0"
      ),
      collapse = "|"
    ),
    ")" 
  )  
  rx <- create_regex(c(prefix, d(2), "(-", d(4), ")?"), sep = "")  
  matches_regex(x, rx)
}

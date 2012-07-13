\name{assert_all_are_uk_national_insurance_numbers}
\alias{assert_all_are_uk_national_insurance_numbers}
\alias{assert_any_are_uk_national_insurance_numbers}
\alias{is_uk_national_insurance_number}
\title{Is the string a valid UK national insurance number?}
\usage{
  assert_all_are_uk_national_insurance_numbers(x)

  assert_any_are_uk_national_insurance_numbers(x)

  is_uk_national_insurance_number(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_uk_national_insurance_number} returns
  \code{TRUE} if the input string contains a valid UK
  national insurance number.  The {assert_*} function
  returns nothing but throw an error when the \code{is_*}
  function returns \code{FALSE}.
}
\description{
  Checks that the input contains UK national insurance
  numbers.
}
\examples{
ni_numbers <- c("AA 00 00 00 A", "AA000000A", "aa 00 00 00 a", "aa 00 00 00")
assert_all_are_uk_national_insurance_numbers(ni_numbers)
}
\references{
  Regex taken from
  \url{http://www.regexlib.com/REDetails.aspx?regexp_id=527}.
}


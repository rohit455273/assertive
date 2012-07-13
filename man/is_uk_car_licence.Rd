\name{assert_all_are_uk_car_licences}
\alias{assert_all_are_uk_car_licences}
\alias{assert_any_are_uk_car_licences}
\alias{is_uk_car_licence}
\title{Is the string a valid UK car licence plate number?}
\usage{
  assert_all_are_uk_car_licences(x)

  assert_any_are_uk_car_licences(x)

  is_uk_car_licence(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_uk_national_insurance_number} returns
  \code{TRUE} if the input string contains a valid UK car
  licence plate number The {assert_*} function returns
  nothing but throw an error when the \code{is_*} function
  returns \code{FALSE}.
}
\description{
  Checks that the input contains UK car licence plate
  numbers.
}
\note{
  A single space, in the appropriate place, is allowed but
  not compulsory.
}
\examples{
licences <- c(
  "AA 11 AAA", "AA11AAA", "aa 11 aaa",
  "A1 AAA", "AAA 1A", "A999 AAA", "AAA 999A",
  "1 AAA", "AAA 1", "9999 AAA", "AAA 999",
  "AA 1", "AA 9999"
)
assert_all_are_uk_car_licences(licences)
}
\references{
  Regex taken from
  \url{http://www.regexlib.com/REDetails.aspx?regexp_id=527}.
}


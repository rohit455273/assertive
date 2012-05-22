\name{assert_all_are_not_na}
\alias{assert_all_are_not_na}
\alias{assert_any_are_not_na}
\alias{is_not_na}
\title{Is the input present?}
\usage{
  assert_all_are_not_na(x)

  assert_any_are_not_na(x)

  is_not_na(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_not_na} is the negation of \code{is.na}.
  \code{assert_is_not_na} returns nothing but throws an
  error if \code{is_not_na} returns \code{FALSE}.
}
\description{
  Checks to see if the input is not NA.
}
\examples{
assert_is_not_na(1:10)
}
\seealso{
  \code{\link[base]{is.na}}
}


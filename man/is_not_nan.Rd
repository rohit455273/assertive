\name{assert_all_are_not_nan}
\alias{assert_all_are_not_nan}
\alias{assert_any_are_not_nan}
\alias{is_not_nan}
\title{Is the input not NaN?}
\usage{
  assert_all_are_not_nan(x)

  assert_any_are_not_nan(x)

  is_not_nan(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_not_nan} returns \code{TRUE} if the input is
  numeric and is not nan.  \code{assert_is_not_nan} returns
  nothing but throws an error if \code{is_not_nan} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is a number that isn't NaN.
}
\examples{
assert_is_not_nan(1:10)
}
\seealso{
  \code{\link[base]{is.nan}}
}


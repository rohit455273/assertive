\name{assert_is_array}
\alias{assert_is_array}
\alias{is_array}
\title{Is the input an array?}
\usage{
  assert_is_array(x)

  is_array(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_array} wraps \code{is.array}, providing more
  information on failure.  \code{assert_is_array} returns
  nothing but throws an error if \code{is_array} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is an array.
}
\examples{
assert_is_array(array())
assert_is_array(matrix())
}
\seealso{
  \code{\link[base]{is.array}}.
}


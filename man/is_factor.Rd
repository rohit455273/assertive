\name{assert_is_factor}
\alias{assert_is_factor}
\alias{is_factor}
\title{Is the input an factor?}
\usage{
  assert_is_factor(x)

  is_factor(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_factor} wraps \code{is.factor}, providing more
  information on failure.  \code{assert_is_factor} returns
  nothing but throws an error if \code{is_factor} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is an factor.
}
\examples{
assert_is_factor(factor(sample(letters, 10)))
}
\seealso{
  \code{\link[base]{is.factor}}.
}


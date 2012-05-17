\name{assert_is_environment}
\alias{assert_is_environment}
\alias{is_environment}
\title{Is the input an environment?}
\usage{
  assert_is_environment(x)

  is_environment(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_environment} wraps \code{is.environment},
  providing more information on failure.
  \code{assert_is_environment} returns nothing but throws
  an error if \code{is_environment} returns \code{FALSE}.
}
\description{
  Checks to see if the input is an environment.
}
\examples{
assert_is_environment(new.env())
assert_is_environment(globalenv())
assert_is_environment(baseenv())
}
\seealso{
  \code{\link[base]{is.environment}}.
}


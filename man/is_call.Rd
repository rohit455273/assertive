\name{assert_is_call}
\alias{assert_is_call}
\alias{is_call}
\title{Is the input a call?}
\usage{
  assert_is_call(x)

  is_call(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_call} wraps \code{is.call}, providing more
  information on failure.  \code{assert_is_call} returns
  nothing but throws an error if \code{is_call} returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is a call.
}
\examples{
assert_is_call(call("sin", "pi"))
}
\seealso{
  \code{\link[base]{is.call}}.
}


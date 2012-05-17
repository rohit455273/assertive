\name{assert_is_an_integer}
\alias{assert_is_an_integer}
\alias{is_an_integer}
\alias{is_integer}
\title{Is the input an integer?}
\usage{
  assert_is_an_integer(x)

  is_an_integer(x)

  is_integer(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_integer} wraps \code{is.integer}, providing more
  information on failure. \code{is_an_integer} returns
  \code{TRUE} if the input is an integer and scalar.  The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is an integer.
}
\examples{
assert_is_integer(1L:10L)
assert_is_an_integer(99L)
}
\seealso{
  \code{\link[base]{is.integer}} and
  \code{\link{is_scalar}}.
}


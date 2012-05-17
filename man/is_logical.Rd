\name{assert_is_a_bool}
\alias{assert_is_a_bool}
\alias{assert_is_logical}
\alias{is_a_bool}
\alias{is_logical}
\title{Is the input logical?}
\usage{
  assert_is_a_bool(x)

  assert_is_logical(x)

  is_a_bool(x)

  is_logical(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_logical} wraps \code{is.logical}, providing more
  information on failure. \code{is_a_bool} returns
  \code{TRUE} if the input is logical and scalar.  The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input is logical.
}
\examples{
assert_is_logical(runif(10) > 0.5)
assert_is_a_bool(TRUE)
assert_is_a_bool(NA)
}
\seealso{
  \code{\link[base]{is.logical}} and
  \code{\link{is_scalar}}.
}


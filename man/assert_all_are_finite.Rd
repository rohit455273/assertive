\name{assert_all_are_finite}
\alias{assert_all_are_finite}
\alias{assert_all_are_infinite}
\alias{assert_any_are_finite}
\alias{assert_any_are_infinite}
\title{Are the inputs (in)finite?}
\usage{
  assert_all_are_finite(x)

  assert_any_are_finite(x)

  assert_all_are_infinite(x)

  assert_any_are_infinite(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{assert_all_are_finite} and
  \code{assert_any_are_finite} return nothing but throw an
  error if the inputs to \code{is.finite} are not
  \code{TRUE}. \code{assert_all_are_infinite} and
  \code{assert_any_are_infinite} work likewise for
  \code{is.infinite}.
}
\description{
  Checks to see if the inputs are (in)finite.
}
\note{
  Note that there are no corresponding \code{is_finite} and
  \code{is_finite} functions in the package.  Use
  \code{is.finite} and \code{is.infinite} instead.
}
\examples{
assert_all_are_finite(1:10)
assert_any_are_finite(c(1, Inf))
}
\seealso{
  \code{\link[base]{is.finite}}
}


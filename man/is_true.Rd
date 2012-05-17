\name{assert_is_false}
\alias{assert_is_false}
\alias{assert_is_true}
\alias{is_false}
\alias{is_true}
\title{Is the input TRUE?}
\usage{
  assert_is_false(x)

  assert_is_true(x)

  is_false(x)

  is_true(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{TRUE} if the input is identical to \code{TRUE}. The
  \code{assert_*} functions return nothing but throw an
  error if the corresponding \code{is_*} function returns
  \code{FALSE}.
}
\description{
  Checks to see if the input if \code{TRUE}.
}
\examples{
assert_is_true(TRUE)
assert_is_false(FALSE)
}
\seealso{
  \code{\link[base]{isTRUE}}.
}


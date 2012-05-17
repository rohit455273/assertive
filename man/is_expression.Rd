\name{assert_is_expression}
\alias{assert_is_expression}
\alias{is_expression}
\title{Is the input an expression?}
\usage{
  assert_is_expression(x)

  is_expression(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_expression} wraps \code{is.expression},
  providing more information on failure.
  \code{assert_is_expression} returns nothing but throws an
  error if \code{is_expression} returns \code{FALSE}.
}
\description{
  Checks to see if the input is an expression.
}
\examples{
assert_is_expression(expression(1:10 + pi, sqrt(169)))
}
\seealso{
  \code{\link[base]{is.expression}}.
}


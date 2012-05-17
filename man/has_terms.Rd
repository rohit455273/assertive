\name{assert_has_terms}
\alias{assert_has_terms}
\alias{has_terms}
\title{Does the input have terms?}
\usage{
  assert_has_terms(x)

  has_terms(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{has_terms} returns \code{TRUE} if the input has an
  element or an attribute named terms.
  \code{assert_has_terms} returns nothing but throws an
  error if \code{has_terms} is not \code{TRUE}.
}
\description{
  Checks to see if the input has a terms component or
  attribute.
}
\examples{
assert_has_terms(lm(uptake ~ conc, CO2))
}
\seealso{
  \code{\link[stats]{terms.default}}.
}


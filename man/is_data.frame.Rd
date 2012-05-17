\name{assert_is_data.frame}
\alias{assert_is_data.frame}
\alias{is_data.frame}
\title{Checks to see if the input is a data.frame.}
\usage{
  assert_is_data.frame(x)

  is_data.frame(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_data.frame} wraps \code{is.data.frame},
  providing more information on failure.
  \code{assert_is_data.frame} returns nothing but throws an
  error if \code{is_data.frame} returns \code{FALSE}.
}
\description{
  Checks to see if the input is a data.frame.
}
\examples{
assert_is_data.frame(data.frame())
assert_is_data.frame(datasets::CO2)
}
\seealso{
  \code{\link[base]{is.data.frame}}.
}


\name{assert_is_function}
\alias{assert_is_function}
\alias{assert_is_primitive}
\alias{is_function}
\alias{is_primitive}
\title{Is the input a function?}
\usage{
  assert_is_function(x)

  assert_is_primitive(x)

  is_function(x)

  is_primitive(x)
}
\arguments{
  \item{x}{Input to check.}
}
\value{
  \code{is_function} and \code{is_primitive} wrap
  \code{is.function} and \code{is.primitive}, providing
  more information on failure.  The \code{assert_*}
  functions return nothing butthrow an error if the
  corresponding \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks to see if the input is a function.
}
\examples{
assert_is_function(sqrt)
assert_is_function(function(){})
}
\seealso{
  \code{\link[base]{is.function}}.
}


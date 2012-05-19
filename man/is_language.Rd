\name{assert_is_call}
\alias{assert_is_call}
\alias{assert_is_expression}
\alias{assert_is_language}
\alias{assert_is_name}
\alias{assert_is_symbol}
\alias{is_call}
\alias{is_expression}
\alias{is_language}
\alias{is_name}
\alias{is_symbol}
\title{Is the input a language object?}
\usage{
  assert_is_call(x)

  assert_is_expression(x)

  assert_is_language(x)

  assert_is_name(x)

  assert_is_symbol(x)

  is_call(x, .xname = get_name_in_parent(x))

  is_expression(x, .xname = get_name_in_parent(x))

  is_language(x, .xname = get_name_in_parent(x))

  is_name(x, .xname = get_name_in_parent(x))

  is_symbol(x, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{is_call}, \code{is_expression}, \code{is_language},
  \code{is_name} and \code{is_symbol} wrap the
  corresponding \code{is.*} functions, providing more
  information on failure.The \code{assert_*} functions
  return nothing but throw an error if the corresponding
  \code{is_*} function returns \code{FALSE}.
}
\description{
  Checks to see if the input is a language object.
}
\note{
  \code{is_name} and \code{is_symbol} are different names
  for the same function.
}
\examples{
assert_is_call(call("sin", "pi"))
}
\seealso{
  \code{\link[base]{is.call}}.
}


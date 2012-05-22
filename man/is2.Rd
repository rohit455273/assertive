\name{assert_is}
\alias{assert_is}
\alias{is2}
\title{Alternative version of is.}
\usage{
  assert_is(x, class, .xname = get_name_in_parent(x))

  is2(x, class, .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{class}{Target class that \code{x} maybe belong to.}

  \item{.xname}{Not intended to be used directly.}
}
\value{
  \code{TRUE} if x belongs to the class and \code{FALSE}
  otherwise.  \code{assert_is} returns nothing but throws
  an error if \code{x} does not have class \code{class}.
}
\description{
  If a function named \code{is.class} exists, call
  \code{is.class(x)}. If not, call \code{is(x, class)}.
}
\examples{
assert_is(1:10, "numeric")
#These examples should fail:
\dontrun{
assert_is(1:10, "list")
}
}
\seealso{
  \code{\link[methods]{is}}.
}


\name{false}
\alias{false}
\title{FALSE, with a message.}
\usage{
  false(...)
}
\arguments{
  \item{...}{Passed to sprintf to create a message.}
}
\value{
  \code{FALSE} with the attribute \code{cause}, as provided
  in the input.
}
\description{
  Always returns the value \code{FALSE}, with a message
  attribute.
}
\seealso{
  \code{\link{cause}}
}


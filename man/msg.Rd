\name{msg}
\alias{msg}
\alias{msg<-}
\title{Get or set the message attribute.}
\usage{
  msg(x)

  msg(x, value) <- value
}
\arguments{
  \item{x}{Any variable.}

  \item{value}{The value to set the message attribute to.}
}
\value{
  The get method returns the \code{"message"} attribute.
}
\description{
  Gets or sets the message attribute of a variable.
}
\examples{
yn <- is_a_bool(123)
msg(yn)
}


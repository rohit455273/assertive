\name{matches_regex}
\alias{matches_regex}
\title{Does the input match the regular expression?}
\usage{
  matches_regex(x, rx, ...)
}
\arguments{
  \item{x}{Inout to check.}

  \item{rx}{A regular expression.}

  \item{...}{Passed to \code{\link{grepl}}.}
}
\value{
  A logical vector that is \code{TRUE} when the input
  matches the regular expression.
}
\description{
  Checks that the input matches the regular expression.
}
\seealso{
  \code{\link{regex}} and \code{\link{regexpr}}.
}


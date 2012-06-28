\name{assert_all_are_date_strings}
\alias{assert_all_are_date_strings}
\alias{assert_any_are_date_strings}
\alias{is_date_character}
\title{Does the character vector contain dates?}
\usage{
  assert_all_are_date_strings(x)

  assert_any_are_date_strings(x)

  is_date_character(x, format,
    .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{format}{Expected format of the dates.  See
  \code{\link[base]{strptime}}.}

  \item{.xname}{Not intended to be called directly.}
}
\value{
  A logical vector that is \code{TRUE} when the input
  contains valid dates or times.
}
\description{
  Checks that the input contains dates or times.
}
\examples{
assert_all_are_date_characters("01Aug1979", format = "\%d\%b\%Y") #My DOB!
}
\seealso{
  \code{\link[base]{strptime}} for specifying formats, and
  the \code{lubridate} package for automatic guessing of
  date formats (and other date manipulation functions).
}


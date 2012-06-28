\name{assert_all_are_credit_card_numbers}
\alias{assert_all_are_credit_card_numbers}
\alias{assert_any_are_credit_card_numbers}
\alias{is_credit_card_number}
\title{Does the character vector contain credit card numbers?}
\usage{
  assert_all_are_credit_card_numbers(x)

  assert_any_are_credit_card_numbers(x)

  is_credit_card_number(x,
    type = c("visa", "mastercard", "amex", "diners", "discover", "jcb"),
    .xname = get_name_in_parent(x))
}
\arguments{
  \item{x}{Input to check.}

  \item{type}{Type of credit card.  Multiple types can be
  selected.}

  \item{.xname}{Not intended to be called directly.}
}
\value{
  A logical vector that is \code{TRUE} when the input
  contains valid credit card numbers.
}
\description{
  Checks that the input contains credit card numbers.
}
\examples{
x <- c(
  visa       = "4111 1111 1111 1111",
  mastercard = "5111 1111 1111 1118",
  amex       = "3411 1111 1111 112",
  diners     = "6011 1111 1111 1115",
  discovery  = "6111 1111 1111 1116",
  jcb        = "2131 1111 1111 113"
)
suppressWarnings(is_credit_card_number(x))
assert_all_are_credit_card_numbers(x)
}
\references{
  \url{http://www.regular-expressions.info/creditcard.html}
  contains the regexes used by this function.
}


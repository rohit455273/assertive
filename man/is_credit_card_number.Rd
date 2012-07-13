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
\note{
  Legacy card numbers, for example 13 digit Visa numbers
  and 15 digits JCB numbers are not supported.
}
\examples{
x <- c(
  #visa
  "4111 1111 1111 1111",    #spaces are allowed where they
                            #would occur on the card
  "4012888888881881",       #though they can be omitted
  #mastercard
  "5555 5555 5555 4444",
  "5105 1051 0510 5100",
  #amex
  "3782 822463 10005",
  "3714 496353 98431",
  "3787 344936 71000",
  #diners
  "3056 930902 5904",
  "3852 000002 3237",
  #discover
  "6011 1111 1111 1117",
  "6011 0009 9013 9424",
  #jcb
  "3530 1113 3330 0000",
  "3566 0020 2036 0505"
)
is_credit_card_number(x)
assert_all_are_credit_card_numbers(x)
}
\references{
  \url{http://www.regular-expressions.info/creditcard.html}
  contains the regexes used by this function. The example
  card numbers are from
  \url{http://www.paypalobjects.com/en_US/vhelp/paypalmanager_help/credit_card_numbers.htm}
}


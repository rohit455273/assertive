#' Are the inputs identical
#' 
#' Generalisation of \code{identical} to an arbitrary number of inputs.
#' @param ... Some R expressions
#' @return A symmetric square logical matrix which is \code{TRUE} where pairs
#' of inputs are identical.
#' @seealso \code{\link[base]{identical}}
#' @examples
#' x <- 1:5
#' are_identical(cos(pi), -1, x, (2:6) - 1L)
#' assert_any_are_identical(cos(pi), -1, x, (2:6) - 1L)
#' \dontrun{
#' assert_all_are_identical(cos(pi), -1, x, (2:6) - 1L)
#' }
#' @export
are_identical <- function(...)
{
  envir <- parent.frame()
  inputs <- as.list(match.call())[-1]
  input_pairs <- expand.grid(expr1 = inputs, expr2 = inputs)
  identicality <- apply(
    input_pairs, 
    1, 
    function(row)
    {       
      with(
        row, 
        identical(
          eval(expr1, envir = envir),
          eval(expr2, envir = envir)
        )
      )
    }
  )
  matrix(
    identicality,
    nrow     = nargs(),
    dimnames = list(inputs, inputs) 
  )
}

#' Does the current call have an argument?
#'
#' Checks to see if the current call has an argument with 
#' the name given in the input.
#'
#' @param x Argument to check. 
#' @param fn Function to find the argument in.
#' @return \code{has_arg} reimplements \code{\link[methods]{hasArg}}, 
#' letting you choose the function to search in, and providing more
#' information on failure.  
#' @note There is currently no corresponding \code{assert_has_arg}
#' function, because evaluating in the correct call is hard.
#' @seealso \code{\link[methods]{hasArg}}.
#' @examples
#' has_arg(x, mean.default)
#' has_arg(y, mean.default)   
#' f <- function(...) has_arg(z)   
#' f(z = 123)
#' f(123)
#' @importFrom methods formalArgs
#' @export
has_arg <- function(x, fn = sys.function(sys.parent()))
{
  arg_name <- get_name_in_parent(x)
  formal_args_of_fn <- formalArgs(fn)
  if(!arg_name %in% formal_args_of_fn)
  {                             
    fn_name <- get_name_in_parent(fn)
    fail <- false(
      "%s is not an argument of %s", 
      sQuote(arg_name), 
      sQuote(fn_name)
    )
    if("..." %in% formal_args_of_fn)
    {
      dots_call <- eval(quote(substitute(list(...))), sys.parent())
      if(!arg_name %in% names(dots_call))
      {
         return(fail)
      }
    } else
    {
       return(fail)
    }
  }
  TRUE
}

#' Does the input have terms?
#'
#' Checks to see if the input has a terms component or attribute.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{has_terms} returns \code{TRUE} if the input has an 
#' element or an attribute named terms. \code{assert_has_terms} returns 
#' nothing but throws an error if \code{has_terms} is not \code{TRUE}.
#' @seealso \code{\link{terms}}.
#' @examples
#' assert_has_terms(lm(uptake ~ conc, CO2))
#' @export
has_terms <- function(x, .xname = get_name_in_parent(x))
{
  if(
    is.null(attr(x, "terms")) && 
    (is.atomic(x) || is.null(x$terms))
  )
  {
    return(false("%s has no terms component nor attribute.", .xname))
  }
  TRUE
}

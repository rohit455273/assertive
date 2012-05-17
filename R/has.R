#' @rdname has_names
#' @export
has_colnames <- function(x)
{
  colnamesx <- colnames(x)
  if(is.null(colnamesx)) return(false("Column names are NULL."))
  if(!any(nzchar(colnamesx))) return(false("Column names are all empty."))
  TRUE
} 

#' Does the input have rows/columns?
#'
#' Checks to see if the input has rows/columns.
#'
#' @param x Input to check.
#' @return \code{has_rows} and \code{has_cols} return \code{TRUE} if 
#' \code{nrow} and \code{ncol} respectively return a value that is 
#' non-null and positive.  The \code{assert_*} functions return nothing 
#' but throw an error if the corresponding \code{has_*} function returns
#' \code{FALSE}.
#' @seealso \code{\link{ncol}}.
#' @examples
#' assert_has_rows(data.frame(x = 1:10))
#' assert_has_cols(matrix())
#' @export
has_cols <- function(x)
{
  ncolx <- ncol(x)
  if(is.null(ncolx)) return(false("Number of columns is NULL."))
  if(ncolx == 0L) return(false("Number of columns is zero."))
  TRUE
} 

#' Does the input have dimensions?
#'
#' Checks to see if the input has dimensions.
#'
#' @param x Input to check.
#' @return \code{has_dims} returns\code{TRUE} if \code{dim} is non-null 
#' and has length at least 1.  \code{assert_has_dims} returns nothing but 
#' throws an error if \code{has_dims} is not \code{TRUE}.
#' @seealso \code{\link{dim}}.
#' @export
has_dims <- function(x)
{
  dimx <- dim(x)
  if(is.null(dimx)) return(false("Dimensions are NULL."))
  if(is_empty(dimx)) return(false("Dimensions are empty."))
  TRUE
}
              
#' @rdname has_names
#' @export
has_dimnames <- function(x)
{
  dimnamesx <- dimnames(x)
  if(is.null(dimnamesx)) return(false("Dimension names are NULL."))
  if(!any(nzchar(dimnamesx))) return(false("Dimension names are all empty."))
  TRUE
} 

#' @rdname has_duplicates
#' @export
has_no_duplicates <- function(x)
{
  if(anyDuplicated(x)) return(false("There are duplicates."))
  TRUE
} 

#' Does the input have duplicates?
#'
#' Checks to see if the input has duplicates.
#'
#' @param x Input to check.
#' @return \code{has_duplicates} returns \code{TRUE} if\code{anyDuplicated} 
#' is \code{TRUE}.  \code{assert_has_duplicates} returns nothing but 
#' throws an error if \code{has_duplicates} is not \code{TRUE}. 
#' \code{has_no_duplicates} is the negation of \code{has_duplicates}.
##' @seealso \code{\link{anyDuplicated}}.
#' @export
has_duplicates <- Negate(has_no_duplicates)

#' Does the input have names?
#'
#' Checks to see if the input has (row/column/dimension) names.
#'
#' @param x Input to check.
#' @return \code{has_names} returns \code{TRUE} if \code{names} is 
#' non-null and at least one column name is not \code{""}. 
#' \code{has_rownames}, \code{has_colnames} and \code{has_dimnames} work
#' in a similar fashion, checking the corresponding attributes.
#' \code{assert_has_names} returns nothing but throws an error if 
#' \code{has_names} is not \code{TRUE}.
#' name is not \code{""}.
#' @seealso \code{\link[base]{names}}, \code{\link[base]{rownames}}, \code{\link[base]{colnames}}, \code{\link[base]{dimnames}}.
#' @examples
#' assert_has_names(c(a = 1, 2))
#' dfr <- data.frame(x = 1:5)
#' assert_has_rownames(dfr)
#' assert_has_colnames(dfr)
#' assert_has_dimnames(dfr)
#' @export
has_names <- function(x)
{
  namesx <- names(x)
  if(is.null(namesx)) return(false("Names are NULL."))
  if(!any(nzchar(namesx))) return(false("Names are all empty."))
  TRUE
} 

#' @rdname has_names
#' @export
has_rownames <- function(x)
{
  rownamesx <- rownames(x)
  if(is.null(rownamesx)) return(false("Row names are NULL."))
  if(!any(nzchar(rownamesx))) return(false("Row names are all empty."))
  TRUE
} 

#' @rdname has_cols
#' @export
has_rows <- function(x)
{
  nrowx <- nrow(x)
  if(is.null(nrowx)) return(false("Number of rows is NULL."))  
  if(nrowx == 0L) return(false("Number of rows is zero."))
  TRUE
} 

#' Does the input have terms?
#'
#' Checks to see if the input has a terms component or attribute.
#'
#' @param x Input to check.
#' @return \code{has_terms} returns \code{TRUE} if the input has an 
#' element or an attribute named terms. \code{assert_has_terms} returns 
#' nothing but throws an error if \code{has_terms} is not \code{TRUE}.
#' @seealso \code{\link[stats]{terms.default}}.
#' @examples
#' assert_has_terms(lm(uptake ~ conc, CO2))
#' @export
has_terms <- function(x)
{
  if(is.null(x$terms) && is.null(attr(x, "terms")))
  {
    return(false("Input has no terms component nor attribute."))
  }
  TRUE
}

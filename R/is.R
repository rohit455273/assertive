#' Alternative version of is.
#' 
#' If a function named \code{is.class} exists, call \code{is.class(x)}.
#' If not, call \code{is(x, class)}.
#' @param x Input to check.
#' @param class Target class that \code{x} maybe belong to.
#' @param .xname Not intended to be used directly.
#' @return \code{TRUE} if x belongs to the class and \code{FALSE} 
#' otherwise.  \code{assert_is} returns nothing but throws an error if
#' \code{x} does not have class \code{class}.
#' @seealso \code{\link[methods]{is}}.
#' @examples
#' assert_is(1:10, "numeric")
#' #These examples should fail:
#' \dontrun{
#' assert_is(1:10, "list")
#' }
#' @export
is2 <- function(x, class, .xname = get_name_in_parent(x))
{  
  fn <- try(match.fun(paste0("is.", class)), silent = TRUE)
  condn <- if(inherits(fn, "try-error"))
  {
    is(x, class)
  } else
  {
    fn(x)
  }
  if(!condn)
  {
    return(false("%s is not of type '%s'.", .xname, class))
  }
  TRUE
}

#' @rdname is_logical
#' @export
is_a_bool <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_logical(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
}

#' @rdname is_complex
#' @export
is_a_complex <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_complex(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
}

#' @rdname is_character
#' @export
is_a_non_empty_string <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_a_string(x))) return(ok)
  if(!nzchar(x))
  {
    return(false("%s has no characters.", .xname))
  }
  TRUE
}

#' @rdname is_numeric
#' @export
is_a_number <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_numeric(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
} 

#' @rdname is_raw
#' @export
is_a_raw <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_raw(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
} 

#' @rdname is_character
#' @export
is_a_string <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_character(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
}

#' @rdname is_character
#' @export
is_an_empty_string <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_a_string(x, .xname))) return(ok)
  if(nzchar(x)) 
  {
    return(false("%s contains characters.", .xname))
  }
  TRUE
}

#' @rdname is_integer
#' @export
is_an_integer <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_integer(x, .xname))) return(ok)
  if(!(ok <- is_scalar(x, .xname))) return(ok)
  TRUE
} 

#' Is the input an array or matrix?
#'
#' Checks to see if the input is an array or matrix.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_array} and \code{is_matrix} wrap \code{is.array}, 
#' and \code{is.matrix} respectively, providing more information on
#' failure.  The \code{assert_*} functions return nothing but throw
#' an error if the corresponding \code{is_*} function returns
#' \code{FALSE}.
#' @examples
#' assert_is_array(array())
#' assert_is_array(matrix())
#' assert_is_matrix(matrix())
#' \dontrun{
#' #These examples should fail:
#' assert_is_matrix(array())
#' }
#' @export
is_array <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "array", .xname)
}

#' Is the input atomic/recursive/vector?
#'
#' Checks to see if the input is a type that is atomic/recursive/vector.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_atomic}, \code{is_recursive} and \code{is_vector} wrap 
#' \code{is.atomic}, \code{is.recursive} and \code{is.vector} respectively,
#' providing more information on failure.  The \code{assert_*} functions
#' return nothing but throw an error if the corresponding \code{is_*}
#' function returns \code{FALSE}.
#' @seealso \code{\link[base]{is.atomic}} and \code{\link[base]{is.recursive}}.
#' @examples
#' atomic_types <- list(
#'   logical(),
#'   integer(),
#'   numeric(), 
#'   complex(),
#'   character(), 
#'   raw(),
#'   matrix(), 
#'   array(),
#'   NULL
#' )
#' for(var in atomic_types) assert_is_atomic(var)
#' recursive_types <- list(
#'   list(), 
#'   expression(),
#'   data.frame(), 
#'   y ~ x,
#'   function(){},
#'   call("sin", "pi")
#' )
#' for(var in recursive_types) assert_is_recursive(var)
#' vector_types <- c(
#'   atomic_types[1:6], 
#'   recursive_types[1:2]
#' )
#' for(var in recursive_types) assert_is_vector(var)
#' @export
is_atomic <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.atomic(x))
  {
    return(false("%s is not atomic.", .xname))
  }
  TRUE
}

#' @rdname is_language
#' @export
is_call <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "call", .xname)
}

#' Is the input of type character?
#'
#' Checks to see if the input is of type character.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_character} wraps \code{is.character}, providing more 
#' information on failure. \code{is_a_string} returns \code{TRUE} if the 
#' input is character and scalar. \code{is_empty_string} returns \code{TRUE}
#' if the input is \code{""}.  \code{is_numeric_string} is vectorised, 
#' returning \code{TRUE} when the inputs are not \code{NA} after conversion
#' to character and then numeric. \code{is_missing_or_empty_string} is
#' also vectorised, returning \code{TRUE} when the input is \code{""} or
#' \code{NA}.
#' The \code{assert_*} functions return nothing but throw an error if the
#' corresponding \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link[base]{is.character}} and \code{\link{is_scalar}}.
#' @examples
#' assert_is_character(letters)
#' assert_is_a_string("foo bar baz")
#' assert_is_empty_string("")
#' assert_all_are_numeric_strings(c("1", "2.3", "-4.5", "6e7", "8E-9"))
#' assert_any_are_numeric_strings(c("1", "Not a number"))
#' assert_all_strings_are_missing_or_empty(c("", NA))
#' assert_any_strings_are_missing_or_empty(c("a", NA, "b"))
#' @export
is_character <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "character", .xname)
}

#' Is the input complex?
#'
#' Checks to see if the input is complex.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_complex} wraps \code{is.complex}, providing more 
#' information on failure. \code{is_a_bool} returns \code{TRUE} if the 
#' input is complex and scalar.  The \code{assert_*} functions return
#' nothing but throw an error if the corresponding \code{is_*} function
#' returns \code{FALSE}.
#' @seealso \code{\link[base]{is.complex}} and \code{\link{is_scalar}}.
#' @examples
#' assert_is_complex(c(1i, 2i))
#' assert_is_a_complex(1i)
#' assert_is_a_complex(1 + 0i)
#' assert_is_a_complex(NA_complex_)
#' \dontrun{
#' #These examples should fail:
#' assert_is_complex(1:10)
#' assert_is_a_complex(c(1i, 2i))
#' assert_is_a_complex(complex())
#' }
#' @export
is_complex <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "complex", .xname)
}       

#' Checks to see if the input is a data.frame.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_data.frame} wraps \code{is.data.frame}, 
#' providing more information on failure.  \code{assert_is_data.frame} 
#' returns nothing but throws an error if \code{is_data.frame} 
#' returns \code{FALSE}.
#' @seealso \code{\link[base]{is.data.frame}}.
#' @examples
#' assert_is_data.frame(data.frame())
#' assert_is_data.frame(datasets::CO2)
#' @export
is_data.frame <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "data.frame", .xname)
}

#' Is the input empty/scalar?
#'
#' Checks to see if the input has length zero/one.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_empty} returns \code{TRUE} if the input has length 
#' zero.  \code{is_scalar} returns \code{TRUE} if the input has length 
#' one.  The \code{assert_*} functions return nothing but throw an
#' error if the corresponding \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link{length}}.
#' @examples
#' assert_is_empty(NULL)
#' assert_is_empty(numeric())
#' assert_is_non_empty(1:10)
#' assert_is_non_empty(NA)
#' assert_is_scalar(1)
#' assert_is_scalar("Multiple words in a single string are scalar.")
#' assert_is_scalar(NA)
#' @export
is_empty <- function(x, .xname = get_name_in_parent(x))
{
  if(length(x) != 0L) 
  {
    return(false("%s has non-zero length.", .xname))
  }
  TRUE
}

#' Is the input the empty model?
#'
#' Checks to see if the input is the empty model.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_[non_]empty_model} returns \code{TRUE} if the input is an  
#' [non] empty model.  (\code{has_terms} is used to determine that a variable 
#' is a model object.)  The model is considered empty if there are no
#' factors and no intercept. The \code{assert_*} functions return nothing 
#' but throw an error if the corresponding \code{is_*} function returns
#' \code{FALSE}.
#' @seealso \code{\link[stats]{is.empty.model}} and \code{is_empty}.
#' @examples
#' assert_is_empty_model(lm(uptake ~ 0, CO2))
#' assert_is_non_empty_model(lm(uptake ~ conc, CO2))
#' assert_is_non_empty_model(lm(uptake ~ 1, CO2))
#' @export
is_empty_model <- function(x, .xname = get_name_in_parent(x))
{
  if(!has_terms(x)) 
  {
    return(false("%s has no terms, probably not a model.", .xname))
  }
  tt <- terms(x)
  if(length(attr(tt, "factors")) != 0L) 
  {
    return(false("%s has factors.", .xname))
  }
  if(attr(tt, "intercept") != 0L) 
  {
    return(false("%s has an intercept.", .xname))
  }
  TRUE
}

#' Is the input an environment?
#'
#' Checks to see if the input is an environment.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_environment} wraps \code{is.environment}, providing more 
#' information on failure.  \code{assert_is_environment} returns nothing
#' but throws an error if \code{is_environment} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.environment}}.
#' @examples
#' assert_is_environment(new.env())
#' assert_is_environment(globalenv())
#' assert_is_environment(baseenv())
#' @export
is_environment <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "environment", .xname)
}

#' @rdname is_language
#' @export
is_expression <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "expression", .xname)
}

#' Is the input an factor?
#'
#' Checks to see if the input is an factor.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_factor} wraps \code{is.factor}, providing more 
#' information on failure.  \code{assert_is_factor} returns nothing
#' but throws an error if \code{is_factor} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.factor}}.
#' @examples
#' assert_is_factor(factor(sample(letters, 10)))
#' @export
is_factor <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "factor", .xname)
}

#' @rdname is_true
#' @export
is_false <- function(x, allow_attributes = FALSE, .xname = get_name_in_parent(x))
{
  if(allow_attributes) attributes(x) <- NULL
  if(!identical(FALSE, x)) 
  {
    return(false("%s is not identical to FALSE.", .xname))
  }
  TRUE
}                  

#' Is the input a function?
#'
#' Checks to see if the input is a function.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_function}, \code{is_primitive} and \code{is_stepfun}
#' wrap \code{is.function}, \code{is.primitive} and \code{is.stepfun} 
#' repsectively, providing more information on failure.  The 
#' \code{assert_*} functions return nothing but throw an error if the
#' corresponding \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link[base]{is.function}}.
#' @examples
#' assert_is_function(sqrt)
#' assert_is_function(function(){})
#' @export
is_function <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "function", .xname)
}

# ' Is the input generic?
# '
# ' Checks to see if the input is a generic function.
# '
# ' @param x Input to check.
#' @param .xname Not intended to be used directly.
# ' @return \code{TRUE} if the input is a generic function. 
# ' \code{assert_is_generic} functions return nothing but throws an error
# ' if \code{is_generic} returns \code{FALSE}.
# ' @seealso \code{\link[methods]{GenericFunctions}}.
# ' @examples
# ' 
# ' @export
# is_generic <- function(x)
# {       
#   x <- use_first(x)  
#   if(!is.function(x)) return(false("Input is not a function"))  
#   fn_name <- get_name_in_parent(x)
#   if(fn_name %in% utils:::getKnownS3generics()) return(TRUE)      
#   where <- find(fn_name, mode = "function")
#   gen <- utils:::findGeneric(fn_name, envir = as.environment(where))
#   if(!nzchar(gen)) return(false("Input is not generic."))
#   TRUE
# }     

#' @rdname is_real
#' @export
is_imaginary <- function(x)
{
  Re(x) == 0
}
               
#' @rdname is_in_range
#' @export
is_in_closed_range <- function(x, lower = -Inf, upper = Inf)
{
  is_in_range(x, lower, upper, FALSE, FALSE)
}

#' @rdname is_in_range
#' @export
is_in_left_open_range <- function(x, lower = -Inf, upper = Inf)
{
  is_in_range(x, lower, upper, TRUE, FALSE)
}

#' @rdname is_in_range
#' @export
is_in_open_range <- function(x, lower = -Inf, upper = Inf)
{
  is_in_range(x, lower, upper, TRUE, TRUE)
}
   
#' Is the input in range?
#'
#' Checks to see if the input is within an numeric interval.
#'
#' @param x Input to check.
#' @param lower Lower bound for the interval.
#' @param upper Upper bound for the interval.
#' @param lower_is_strict If \code{TRUE}, the lower bound is open (strict) 
#' otherwise it is closed.
#' @param upper_is_strict If \code{TRUE}, the upper bound is open (strict)
#' otherwise it is closed.
#' @note \code{is_in_range} provides the most flexibility in determining
#' if values are within a numeric interval.  The other functions restrict
#' the input arguments for convience in common cases.  For example,
#' \code{is_percentage} forces the interval to be from 0 to 100.
#' @return The \code{is_*} function return \code{TRUE} if the input is 
#' within an interval.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @examples
#' assert_all_are_positive(1:10)
#' assert_all_are_non_negative(0:10)
#' assert_any_are_positive(c(-1, 1))
#' assert_all_are_percentages(c(0, 50, 100))
#' assert_all_are_proportions(c(0, 0.5, 1))
#' assert_all_are_in_left_open_range(1 + .Machine$double.eps, lower = 1)
#' @export
is_in_range <- function(x, lower = -Inf, upper = Inf, lower_is_strict = FALSE, upper_is_strict = FALSE)
{
  ok <- rep.int(TRUE, length(x))
  ok[x < lower] <- FALSE                     
  ok[x > upper] <- FALSE
  if(lower_is_strict) ok[x == lower] <- FALSE
  if(upper_is_strict) ok[x == upper] <- FALSE
  ok
}

#' @rdname is_in_range
#' @export
is_in_right_open_range <- function(x, lower = -Inf, upper = Inf)
{
  is_in_range(x, lower, upper, FALSE, TRUE)
}

#' Is the input an integer?
#'
#' Checks to see if the input is an integer.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_integer} wraps \code{is.integer}, providing more 
#' information on failure. \code{is_an_integer} returns \code{TRUE} if the 
#' input is an integer and scalar.  The \code{assert_*} functions return 
#' nothing but throw an error if the corresponding \code{is_*} function
#' returns \code{FALSE}.
#' @seealso \code{\link[base]{is.integer}} and \code{\link{is_scalar}}.
#' @examples
#' assert_is_integer(1L:10L)
#' assert_is_an_integer(99L)
#' \dontrun{
#' #These examples should fail:
#' assert_is_integer(1:10)
#' assert_is_an_integer(1L:10L)
#' assert_is_an_integer(integer())
#' }
#' @export
is_integer <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "integer", .xname)
}

#' Is the input a language object?
#'
#' Checks to see if the input is a language object.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_call}, \code{is_expression}, \code{is_language}, 
#' \code{is_name} and \code{is_symbol} wrap the corresponding \code{is.*}
#' functions, providing more information on failure.  The \code{assert_*}
#' functions return nothing but throw an error if the corresponding
#' \code{is_*} function returns \code{FALSE}.
#' @note \code{is_name} and \code{is_symbol} are different names for 
#' the same function.
#' @seealso \code{\link[base]{is.call}}, \code{\link[base]{is.expression}}
#' \code{\link[base]{is.language}} and \code{\link[base]{is.name}}.
#' @examples
#' a_call <- call("sin", "pi")
#' assert_is_call(a_call)
#' assert_is_language(a_call)
#' an_expression <- expression(sin(pi))
#' assert_is_expression(an_expression)
#' assert_is_language(an_expression)
#' a_name <- as.name("foo")
#' assert_is_name(a_name)
#' assert_is_language(a_name)
#' \dontrun{
#' #These tests should fail:
#' assert_is_language(function(){})
#' }
#' @export
is_language <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.language(x)) 
  {
    return(false("%s is not a language object (name, call or expression).", .xname))
  }
  TRUE
}

#' Is the input a (dendrogram) leaf?
#'
#' Checks to see if the input is a (dendrogram) leaf.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_leaf} reimplements \code{is.leaf}, providing more 
#' information on failure.
#' @seealso \code{\link[stats]{dendrogram}}.
#' @export
is_leaf <- function(x, .xname = get_name_in_parent(x))
{
  leaf <- attr(x, "leaf")
  if(is.null(leaf)) 
  {
    return(false("%s has no 'leaf' attribute.", .xname))
  }
  if(!(ok <- is_true(
    leaf, 
    TRUE,
    paste("The leaf attribute of", .xname)
  )))
  {
    return(ok)
  }
  TRUE
}

#' Is the input a list?
#'
#' Checks to see if the input is a list.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_list} wraps \code{is.list}, providing more 
#' information on failure.
#' @seealso \code{\link[base]{is.list}}.
#' @examples
#' assert_is_list(list(1,2,3))
#' \dontrun{
#' #These tests should fail:
#' assert_is_list(1:10)
#' }
#' @export
is_list <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "list", .xname)
}

#' Is the input DLL loaded?
#'
#' Checks to see if the input DLL (a.k.a. shared object) is loaded.
#'
#' @param x Input to check.
#' @param PACKAGE Passed to \code{is.loaded}.
#' @param type Passed to \code{is.loaded}.
#' @param .xname Not intended to be used directly.
#' @return \code{is_loaded} wraps \code{is.loaded}, providing more 
#' information on failure.
#' @seealso \code{\link[base]{is.loaded}}.
is_loaded <- function(x, PACKAGE = "", type = "", .xname = get_name_in_parent(x))
{
  if(!is.loaded(x, PACKAGE = PACKAGE, type = type))
  {
    return(false("%s is not loaded.", .xname))
  }
}

#' Is the input logical?
#'
#' Checks to see if the input is logical.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_logical} wraps \code{is.logical}, providing more 
#' information on failure. \code{is_a_bool} returns \code{TRUE} if the 
#' input is logical and scalar.  The \code{assert_*} functions return
#' nothing but throw an error if the corresponding \code{is_*} function
#' returns \code{FALSE}.
#' @seealso \code{\link[base]{is.logical}} and \code{\link{is_scalar}}.
#' @examples
#' assert_is_logical(runif(10) > 0.5)
#' assert_is_a_bool(TRUE)
#' assert_is_a_bool(NA)
#' \dontrun{
#' #These examples should fail:
#' assert_is_logical(1)
#' assert_is_a_bool(c(TRUE, FALSE))
#' assert_is_a_bool(logical())
#' }
#' @export
is_logical <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "logical", .xname)
}       

#' @rdname is_array
#' @export
is_matrix <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "matrix", .xname)
}

#' Is the input (not) NaN?
#'
#' Checks to see if the input is a number that is(n't) NaN.
#'
#' @param x Input to check.
#' @return \code{is_nan} wraps \code{is.nan}, coercing the input to
#' numeric if necessary.  \code{is_not_nan} works similarly, but returns
#' the negation.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns
#' \code{FALSE}.
#' @seealso \code{\link[base]{is.nan}}
#' @examples
#' assert_is_not_nan(1:10)
#' @export
is_nan <- function(x)
{
  x <- coerce_to(x, "numeric")
  is.nan(x)
}

#' @rdname is_language
#' @export
is_name <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "name", .xname)
}

#' @rdname is_in_range
#' @export
is_negative <- function(x)
{
  is_in_range(x, upper = 0, upper_is_strict = TRUE)
}

#' @rdname is_empty
#' @export
is_non_empty <- function(x, .xname = get_name_in_parent(x))
{
  if(length(x) == 0L) 
  {
    return(false("%s has length zero.", .xname))
  }
  TRUE
}

#' @rdname is_empty_model
#' @export
is_non_empty_model <- function(x, .xname = get_name_in_parent(x))
{
  if(!has_terms(x)) 
  {
    return(false("%s has no terms, is probably not a model.", .xname))
  }
  tt <- terms(x)
  if(length(attr(tt, "factors")) == 0L && attr(tt, "intercept") == 0L)  
  {
    return(false("%s is an empty model.", .xname))
  }
  TRUE
}

#' @rdname is_in_range
#' @export
is_non_negative <- function(x)
{
  is_in_range(x, 0)
}

#' @rdname is_in_range
#' @export
is_non_positive <- function(x)
{
  is_in_range(x, upper = 0)
}
  
#' Is the input present?
#'
#' Checks to see if the input is not NA.
#'
#' @param x Input to check.
#' @return \code{is_not_na} is the negation of \code{is.na}. 
#' \code{assert_is_not_na} returns nothing but throws an error if 
#' \code{is_not_na} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.na}}
#' @examples
#' assert_is_not_na(1:10)
#' @export
is_not_na <- function(x)
{
  !is.na(x)
}
  
#' @rdname is_nan
#' @export
is_not_nan <- function(x)
{
  x <- coerce_to(x, "numeric")
  !is.nan(x)
}

#' @rdname is_null
#' export
is_not_null <- function(x, .xname = get_name_in_parent(x))
{
  if(is.null(x))
  {
    return(false("%s is NULL.", .xname))
  }
  TRUE
}

#' Is the input (not) null?
#'
#' Checks to see if the input is (not) null.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_null} wraps \code{is.null}, providing more 
#' information on failure. \code{is_not_null} returns \code{TRUE} in
#' the opposite case.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{is.null}}.
#' @examples
#' assert_is_null(NULL)
#' assert_is_null(c())
#' assert_is_not_null(NA
#' @export
is_null <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.null(x))
  {
    return(false("%s is not NULL.", .xname))
  }
  TRUE
}

#' Is the input numeric?
#'
#' Checks to see if the input is numeric.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_numeric} wraps \code{is.numeric}, providing more 
#' information on failure. \code{is_a_number} returns \code{TRUE} if the 
#' input is numeric and scalar.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{is.numeric}} and \code{\link{is_scalar}}.
#' @examples
#' assert_is_numeric(1:10)
#' assert_is_a_number(pi)
#' assert_is_a_number(1L)
#' assert_is_a_number(NA_real_)
#' \dontrun{
#' #These examples should fail:
#' assert_is_numeric(c(TRUE, FALSE))
#' assert_is_a_number(1:10)
#' assert_is_a_number(numeric())
#' }
#' @export
is_numeric <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "numeric", .xname)
}

#' @rdname is_character
#' @export
is_numeric_string <- function(x)
{
  x <- coerce_to(x, "character")
  x <- suppressWarnings(as.numeric(x))
  is_not_na(x)
}

#' @rdname is_factor
#' @export
is_ordered <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_factor(x))) return(ok)
  if(!is.ordered(x))
  {
    return(false("%s is not an ordered factor.", .xname))
  }
  TRUE
}

#' @rdname is_in_range
#' @export
is_percentage <- function(x, lower_is_strict = FALSE, upper_is_strict = FALSE)
{
  is_in_range(x, 0, 100, lower_is_strict, upper_is_strict)
}

#' @rdname is_in_range
#' @export
is_positive <- function(x)
{
  is_in_range(x, 0, lower_is_strict = TRUE)
}

#' @rdname is_function
#' @export
is_primitive <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_function(x))) return(ok)
  if(!is.primitive(x))
  {
    return(false("%s is not a primitive function.", .xname))
  }
  TRUE
} 

#' @rdname is_in_range
#' @export
is_proportion <- function(x, lower_is_strict = FALSE, upper_is_strict = FALSE)
{
  is_in_range(x, 0, 1, lower_is_strict, upper_is_strict)
}

#' Is the input a QR decomposition of a matrix?
#'
#' Checks to see if the input is a QR decomposition of a matrix.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_qr} wraps \code{is.qr}, providing more 
#' information on failure.  \code{assert_is_qr} returns nothing but
#' throws an error if \code{is_qr} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.qr}}.
#' @examples
#' assert_is_qr(qr(matrix(rnorm(25), nrow = 5)))
#' @export
is_qr <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "qr", .xname)
}

#' Are you running R?
#'
#' Checks to see you are running R.
#'
#' @return \code{is_R} wraps \code{is.R}, providing more 
#' information on failure.  \code{assert_is_R} returns nothing but
#' throws an error if \code{is_R} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.R}}.
#' @examples
#' assert_is_R()
#' @export
is_R <- function()
{
  if(!is.R())
  {
    return(false("You are not running R."))
  } 
  TRUE
}

#' Is the input a raster?
#'
#' Checks to see if the input is a raster.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_raster} wraps \code{is.raster}, providing more 
#' information on failure. \code{is_a_raster} returns \code{TRUE} if the 
#' input is raster and scalar.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[grDevices]{is.raster}}.
#' @examples
#' m <- matrix(hcl(0, 80, seq(50, 80, 10)), nrow=4, ncol=5)
#' assert_is_raster(as.raster(m))
#' \dontrun{
#' #These examples should fail:
#' assert_is_raster(m)
#' }
#' @export
is_raster <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "raster", .xname)
}

#' Is the input raw?
#'
#' Checks to see if the input is raw.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_raw} wraps \code{is.raw}, providing more 
#' information on failure. \code{is_a_raw} returns \code{TRUE} if the 
#' input is raw and scalar.  The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{is.raw}} and \code{\link{is_scalar}}.
#' @examples
#' assert_is_raw(as.raw(1:10))
#' assert_is_a_raw(as.raw(255))
#' \dontrun{
#' #These examples should fail:
#' assert_is_raw(c(TRUE, FALSE))
#' assert_is_a_raw(as.raw(1:10))
#' assert_is_a_raw(raw())
#' }
#' @export
is_raw <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "raw", .xname)
}

#' Is the input real/imaginary?
#'
#' Checks to see if the input is real or imaginary.
#'
#' @param x Input to check.
#' @return \code{TRUE} if the input has imaginary component equal to zero.
#' The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{complex}}
#' @examples
#' assert_are_all_real(1:10)
#' assert_are_all_real(1:10 + 0i)
#' assert_any_all_real(c(1i, 0))
#' assert_are_all_imaginary(1:10 * 1i)
#' assert_any_all_imaginary(c(1i, 0))
#' @export
is_real <- function(x)
{
  Im(x) == 0
}

#' @rdname is_atomic
#' @export
is_recursive <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.recursive(x))
  {
    return(false("%s is not recursive.", .xname))
  }
  TRUE
}

#' @rdname is_empty
#' @export
is_scalar <- function(x, .xname = get_name_in_parent(x))
{
  if(length(x) != 1L)
  {
    return(false("%s does not have length one.", .xname))
  }
  TRUE
}                

#' @rdname is_function
#' @export
is_stepfun <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_function(x))) return(ok)
  if(!is.stepfun(x))
  {
    return(false("%s is not a step function.", .xname))
  }
  TRUE
} 

#' Is the input a symmetric matrix?
#'
#' Checks that the input is a symmetric matrix.
#' 
#' @param x Input to check.
#' @param tol Differences smaller than \code{tol} are not considered.
#' @param .xname Not intended to be used directly.
#' @param ... Passed to \code{all.equal}.
#' @return \code{TRUE} if the input is symmetrix (after coersion to be a matrix).
#' @examples
#' m <- diag(3); m[3, 1] <- 1e-100
#' assert_is_symmetric_matrix(m)
#' \dontrun{
#' assert_is_symmetric_matrix(m, tol = 0)
#'}
#' @export
is_symmetric_matrix <- function(x, tol = 100 * .Machine$double.eps, .xname = get_name_in_parent(x), ...)
{
  x <- coerce_to(x, "matrix")
  dimx <- dim(x)
  if(dimx[1L] != dimx[2L])
  {
    return(false("%s is not a square matrix.", .xname))
  }
  symmetry_test <- if(is.complex(x)) 
  {
    all.equal.numeric(x, Conj(t(x)), tolerance = tol, ...)
  } else 
  {
    all.equal(x, t(x), tolerance = tol, ...)
  }
  if(!is_true(symmetry_test))
  {
    return(false("%s is not a symmetric matrix.", .xname))
  }
  TRUE
}

#' Is the input a table?
#'
#' Checks to see if the input is a table.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_table} wraps \code{is.table}, providing more 
#' information on failure.  \code{assert_is_table} returns nothing but
#' throws an error if \code{is_table} returns \code{FALSE}.
#' @seealso \code{\link[base]{is.table}}.
#' @examples
#' assert_is_table(table(sample(letters, 100, replace = TRUE)))
#' @export
is_table <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "table", .xname)
}

#' @rdname is_character
#' @export
is_missing_or_empty_character <- function(x)
{ 
  x <- coerce_to(x, "character")
  !nzchar(x) | is.na(x)
}

#' @rdname is_character
#' @export
is_not_missing_nor_empty_character <- function(x)
{ 
  x <- coerce_to(x, "character")
  nzchar(x) & !is.na(x)
}

#' @rdname is_language
#' @export
is_symbol <- is_name

#' Is the input TRUE?
#' 
#' Checks to see if the input if \code{TRUE}.
#'
#' @param x Input to check.
#' @param allow_attributes If \code{TRUE}, a scalar value of \code{TRUE}
#' with attributes is allowed.
#' @param .xname Not intended to be used directly.
#' @return \code{TRUE} if the input is identical to \code{TRUE}.
#' The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link[base]{isTRUE}}.
#' @examples
#' assert_is_true(TRUE)
#' assert_is_false(FALSE)
#' assert_is_true(c(truth = TRUE), allow_attributes = TRUE)
#' assert_is_false(false("This has an attribute"), allow_attributes = TRUE)
#' \dontrun{
#' #These tests should fail:
#' assert_is_true(c(truth = TRUE))
#' assert_is_false(false("This has an attribute"))
#' }
#' @export
is_true <- function(x, allow_attributes = FALSE, .xname = get_name_in_parent(x))
{
  if(allow_attributes) attributes(x) <- NULL
  if(!isTRUE(x))
  {
    return(false("%s is not identical to TRUE.", .xname))
  }
  TRUE
}

#' Is the string a valid variable name?
#'
#' Checks strings to see if they are valid variable names.
#'
#' @param x Input to check.
#' @param allow_reserved If \code{TRUE} then "..." and "..1", "..2", etc. 
#' are considered valid.
#' @param allow_duplicates If \code{TRUE} then duplicated names are allowed.
#' @return \code{TRUE} if the input is a valid variable name.
#' The \code{assert_*} functions return nothing but
#' throw an error if the corresponding \code{is_*} function returns 
#' \code{FALSE}.
#' @seealso \code{\link{make.names}}.
#' @examples
#' assert_all_are_valid_variable_names(c("x", "y_y0.y", ".", "...", "..1"))
#' \dontrun{
#' #These examples should fail:
#' assert_all_are_valid_variable_names(c("...", "..1"), allow_reserved = FALSE) 
#' assert_all_are_valid_variable_names(c("x", "x"), allow_duplicates = FALSE)
#' }
#' @references
#' \url{http://4dpiecharts.com/2011/07/04/testing-for-valid-variable-names/}
#' @export
is_valid_variable_name <- function(x, allow_reserved = TRUE, allow_duplicates = TRUE)
{
  x <- coerce_to(x, "character")
  ok <- rep.int(TRUE, length(x))

  #is name too long?
  max_name_length <- if(getRversion() < "2.13.0") 256L else 10000L
  ok[nchar(x) > max_name_length] <- FALSE

  #is it a reserved variable, i.e.
  #an ellipsis or two dots then a number?
  if(!allow_reserved)
  {
    ok[x == "..."] <- FALSE
    ok[grepl("^\\.{2}[[:digit:]]+$", x)] <- FALSE
  }

  #are names valid (and maybe unique)
  ok[x != make.names(x, unique = !allow_duplicates)] <- FALSE

  ok
}

#' @rdname is_atomic
#' @export
is_vector <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.vector(x)) return(false("%s is not a vector.", .xname))
  TRUE
}                

#' Is the input a whole number?
#'
#' Checks that the (probably floating point) input is a whole number.
#' 
#' @param x Input to check.
#' @param tol Differences smaller than \code{tol} are not considered.
#' @note The term whole number is used to distinguish from integer in
#' that the input \code{x} need not have type \code{integer}.  In fact
#' it is expected that \code{x} will be \code{numeric}.
#' @return \code{TRUE} if the input is a whole number.
is_whole_number <- function(x, tol = 100 * .Machine$double.eps)
{
  x <- coerce_to(x, "numeric")
  abs(x - floor(x)) < tol
}

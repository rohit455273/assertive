#' Does the file exist?
#'
#' Checks to see if the input files exist.
#'
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @return \code{is_existing_file} wraps \code{file.exists}, showing
#' the names of the inputs in the answer.   \code{assert_is_existing_file} 
#' returns nothing but throws an error if \code{is_generic} returns
#' \code{FALSE}.
#' @seealso \code{\link[base]{file.exists}}.
#' @examples
#' assert_all_are_existing_files(c(dir(), "~", getwd()))
#' \dontrun{
#' assert_all_are_existing_files("not an existing file (probably)")
#' }
#' @export
is_existing_file <- function(x, .xname = get_name_in_parent(x))
{
  x <- coerce_to(x, "character")
  call_and_name(file.exists, x)
}

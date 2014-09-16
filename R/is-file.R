#' Is the path a directory?
#' Checks to see if the input path is a directory.
#' 
#' @param x File paths.
#' @return \code{is_dir} returns \code{TRUE} if and only if the input 
#' path is a directory that exists, as determined by \code{file.info}.
#' @examples
#' assert_all_are_dirs(R.home())
#' @export
is_dir <- function(x)
{  
  x <- coerce_to(x, "character")
  call_and_name(
    function(x) 
    {
      ok <- file.info(x)[["isdir"]]
      causes <- ifelse(
        is.na(ok),
        "nonexistent",
        ifelse(ok, "", "file")
      )
      ok <- is_true(ok) 
      cause(ok) <- causes
      unname(ok)
    }, 
    x
  )
}

#' Does the file exist?
#'
#' Checks to see if the input files exist.
#'
#' @param x Input to check.
#' @return \code{is_existing_file} wraps \code{file.exists}, showing
#' the names of the inputs in the answer.   \code{assert_is_existing_file} 
#' returns nothing but throws an error if \code{is_existing_file} returns
#' \code{FALSE}.
#' @seealso \code{\link[base]{file.exists}}.
#' @examples
#' assert_all_are_existing_files(dir())
#' \dontrun{
#' #These examples should fail.
#' assert_all_are_existing_files("not an existing file (probably)")
#' }
#' @export
is_existing_file <- function(x)
{
  x <- coerce_to(x, "character")
  call_and_name(
    function(x)
    {
      ok <- file.exists(x)
      cause(x) <- ifelse(ok, "", "nonexistent")
    }, 
    x
  )
}

#' Is the file accessible?
#'
#' Checks to see if the input files can be executed/read/written to.
#'
#' @param x Input to check.
#' @return \code{is_ex_file} wraps \code{file.access}, showing
#' the names of the inputs in the answer.   \code{assert_is_ex_file} 
#' returns nothing but throws an error if \code{is_ex_file} returns
#' \code{FALSE}.
#' @seealso \code{\link[base]{file.access}}.
#' @examples
#' \dontrun{
#' assert_all_are_readable_files(dir())
#' }
#' @export
is_ex_file <- function(x)
{
  warn_about_file.access_under_windows()
  x <- coerce_to(x, "character")
  call_and_name(
    function(x)
    {
      ok <- file.access(x, mode = 1) == 0L
      cause(ok) <- ifelse(
        ok, 
        "", 
        ifelse(file.exists(x, "unexecutable", "nonexistent"))
      )
    }, 
    x
  )
}

#' Is the directory a known R library?
#' 
#' Checks to see if the input directories are known R libraries.
#' 
#' @param x Directory paths
#' @note Input paths are converted to character, and then normalized using
#' \code{normalizePaths}.
#' @return \code{is_library} returns \code{TRUE} if and only if the input
#' paths are known R package libraries.  That is, they must be paths
#' returned by \code{.libPaths}.
#' @export
is_library <- function(x)
{
  x <- coerce_to(x, "character")
  call_and_name(
    function(x) 
    {
      std_x <- normalizePath(path.expand(x), winslash = "/", mustWork = FALSE)
      std_x %in% .libPaths()
    }, 
    x
  )
}

#' @rdname is_ex_file
#' @export
is_readable_file <- function(x)
{
  warn_about_file.access_under_windows()
  x <- coerce_to(x, "character")  
  call_and_name(
    function(x)
    {
      ok <- file.access(x, mode = 4) == 0L
      cause(ok) <- ifelse(
        ok, 
        "", 
        ifelse(file.exists(x, "unreadable", "nonexistent"))
      )
    }, 
    x
  )
}

#' @rdname is_ex_file
#' @export
is_writable_file <- function(x)
{
  warn_about_file.access_under_windows()
  x <- coerce_to(x, "character")
  call_and_name(
    function(x)
    {
      ok <- file.access(x, mode = 2) == 0L
      cause(ok) <- ifelse(
        ok, 
        "", 
        ifelse(file.exists(x, "unwritable", "nonexistent"))
      )
    }, 
    x
  )
}

#' Warn about file.access under Windows
#' 
#' 
warn_about_file.access_under_windows <- function()
{ 
  if(is_windows())
  {
    warning(
      "This function depends on file.access, which can give unexpected results under Windows."
    )
  }
}

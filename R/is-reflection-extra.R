# The smart implementation of this function uses rvest, but we don't want 
# the dependency, so use readLines + regex matching instead.
# get_current_r <- function(cran = getOption("repos", c(CRAN = "http://cran.r-project.org"))["CRAN"])
# {
#   doc <- rvest::html(paste(cran, "sources.html", sep = "/"))
#   # Version should be contained in the first link on this page
#   `%>%` <- rvest::`%>%`
#   version_string <- doc %>% 
#     rvest::html_node("li > a") %>%
#     rvest::html_text()
#   R_system_version(substring(version_string, 3, nchar(version_string) - 7))
# }

get_current_r <- function(cran = getOption("repos", c(CRAN = "http://cran.r-project.org"))["CRAN"])
{
  lines <- readLines(paste(cran, "sources.html", sep = "/"))
  rx <- "R-(\\d\\.\\d\\.\\d)"
  version_string <- regmatches(lines, regexpr(rx, lines))
  R_system_version(substring(version_string, 3))
}

#' Is this version of R up to date?
#' 
#' Check if this version of R is as new as the current release version of R.
#' @param cran A string giving the URL of the CRAN repository to check.
#' @return An object of class \code{R_system_version} giving the current release
#' version of R.
#' @note Development versions of R can have versions higher than the current
#' release version of R.  For convenience, these will return \code{TRUE}.
#' @examples
#' is_current_r()
is_current_r <- function(cran = getOption("repos", c(CRAN = "http://cran.r-project.org"))["CRAN"])
{
  this_version <- getRversion()
  current_version <- get_current_r(cran = cran)
  if(this_version < current_version)
  {
    return(
      false(
        "This version of R is %s but the current version is %s.", 
        this_version,
        current_version
      )
    )
  }
  TRUE
}

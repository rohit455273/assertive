library(RUnit)

# This script runs all the RUnit unit tests for the assertive package.
# To run tests from an installed version, change the mode variable to 
# 'installed_package'. If you have checked out a development version,
# change mode to 'devel' and set your working directory to the parent
# of the assertive package root.  To make the package build correctly
# change mode to 'build'.
mode <- "build"  #build or devel or installed_package
switch(
  mode,
  build = {
    # when building the package, the current working directory is 
    # assertive.Rcheck/tests
    r_files <- dir("../../assertive/R", pattern = "\\.R$", full.names = TRUE)
    invisible(lapply(r_files, sys.source, envir = globalenv()))
    test_dir <- "../../assertive/inst/tests"       
  },
  devel = 
  {
    # current working directory assumed to be parent of assertive package root
    # need to source contents of R dir
    source("assertive/tests/problems.R")
    r_files <- dir("assertive/R", pattern = "\\.R$", full.names = TRUE)
    invisible(lapply(r_files, sys.source, envir = globalenv()))
    test_dir <- "assertive/inst/tests"
  },
  installed_package = {
    # take files from existing package installation
    library(assertive)
    source("assertive/tests/problems.R")
    test_dir <- system.file("tests", package = "assertive") 
  },
  stop("Unknown development mode.")
)

ts <- defineTestSuite(
  "Test Assertive", 
  test_dir,
  testFileRegexp = "^test-"    
)

(results <- runTestSuite(ts, verbose = FALSE))
if(with(results[["Test Assertive"]], nErr + nFail) > 0) 
{
  if(mode == 'build')
  {
    stop("Units tests have thrown errors.")
  }
  bad_tests <- problems(results)
  View(bad_tests)
}




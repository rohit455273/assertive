library(RUnit)

is_devel <- TRUE
if(is_devel)
{
  source("assertive/tests/problems.R")
  r_files <- dir("assertive/R", pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(r_files, sys.source, envir = globalenv()))
#   load_code("assertive")
  test_dir <- "assertive/inst/tests"
} else
{
  library(assertive)
  source("assertive/tests/problems.R")
  test_dir <- system.file("tests", package = "assertive") 
}

ts <- defineTestSuite(
  "Test Assertive", 
  test_dir,
  testFileRegexp = "^test-"    
)

(results <- runTestSuite(ts, verbose = FALSE))
bad_tests <- problems(results)
if(nrow(bad_tests) > 0) View(bad_tests)




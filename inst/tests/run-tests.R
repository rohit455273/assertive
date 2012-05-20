library(RUnit)

setwd("d:/workspace/assertive") 
fnames <- c("internal", "utils", "is", "has", "assert-is", "assert-has")
for(f in fnames) source(paste0("R/", f, ".R"))
  


#ts <- defineTestSuite("assertive tests", "inst/test", "test-is-valid")
ts <- defineTestSuite("assertive tests", "inst/tests", "test-has")
res <- runTestSuite(ts)


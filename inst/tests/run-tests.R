library(RUnit)

setwd("c:/source/project/assertive/pkg/assertive")  
source("R/internal.r")                               
source("R/is.r")                               
source("R/has.r")
source("R/assert-is.r")
source("R/assert-has.r")   
source("R/warnings.r")


#ts <- defineTestSuite("assertive tests", "inst/test", "test-is-valid")
ts <- defineTestSuite("assertive tests", "inst/test", "test-is-valid-variable-name")
res <- runTestSuite(ts)
res

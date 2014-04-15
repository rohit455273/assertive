#' Find problematic RUnit tests
#' 
#' Finds RUnit tests that failed or threw an error.
#' @param x An object of class 'RUnitTestData'.
#' @return A data frame with columns
#' \describe{
#'  \item{suite}{The test suite that the problem occured in.}
#'  \item{file}{The file that the problem occured in.}
#'  \item{test}{The test that the problem occured in.}
#'  \item{kind}{Either 'error' or 'failure'.}
#'  \item{message}{The error message.}
#' }
problems <- function(x)
{
  if(!inherits(x, "RUnitTestData"))
  {
    stop("x is not an object of class 'RUnitTestData'.")
  }
  y <- do.call(
    rbind,
    Map(    
      function(suite, suitename)
      {
        do.call(
          rbind,
          Map(
            function(tests, filename)
            {
              do.call(
                rbind,
                Map(
                  function(test, testname)
                  {           
                    if(test$kind != "success")
                    {
                      data.frame(
                        suite   = suitename, 
                        file    = filename, 
                        test    = testname, 
                        kind    = test$kind,  
                        message = test$msg
                      )
                    }
                  },
                  tests,
                  names(tests)
                )
              )
            },
            suite$sourceFileResults,
            names(suite$sourceFileResults)
          )
        )
      },
      x,
      names(x)
    )
  )
  rownames(y) <- NULL
  y
}

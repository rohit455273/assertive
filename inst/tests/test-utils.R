test.coerce_to.numeric_vector_to_data_frame.returns_data_frame <- function()
{
  x <- 1:5
  expected <- data.frame(x = x)
  checkEquals(expected, coerce_to(x, "data.frame"))
}



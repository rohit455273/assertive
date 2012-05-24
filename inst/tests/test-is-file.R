test.is_existing_file.some_paths.returns_true_when_file_exists <- function()
{
  tf <- tempfile()
  file.create(tf)
  x <- c("~", getwd(), tf, "~not an existing file~")
  expected <- c(TRUE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_existing_file(x)
  )  
}

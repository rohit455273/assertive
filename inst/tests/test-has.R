test_has_colnames_data_frame_with_colnames_returns_true <- function()
{
  dfr <- data.frame(x = 1:5, y = runif(5))
  checkTrue(has_colnames(dfr))
}

test_has_colnames_matrix_with_colnames_returns_true <- function()
{
  mat <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
  checkTrue(has_colnames(mat))
}

test_has_colnames_matrix_without_colnames_returns_false <- function()
{
  mat <- matrix(1:12, nrow = 3)
  checkTrue(!has_colnames(mat))
}
  

test_has_dim_a_matrix_returns_true <- function()
{
  mat <- matrix(1:12, nrow = 3)
  checkTrue(has_dim(mat))
}
              
test_has_dim_a_data_frame_returns_true <- function()
{
  dfr <- data.frame(x = 1:5, y = runif(5))
  checkTrue(has_dim(dfr))
}
                                          
test_has_dim_a_vector_frame_returns_true <- function()
{
  x <- 1:3
  checkTrue(!has_dim(x))
}

#TODO:has_dimnames

test_has_names_named_vector_returns_true <- function()
{
  x <- c(foo = 1, 2, 3)
  checkTrue(has_names(x))
}
              
test_has_names_data_frame_returns_true <- function()
{
  dfr <- data.frame(x = 1:5, y = runif(5))
  checkTrue(has_names(dfr))
}
              
test_has_names_unnamed_vector_returns_false <- function()
{
  x <- 1:3
  checkTrue(!has_names(x))
}
              
#TODO:has_rownames

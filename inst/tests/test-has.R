test.has_colnames.data_frame_with_colnames.returns_true <- function()
{
  dfr <- data.frame(x = 1:5, y = runif(5))
  checkTrue(has_colnames(dfr))
}

test.has_colnames.matrix_with_colnames.returns_true <- function()
{
  mat <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
  checkTrue(has_colnames(mat))
}

test.has_colnames.matrix_without_colnames.returns_false <- function()
{
  mat <- matrix(1:12, nrow = 3)
  checkTrue(!has_colnames(mat))
}
  

test.has_dim.a_matrix.returns_true <- function()
{
  mat <- matrix(1:12, nrow = 3)
  checkTrue(has_dim(mat))
}
              
test.has_dim.a_data_frame.returns_true <- function()
{
  dfr <- data.frame(x = 1:5, y = runif(5))
  checkTrue(has_dim(dfr))
}
                                          
test.has_dim.a_vector_frame.returns_true <- function()
{
  x <- 1:3
  checkTrue(!has_dim(x))
}


test.has_names.named_vector.returns_true <- function()
{
  x <- c(foo = 1, 2, 3)
  checkTrue(has_names(x))
}
              
test.has_names.data_frame.returns_true <- function()
{
  dfr <- data.frame(x = 1:5, y = runif(5))
  checkTrue(has_names(dfr))
}
              
test.has_names.unnamed_vector.returns_false <- function()
{
  x <- 1:3
  checkTrue(!has_names(x))
}
       
test.is_array.an_array.returns_true <- function()
{
  checkTrue(is_array(array()))
}

test.is_array.a_matrix.returns_true <- function()
{
  checkTrue(is_array(matrix()))
}

test.is_array.a_data.frame.returns_false <- function()
{
  checkTrue(!is_array(data.frame(x = 1:5)))
}

test.is_array.a_vector.returns_false <- function()
{
  checkTrue(!is_array(1:10))
}

#'   logical(),
#'   integer(),
#'   numeric(), 
#'   complex(),
#'   character(), 
#'   raw(),
#'   matrix(), 
#'   array(),
#'   NULL
test.is_atomic.logical.returns_true <- function()
{
  checkTrue(is_atomic(logical()))
}

test.is_atomic.integer.returns_true <- function()
{
  checkTrue(is_atomic(integer()))
}

test.is_atomic.numeric.returns_true <- function()
{
  checkTrue(is_atomic(numeric()))
}

test.is_atomic.complex.returns_true <- function()
{
  checkTrue(is_atomic(complex()))
}

test.is_atomic.raw.returns_true <- function()
{
  checkTrue(is_atomic(raw()))
}

test.is_atomic.matrix.returns_true <- function()
{
  checkTrue(is_atomic(matrix()))
}

test.is_atomic.array.returns_true <- function()
{
  checkTrue(is_atomic(array()))
}

test.is_atomic.null.returns_true <- function()
{
  checkTrue(is_atomic(NULL()))
}

test.is_atomic.something_recursive.returns_false <- function()
{
  checkTrue(!is_atomic(list()))
}


test.is_call.a_call.returns_true <- function()
{
  checkTrue(is_call(call("sin", "pi")))
}

test.is_atomic.not_a_call.returns_false <- function()
{
  checkTrue(!is_call(expression(sin(pi)))))
}


test.is_character.character_vector.returns_true <- function()
{
  checkTrue(is_character(letters))
}

test.is_character.NA_character_.returns_true <- function()
{
  checkTrue(is_character(NA_character_)))
}

test.is_character.not_a_character_vector.returns_false <- function()
{
  checkTrue(!is_character(1:10)))
}


test.is_complex.1i.returns_true <- function()
{
  checkTrue(is_complex(1i))
}

test.is_complex.1.returns_false <- function()
{
  checkTrue(!is_complex(1L))
}

test.is_complex.1_plus_0i.returns_true <- function()
{
  checkTrue(is_complex(1 + 0i))
}

test.is_complex.na_complex_.returns_true <- function()
{
  checkTrue(is_complex(NA_complex_))
}


test.is_data.frame.a_data.frame.returns_true <- function()
{
  checkTrue(is_data.frame(data.frame(x = 1:5)))
}

test.is_data.frame.not_a_data.frame.returns_false <- function()
{
  checkTrue(!is_data.frame(list(x = 1:5)))
}


test.is_environment.an_environment.returns_true <- function()
{
  checkTrue(is_environment(new.env()))
}

test.is_environment.global_environment.returns_true <- function()
{
  checkTrue(is_environment(globalenv()))
}

test.is_environment.base_environment.returns_true <- function()
{
  checkTrue(is_environment(baseenv()))
}

test.is_environment.not_an_environment.returns_false <- function()
{
  checkTrue(!is_environment(list()))
}







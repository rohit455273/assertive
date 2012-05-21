test.is_in_range.1_in_0_to_2.returns_true <- function()
{
  checkTrue(is_in_range(1, 0, 2))
}

test.is_in_range.0_in_0_to_2.returns_true <- function()
{
  checkTrue(is_in_range(0, 0, 2))
}

test.is_in_range.0_in_0_to_2_with_strict_lower_bound.returns_false <- function()
{
  checkTrue(!is_in_range(0, 0, 2, lower_is_strict = TRUE))
}

test.is_in_range.2_in_0_to_2_with_strict_upper_bound.returns_false <- function()
{
  checkTrue(!is_in_range(2, 0, 2, upper_is_strict = TRUE))
}


test.is_negative.all_minus_1_to_minus_10.returns_true <- function()
{
  checkTrue(all(is_negative(-1:-10)))
}

test.is_negative.all_1_to_10.returns_false <- function()
{
  checkTrue(!all(is_negative(1:10)))
}


test.is_non_negative.all_0_to_10.returns_true <- function()
{
  checkTrue(all(is_non_negative(0:10)))
}

test.is_non_negative.all_minus_1_to_minus_10.returns_false <- function()
{
  checkTrue(!all(is_non_negative(-1:-10)))
}


test.is_non_positive.all_0_to_minus_10.returns_true <- function()
{
  checkTrue(all(is_non_positive(0:-10)))
}

test.is_non_positive.all_1_to_10.returns_false <- function()
{
  checkTrue(!all(is_non_positive(1:10)))
}


test.is_percentage.all_0_to_100.returns_true <- function()
{
  checkTrue(all(is_percentage(0:100)))
}

test.is_percentage.0_with_strict_lower_bound.returns_false <- function()
{
  checkTrue(!is_percentage(0, lower_is_strict = TRUE))
}

test.is_percentage.100_with_strict_upper_bound.returns_false <- function()
{
  checkTrue(!is_percentage(100, upper_is_strict = TRUE))
}


test.is_proportion.all_0_to_1.returns_true <- function()
{
  checkTrue(all(is_proportion(seq.int(0, 1, 0.1))))
}

test.is_proportion.0_with_strict_lower_bound.returns_false <- function()
{
  checkTrue(!is_proportion(0, lower_is_strict = TRUE))
}

test.is_proportion.1_with_strict_upper_bound.returns_false <- function()
{
  checkTrue(!is_proportion(1, upper_is_strict = TRUE))
}
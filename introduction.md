---
title: "Introduction"
author: "Richard Cotton"
date: '2015-07-07'
output: html_document
---

<!--
%\VignetteEngine{knitr::rmarkdown}
%\VignetteIndexEntry{1. Introduction}
-->



### *assert* functions

There are times when it is a good idea to check the state of your variables, to
ensure that they have the properties that you think they have.  For example,
if you have a count variable, you might want to check that it is numeric, that
all the values are non-negative, and that all the values are whole numbers.

Base-R has a function called `stopifnot` that lets you perform such checks.


```r
counts <- c(1, 2, 3, 4.5)
stopifnot(
  is.numeric(counts),
  all(counts >= 0),
  isTRUE(all.equal(counts, round(counts)))
)
```

```
## Error: isTRUE(all.equal(counts, round(counts))) is not TRUE
```

This is OK, but not that easy to read.  Worse, the error messages that it 
produces in the event of failure aren't very user-friendly.

`assertive` provides lots of *assert* functions that provide checks
for specific conditions.  (An *assertion* is software development jargon for a 
check.)  They are designed to make your code easier to read, and to return 
helpful error messages to users in the event of a check failing.

Here's the same example again, written in an `assertive` style.


```r
counts <- c(1, 2, 3, 4.5)
assert_is_numeric(counts)
assert_all_are_non_negative(counts)
assert_all_are_whole_numbers(counts)
```

```
## Error in eval(expr, envir, enclos): counts are not all whole numbers (tol = 2.22045e-14).
## There was 1 failure:
##   Position Value      Cause
## 1        4   4.5 fractional
```

Here you see that the error message contains a human readable sentence, followed
by information on the values that caused problems, along with their positions
and reasons for failure.

You can also use pipes, but this means that the variable name is changed to `.`.


```r
library(magrittr)
counts %>% 
  assert_is_numeric %>% 
  assert_all_are_non_negative %>% 
  assert_all_are_whole_numbers
```

```
## Error in function_list[[k]](value): . are not all whole numbers (tol = 2.22045e-14).
## There was 1 failure:
##   Position Value      Cause
## 1        4   4.5 fractional
```

### *is* and *has* functions

Each of the *assert* functions has an underlying *is* or *has* function.  For
example, `assert_is_numeric` calls `is_numeric`, `assert_all_are_non_negative`
calls `is_non_negative`, and so on.

Some *is* and *has* functions, such as `is_numeric`, return a single logical 
value.


```r
is_numeric(1:6)
```

```
## [1] TRUE
```

```r
is_numeric(letters)
```

```
## [1] FALSE
## Cause of failure:  letters is not of type 'numeric'; it has class 'character'.
```

When the check passed, `is_numeric` returned `TRUE`, and when it failed, 
`is_numeric` returned `FALSE` with a `cause` attribute explaining the problem.

Where *is* functions return a single value, they have a single corresponding 
*assert* function prefixed by `assert_`.  For example, `is_numeric` is paired 
with `assert_is_numeric`.

Some *is* and *has* functions, such as `is_non_negative`, return a logical 
vector.


```r
is_nn <- is_non_negative(rnorm(6))
unclass(is_nn)
```

```
##   1.85027386134187 -0.578688500050631  -1.47971295307092 
##               TRUE              FALSE              FALSE 
##    -0.133276736157 -0.232058794760384 0.0147176397242853 
##              FALSE              FALSE               TRUE 
## attr(,"cause")
## [1]         too low too low too low too low
```

For convenience, this has a pretty print method that shows you where the 
problems are (in the same way as the `assert_` function).  The actual value 
works in exactly the same way as a regular logical vector.

`is_non_negative` returned a logical vector which was `TRUE` where the check
passed, and `FALSE` where the check failed.  This time the `cause` attribute
was also vectorised, returning an empty string for the passes and a brief
explanation of the problem for the failures.

Where *is* functions return a vector, there are two corresponding *assert* 
functions, prefixed `assert_all_are_`, and `assert_any_are_`.  For example,
`is_non_negative` is paired with `assert_all_are_non_negative` and 
`assert_any_are_non_negative`.

### Summary

1. *assertive* contains `is*` functions that return logical values or vectors,
with a `cause` attribute the describes what went wrong when the return value
is not \code{TRUE}.
2. *assertive* also contains `assert*` functions that throw an error if a 
conditon is not met (and do nothing if the condition is met).
3. If a function `is_something` returns a single value, then the corresponding 
`assert*` function is named `assert_is_something`.
4. If a function `is_something` returns a vector, then the corresponding 
`assert*` functions are named `assert_all_are_something` and 
`assert_any_are_something`.

### Exercises.

1. List the contents of the *assertive* package using `ls("package:assertive")`.
[1 min].
2. Find a function that will throw an error if its input is not a vector.  Run 
the examples for that function Hint: The `pattern` argument to `ls` may be 
useful for narrowing your search. [2 mins].
3. Find a function that will throw an error if its input contains values that
aren't whole numbers.  Run the example.  [2 mins].
4. Find a function that returns a logical vector if its input contains US zip 
codes. Run the example.  [2 mins].



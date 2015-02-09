# assertive

Readable check functions to ensure code integrity.


### Installation

To install, you first need the devtools package.

```{r}
install.packages("devtools")
```

Then you can install the assertive package using

```{r}
library(devtools)
install_bitbucket("richierocks/assertive")
```

### How to use the package

assertive contains lots of *assert* functions that throw errors if conditions 
aren't met.  They are very useful for checking user input to your functions.

For example,

```{r}
f <- function(x)
{
  assert_is_not_null(x)
  x + 1
}
f(1)
## [1] 2
f(NULL)
## Error: x is NULL.
```

(You can think of the *assert* functions as more specific versions of 
`base::stopifnot` that make your code easier to read and give more informative 
error messages.)

Each *assert* function has a corresponding *is* function.  In this case, 
`is_not_null` is a wrapper to base-R's `!is.null`, that gives a more informative 
error message on failure (in an attribute named `cause`).

```{r}
is_not_null(1)
## [1] TRUE

is_not_null(NULL)
## [1] FALSE
## attr(,"cause")
## [1] NULL is NULL.
```

Many of the *is* functions are wrappers to base functions.  They all return causes
of failure, and they have consistent naming, beginning `is_` or `has_` (so 
`base::interactive` becomes `is_interactive`, for example.)



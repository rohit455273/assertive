#' @rdname are_identical
#' @export
assert_all_are_identical <- function(...)
{
  # Nasty reimplementation of functionality since assert_engine doesn't work
  # ... inputs right now.
  ok <- are_identical(...)
  if(!all(ok))
  {
    handler <- match.fun(
      match.arg(
        getOption("assertive.severity"), 
        c("stop", "warning", "message")
      )
    )
    handler(
      "The expressions ", 
      toString(as.list(match.call())[-1]), 
      " are not all identical.",
      call. = FALSE
    )
  }
}

#' @rdname are_identical
#' @export
assert_any_are_identical <- function(...)
{
  # Also nasty.
  ok <- are_identical(...)
  if(!any(ok))
  {
    handler <- match.fun(
      match.arg(
        getOption("assertive.severity"), 
        c("stop", "warning", "message")
      )
    )
    handler(
      "The expressions ", 
      toString(as.list(match.call())[-1]), 
      " are all not identical.",
      call. = FALSE
    )
  }
}

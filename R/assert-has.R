#' @rdname has_terms
#' @export
assert_has_terms <- function(x)
{                                                             
  msg <- gettextf("%s has no terms.", get_name_in_parent(x))
  assert_engine(has_terms, x, msg = msg)
}

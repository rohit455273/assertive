#' @rdname is_existing_file
#' @export
assert_all_are_existing_files <- function(x)
{
  msg <- sprintf(
    "The files %s do not all exist.", 
    get_name_in_parent(x)
  )
  assert_engine(x, is_existing_file, msg = msg)
}

#' @rdname is_existing_file
#' @export
assert_any_are_existing_files <- function(x)
{
  msg <- sprintf(
    "The files %s do not all exist.", 
    get_name_in_parent(x)
    )
  assert_engine(x, is_existing_file, msg = msg, what = "any")
}

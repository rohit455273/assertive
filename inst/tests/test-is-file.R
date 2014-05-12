test_that("test.is_dir.some_paths.returns_true_when_path_is_dir", {
  x <- c(R.home(), dir(R.home("bin"), full.names = TRUE))
  expected <- c(TRUE, logical(length(x) - 1))
  names(expected) <- x
  expect_equal(is_dir(x), expected)
})

test_that("test.is_ex_file.r_exes.returns_true", {
  x <- dir(R.home("bin"), "\\.exe$", full.names = TRUE)
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  expect_equal(is_ex_file(x), expected)
})

test_that("test.is_existing_file.some_paths.returns_true_when_file_exists", 
  {
    tf <- tempfile()
    file.create(tf)
    x <- c("~", getwd(), tf, "~not an existing file~")
    expected <- c(TRUE, TRUE, TRUE, FALSE)
    names(expected) <- x
    expect_equal(is_existing_file(x), expected)
  })

test_that("test.is_library.some_paths.returns_true_when_path_is_library", {
  x <- c(.libPaths(), "a made up directory")
  expected <- c(rep.int(TRUE, length(x) - 1), FALSE)
  names(expected) <- x
  expect_equal(is_library(x), expected)
})

test_that("test.is_readable_file.r_bin_files.returns_true", {
  x <- dir(R.home("bin"), full.names = TRUE)
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  expect_equal(is_readable_file(x), expected)
})

test_that("test.is_writable_file.tempfile.returns_true", {
  file.create(x <- tempfile())
  expected <- TRUE
  names(expected) <- x
  expect_equal(is_writable_file(x), expected)
  unlink(x)
}) 

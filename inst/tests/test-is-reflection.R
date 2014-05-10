test_that("test.is_64_bit_os.any_os.returns_pointer_size_equals_8", {
  expected <- .Machine$sizeof.pointer == 8
  expect_equal(is_64_bit_os(), expected)
})

test_that("test.is_batch_mode.any_mode.returns_true_if_called_from_batch_mode", 
  {
    expected <- !is.na(Sys.getenv("R_BATCH", NA))
    actual <- is_batch_mode()
    expect_equal(strip_attributes(actual), expected)
    if (!actual) {
      expect_equal(cause(actual), noquote("R is not running in batch mode."))
    }
  })

test_that("test.is_comma_for_decimal_point.any_locale.returns_true_if_locale_uses_comma", 
  {
    expected <- unname(Sys.localeconv()["mon_decimal_point"] == ",")
    actual <- is_comma_for_decimal_point()
    expect_equal(strip_attributes(actual), expected)
    if (!actual) {
      expect_equal(cause(actual), noquote("The locale convention is to use a '.' for a decimal point."))
    }
  })

test_that("test.is_interactive.any_mode.returns_true_if_r_runs_interactively", 
  {
    expected <- interactive()
    expect_equal(is_interactive(), expected)
  })

test_that("test.is_linux.any_mode.returns_true_if_os_is_linux", {
  expected <- unname(Sys.info()["sysname"] == "Linux")
  expect_equal(strip_attributes(is_linux()), expected)
})

test_that("test.is_mac.any_mode.returns_true_if_os_is_osx", {
  expected <- unname(Sys.info()["sysname"] == "Darwin")
  actual <- is_mac()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("The operating system is not OS X (or another Darwin-based OS)."))
  }
})

test_that("test.is_on_os_path.made_up_paths.returns_false_for_all", {
  paths <- c("a made up path", "path with bad chars !@#$%^&*(){}[]<>;:/?'")
  expect_false(any(is_on_os_path(paths)))
})

test_that("test.is_on_os_path.os_paths.returns_true_for_all", {
  paths <- strsplit(Sys.getenv("path"), ";")[[1]]
  expect_true(all(is_on_os_path(paths)))
})

test_that("test.is_period_for_decimal_point.any_locale.returns_true_if_locale_uses_period", 
  {
    expected <- unname(Sys.localeconv()["mon_decimal_point"] == ".")
    actual <- is_period_for_decimal_point()
    expect_equal(strip_attributes(actual), expected)
    if (!actual) {
      expect_equal(cause(actual), noquote("The locale convention is to use a ',' for a decimal point."))
    }
  })

test_that("test.is_r.r_or_s.returns_true_if_is_r", {
  expected <- exists("is.R") && is.function(is.R) && is.R()
  expect_equal(strip_attributes(is_r()), expected)
})

test_that("test.is_unix.any_os.returns_true_if_os_is_unix_based", {
  expected <- .Platform$OS.type == "unix"
  actual <- is_unix()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("The operating system is not Unix-based."))
  }
})

test_that("test.is_windows.any_os.returns_true_if_os_is_windows", {
  expected <- .Platform$OS.type == "windows"
  actual <- is_windows()
  expect_equal(strip_attributes(actual), expected)
  if (!actual) {
    expect_equal(cause(actual), noquote("The operating system is not Windows."))
  }
}) 

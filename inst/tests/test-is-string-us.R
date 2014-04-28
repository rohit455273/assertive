test_that("test.is_us_telephone_number.a_character_vector.returns_true_when_string_contains_a_valid_us_telephone_number", 
  {
    x <- c("12345678901", "+12345678901", "0012345678901", "2345678901", "10345678901", "11345678901", "12335678901", 
      "12340678901", "12341678901", "12345118901", "1234567", "12345678")
    expected <- rep.int(c(TRUE, FALSE), c(4, 8))
    names(expected) <- x
    expect_equal(is_us_telephone_number(x), expected)
  })

test_that("test.is_us_zip_code.5_digit_strings.returns_true_when_zip_code_has_valid_prefix", {
  unused <- c(0, 2:4, 99, 213, 269, 343, 345, 348, 353, 419, 428, 429, 517:519, 529, 533, 536, 552, 568, 578, 
    579, 589, 621, 632, 642, 643, 659, 663, 682, 694:699, 702, 709, 715, 732, 742, 771, 817, 818, 819, 839, 
    848, 849, 854, 858, 861, 862, 866:869, 876, 886:888, 892, 896, 899, 909, 929, 987)
  three_digits <- formatC(0:999, width = 3, flag = "0")
  expected <- !(0:999 %in% unused)
  x <- paste0(three_digits, "01")
  names(expected) <- x
  expect_equal(is_us_zip_code(x), expected)
})

test_that("test.is_us_zip_code.a_character_vector.returns_true_when_string_contains_a_valid_us_zip_code", {
  x <- c("22313", "22313-1450", "223130", "2231", "223131450", "2231E", " 22313")
  expected <- rep.int(c(TRUE, FALSE), c(2, 5))
  names(expected) <- x
  expect_equal(is_us_zip_code(x), expected)
})
 

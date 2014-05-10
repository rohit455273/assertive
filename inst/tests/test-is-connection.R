test_that("test.is_connection.a_connection.returns_true", {
  fcon <- file()
  on.exit(close(fcon))
  expect_true(is_connection(fcon))
})

test_that("test.is_connection.not_a_connection.returns_false", {
  expect_false(is_connection("not a connection"))
})

test_that("test.is_connection.std_connections.returns_true", {
  for (con in c(stdin, stdout, stderr)) {
    expect_true(is_connection(con()))
  }
})

test_that("test.is_incomplete_connection.a_closed_connection.returns_false", 
  {
    tcon <- textConnection("txt", "w", local = TRUE)
    close(tcon)
    expect_false(is_incomplete_connection(tcon))
  })

test_that("test.is_incomplete_connection.a_complete_connection.returns_false", 
  {
    tcon <- textConnection("txt", "w", local = TRUE)
    on.exit(close(tcon))
    cat("this has a final newline character\n", file = tcon)
    expect_false(is_incomplete_connection(tcon))
  })

test_that("test.is_incomplete_connection.an_incomplete_connection.returns_true", 
  {
    tcon <- textConnection("txt", "w", local = TRUE)
    on.exit(close(tcon))
    cat("this has no final newline character", file = tcon)
    expect_true(is_incomplete_connection(tcon))
  })

test_that("test.is_incomplete_connection.not_a_connection.returns_false", {
  expect_false(is_incomplete_connection("not a connection"))
})

test_that("test.is_open_connection.a_closed_connection.returns_false", {
  fcon <- file()
  close(fcon)
  expect_false(is_open_connection(fcon))
})

test_that("test.is_open_connection.an_open_readable_connection.returns_true", 
  {
    readable <- "r"
    file.create(tmp <- tempfile())
    fcon <- file(tmp, open = readable)
    on.exit({
      close(fcon)
      unlink(tmp)
    })
    expect_true(is_open_connection(fcon, readable))
  })

test_that("test.is_open_connection.an_open_writable_connection.returns_true", 
  {
    writable <- "w"
    file.create(tmp <- tempfile())
    fcon <- file(tmp, open = writable)
    on.exit({
      close(fcon)
      unlink(tmp)
    })
    expect_true(is_open_connection(fcon, writable))
  })

test_that("test.is_open_connection.not_a_connection.returns_false", {
  expect_false(is_open_connection("not a connection"))
}) 

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("control quoting", {
  expect_equal(strquote("foo"), '"foo"')
  expect_equal(strquote('foo "bar"'), "'foo \"bar\"'")
  expect_equal(strquote("foo 'bar'"), "\"foo 'bar'\"")
  expect_error(strquote("'foo' \"bar\""),
               "Can't quote this")
})

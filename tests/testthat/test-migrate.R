test_that("can migrate demo", {
  src <- orderly_demo_archive()
  msg <- testthat::capture_messages(dst <- orderly2outpack(src, tempfile()))
  expect_true(file.exists(dst))
  expect_equal(dir(dst, all.files = TRUE, no.. = TRUE), ".outpack")
  expect_true(file.exists(file.path(dst, ".outpack")))
  root <- outpack::outpack_root_open(dst)
  idx <- root$index()

  contents <- orderly::orderly_list_archive(src)
  expect_setequal(names(idx$metadata), contents$id)
})

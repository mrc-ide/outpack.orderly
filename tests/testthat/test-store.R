test_that("can create link-based file store", {
  tmp <- tempfile()
  obj <- file_store_link$new(tmp)
  expect_equal(obj$list(), character(0))
})


test_that("can add files to a link based store", {
  skip_on_os("windows")
  tmp <- tempfile()
  dir.create(tmp)
  p <- file.path(tmp, "a")
  saveRDS(runif(10), p)
  h <- outpack:::hash_file(p)
  obj <- file_store_link$new(tempfile())
  expect_equal(obj$put(p, h), h)
  expect_true(file.exists(obj$filename(h)))
  info <- fs::file_info(obj$filename(h))
  expect_identical(info$inode, fs::file_info(p)$inode)
  expect_equal(info$hard_links, 2)
})


test_that("prevent use of 'move' argument with link store", {
  skip_on_os("windows")
  tmp <- tempfile()
  dir.create(tmp)
  p <- file.path(tmp, "a")
  saveRDS(runif(10), p)
  h <- outpack:::hash_file(p)
  obj <- file_store_link$new(tempfile())
  expect_error((obj$put(p, h, TRUE), "Can't move files into a link store")
})

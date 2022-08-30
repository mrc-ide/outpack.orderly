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


test_that("migration destination must not yet exist", {
  src <- orderly_demo_archive()
  dest <- tempfile()
  dir.create(dest)
  expect_error(orderly2outpack(src, dest),
               "Destination already exists")
})


test_that("fail migrations if files have been modified", {
  src <- orderly_demo_archive()
  contents <- orderly::orderly_list_archive(src)
  name <- "use_resource"
  id <- contents$id[contents$name == name][[1]]
  path <- file.path(src, "archive", name, id, "meta", "data.csv")
  txt <- readLines(path)
  writeLines(txt[-length(txt)], path)
  expect_error(
    testthat::capture_messages(dst <- orderly2outpack(src, tempfile())),
    "Some hashes do not agree for use_resource/")
})


test_that("refuse to migrate incomplete graph", {
  src1 <- orderly_demo_archive()
  src2 <- tempfile()
  dir.create(src2)
  dir.create(file.path(src2, "global"))
  file.copy(file.path(src1, "orderly_config.yml"),
            file.path(src2, "orderly_config.yml"))
  r <- orderly::orderly_remote_path(src1)
  contents <- orderly::orderly_list_archive(src1)
  name <- "use_dependency"
  id <- contents$id[contents$name == name]
  for (i in id) {
    suppressMessages(orderly::orderly_pull_archive(name, i, root = src2,
                                                   remote = r,
                                                   recursive = FALSE))
  }
  expect_error(check_complete_tree(src2),
               "orderly graph is incomplete")
  expect_error(suppressMessages(orderly2outpack(src2, tempfile())),
               "orderly graph is incomplete")
})

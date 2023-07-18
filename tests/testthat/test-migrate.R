test_that("can migrate demo", {
  src <- orderly_demo_archive()
  msg <- testthat::capture_messages(dst <- orderly2outpack(src, tempfile()))
  expect_true(file.exists(dst))
  expect_equal(dir(dst, all.files = TRUE, no.. = TRUE), ".outpack")
  expect_true(file.exists(file.path(dst, ".outpack")))
  root <- orderly2::outpack_root_open(dst)
  idx <- root$index()

  contents <- orderly1::orderly_list_archive(src)
  expect_setequal(names(idx$metadata), contents$id)
})


test_that("migration destination must not have non-outpack files in", {
  src <- orderly_demo_archive()
  dest <- tempfile()
  dir.create(dest)
  file.create(file.path(dest, "anything"))
  expect_error(orderly2outpack(src, dest),
               "Destination directory is not a bare outpack destination")

  empty_dir <- file.path(dest, "empty")
  dir.create(empty_dir)
  suppressMessages(orderly2outpack(src, empty_dir))
})


test_that("notify migrations if files have been modified, but continue", {
  src <- orderly_demo_archive()
  contents <- orderly1::orderly_list_archive(src)
  name <- "use_resource"
  id <- contents$id[contents$name == name][[1]]
  path <- file.path(src, "archive", name, id, "meta", "data.csv")
  txt <- readLines(path)
  writeLines(txt[-length(txt)], path)
  res <- testthat::evaluate_promise(
    orderly2outpack(src, tempfile()))
  expect_match(
    res$messages,
    "Some hashes do not agree for use_resource/.*meta/data.csv",
    all = FALSE)
  expect_true(file.exists(res$result))

  root <- orderly2::outpack_root_open(res$result)
  expect_setequal(names(root$index()$metadata), contents$id)
})


test_that("refuse to migrate incomplete graph", {
  src1 <- orderly_demo_archive()
  src2 <- tempfile()
  dir.create(src2)
  dir.create(file.path(src2, "global"))
  file.copy(file.path(src1, "orderly_config.yml"),
            file.path(src2, "orderly_config.yml"))
  r <- orderly1::orderly_remote_path(src1)
  contents <- orderly1::orderly_list_archive(src1)
  name <- "use_dependency"
  id <- contents$id[contents$name == name]
  for (i in id) {
    suppressMessages(orderly1::orderly_pull_archive(name, i, root = src2,
                                                    remote = r,
                                                    recursive = FALSE))
  }
  expect_error(check_complete_tree(src2),
               "orderly graph is incomplete")
  expect_error(suppressMessages(orderly2outpack(src2, tempfile())),
               "orderly graph is incomplete")
})


test_that("test weird special cases", {
  src <- orderly_demo_archive()
  cmp <- suppressMessages(orderly2outpack(src, tempfile()))
  contents <- orderly1::orderly_list_archive(src)

  ## 1: missing dependency index
  id1 <- contents$id[contents$name == "use_dependency"][[1]]
  path <- file.path(src, "archive", "use_dependency", id1, "orderly_run.rds")
  data <- readRDS(path)
  data$meta$depends$index <- NULL
  saveRDS(data, path)

  ## 2: data frame parameters
  id2 <- contents$id[contents$name == "other"][[1]]
  path <- file.path(src, "archive", "other", id2, "orderly_run.rds")
  data <- readRDS(path)
  data$meta$parameters <- as.data.frame(data$meta$parameters)
  saveRDS(data, path)

  ## Do the migration
  res <- suppressMessages(orderly2outpack(src, tempfile()))

  ## Check everything is the same
  root_cmp <- orderly2::outpack_root_open(cmp)
  root_res <- orderly2::outpack_root_open(res)

  expect_identical(root_cmp$metadata(id1, full = TRUE),
                   root_res$metadata(id1, full = TRUE))
  expect_identical(root_cmp$metadata(id2, full = TRUE),
                   root_res$metadata(id2, full = TRUE))
})


test_that("can create link-based file store", {
  src <- orderly_demo_archive()
  dst <- suppressMessages(orderly2outpack(src, tempfile(), link = TRUE))

  path_files <- file.path(dst, ".outpack", "files")
  files <- dir(path_files, recursive = TRUE, include.dirs = FALSE,
               full.names = TRUE)
  info <- fs::file_info(files)
  expect_true(all(info$hard_links == 2))
})


test_that("can update archive", {
  src <- orderly_demo_archive()
  dst <- suppressMessages(orderly2outpack(src, tempfile(), link = TRUE))

  id <- suppressMessages(
    orderly1::orderly_run("minimal", root = src, echo = FALSE))
  suppressMessages(orderly1::orderly_commit(id, root = src))

  root <- orderly2::outpack_root_open(dst, locate = FALSE)

  expect_false(id %in% names(root$index(TRUE)$metadata))

  expect_equal(
    suppressMessages(orderly2outpack(src, dst, link = TRUE)),
    dst)

  expect_true(id %in% names(root$index(TRUE)$metadata))
  expect_true(id %in% root$index(TRUE)$unpacked)
})

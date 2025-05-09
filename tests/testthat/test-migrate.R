test_that("can migrate demo", {
  src <- orderly_demo_archive()
  msg <- testthat::capture_messages(dst <- orderly2outpack(src, tempfile()))
  expect_true(file.exists(dst))
  expect_equal(dir(dst, all.files = TRUE, no.. = TRUE),
               c(".outpack", "orderly_config.yml"))
  expect_true(file.exists(file.path(dst, ".outpack")))

  ids <- orderly2::orderly_search(NULL, location = "local", root = dst)
  expect_setequal(ids, orderly1::orderly_list_archive(src)$id)
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
  dst <- tempfile()
  res <- testthat::evaluate_promise(
    orderly2outpack(src, dst))
  expect_match(
    res$messages,
    "Some hashes do not agree for use_resource/.*meta/data.csv",
    all = FALSE)
  expect_true(file.exists(res$result))

  ids <- orderly2::orderly_search(NULL, location = "local", root = dst)
  expect_setequal(ids, contents$id)
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
  expect_identical(orderly2::orderly_metadata(id1, cmp),
                   orderly2::orderly_metadata(id1, res))
  expect_identical(orderly2::orderly_metadata(id2, cmp),
                   orderly2::orderly_metadata(id2, res))
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

  ids <- orderly2::orderly_search(NULL, location = "local", root = dst)
  expect_false(id %in% ids)

  expect_equal(
    suppressMessages(orderly2outpack(src, dst, link = TRUE)),
    dst)

  ids <- orderly2::orderly_search(NULL, location = "local", root = dst)
  expect_true(id %in% ids)
})


test_that("dependency migration", {
  depends <- data.frame(
    id = "a",
    index = 1,
    name = "foo",
    id_requested = "latest",
    as = "here.csv",
    filename = "there.csv")
  expect_equal(
    archive_migrate_depends(depends, NULL),
    list(list(packet = "a",
              query = 'latest(name == "foo")',
              files = data_frame(here = "here.csv", there = "there.csv"))))
})


test_that("dependency migration with fixed id", {
  depends <- data.frame(
    id = "a",
    index = 1,
    name = "foo",
    id_requested = "20240202-111943-fa53e980",
    as = "here.csv",
    filename = "there.csv")
  expect_equal(
    archive_migrate_depends(depends, NULL),
    list(list(packet = "a",
              query = "20240202-111943-fa53e980",
              files = data_frame(here = "here.csv", there = "there.csv"))))
})


test_that("dependency migration with fancy query", {
  depends <- data.frame(
    id = "a",
    index = 1,
    name = "foo",
    id_requested = "latest(x == parameter:y)",
    as = "here.csv",
    filename = "there.csv")
  parameters <- list(x = 1)
  expect_equal(
    archive_migrate_depends(depends, "x"),
    list(list(packet = "a",
              query = 'latest(this:x == parameter:y && name == "foo")',
              files = data_frame(here = "here.csv", there = "there.csv"))))
})


test_that("can migrate when archive has more than 1 dependency from 1 report", {
  src <- orderly_demo_archive()
  multi_dependency_yml <- list(script = "script.R",
                               depends = list(
                                 "multi-artefact" = list(
                                   id = "latest",
                                   use = list("all.csv" = "all.csv",
                                              "subset.csv" = "subset.csv")
                                 )
                               ),
                               artefacts = list(list(
                                 data = list(
                                   description = "some data",
                                   filenames = "rows.rds"
                                 )
                               )),
                               requester = "Joe Bloggs",
                               author = "Joe Bloggs")
  multi_dependency_script <- c("x <- read.csv('all.csv')",
                               "y <- read.csv('subset.csv')",
                               "z <- nrow(x) + nrow(y)",
                               "saveRDS(z, 'rows.rds')")
  multi_dependency <- file.path(src, "src", "multi-dependency")
  dir.create(multi_dependency)
  writeLines(multi_dependency_script, file.path(multi_dependency, "script.R"))
  yaml::write_yaml(multi_dependency_yml,
                   file.path(multi_dependency, "orderly.yml"))
  x <- orderly1::orderly_run("multi-dependency", root = src)
  orderly1::orderly_commit(x, root = src)

  msg <- testthat::capture_messages(dst <- orderly2outpack(src, tempfile()))
  expect_true(file.exists(dst))
  expect_equal(dir(dst, all.files = TRUE, no.. = TRUE),
               c(".outpack", "orderly_config.yml"))
  expect_true(file.exists(file.path(dst, ".outpack")))

  ids <- orderly2::orderly_search(NULL, location = "local", root = dst)
  expect_setequal(ids, orderly1::orderly_list_archive(src)$id)
})


test_that("cope nicely with migration failure", {
  src <- orderly_demo_archive()

  contents <- orderly1::orderly_list_archive(src)
  id1 <- contents$id[contents$name == "use_dependency"][[1]]
  meta <- readRDS(
    file.path(src, "archive", "use_dependency", id1, "orderly_run.rds"))
  id2 <- meta$meta$depends$id
  unlink(file.path(src, "archive", "other", id2, "README.md"))

  expect_error(
    orderly2outpack(src, tempfile()),
    "Metadata migration failure for 1/21 packets")

  dst <- orderly2outpack(src, tempfile(), keep_going = TRUE)
  expect_equal(readLines(file.path(dst, ".outpack/import_skipped_ids")),
               c(id2, id1))
  d <- readRDS(file.path(dst, ".outpack/import_skipped.rds"))
  expect_false(d[[1]]$success)
  expect_true(d[[2]]$success)
  expect_s3_class(d[[1]]$error, "error")
})


test_that("can migrate git data", {
  expect_null(migrate_git(list()))
  expect_equal(
    migrate_git(list(git = list(sha = "abc123"))),
    list(sha = "abc123",
         branch = NULL,
         url = NULL))
  expect_equal(
    migrate_git(list(git = list(sha = "abc123", branch = "main", other = 1))),
    list(sha = "abc123",
         branch = "main",
         url = NULL))
  expect_equal(
    migrate_git(list(git = list(
      sha = "abc123", branch = "main",
      github_url = "https://github.com/example/repo"))),
    list(sha = "abc123",
         branch = "main",
         url = "https://github.com/example/repo"))
})

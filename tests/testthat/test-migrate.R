test_that("can migrate demo", {
  src <- orderly_demo_archive()
  msg <- testthat::capture_messages(dst <- orderly2outpack(src, tempfile()))
  expect_true(file.exists(dst))
  expect_equal(dir(dst, all.files = TRUE, no.. = TRUE),
               c(".outpack", "orderly_config.yml"))
  expect_true(file.exists(file.path(dst, ".outpack")))

  ids <- orderly::orderly_search(NULL, location = "local", root = dst)
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

  ids <- orderly::orderly_search(NULL, location = "local", root = dst)
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
  expect_identical(orderly::orderly_metadata(id1, cmp),
                   orderly::orderly_metadata(id1, res))
  expect_identical(orderly::orderly_metadata(id2, cmp),
                   orderly::orderly_metadata(id2, res))
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

  ids <- orderly::orderly_search(NULL, location = "local", root = dst)
  expect_false(id %in% ids)

  expect_equal(
    suppressMessages(orderly2outpack(src, dst, link = TRUE)),
    dst)

  ids <- orderly::orderly_search(NULL, location = "local", root = dst)
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
  suppressMessages({
    x <- orderly1::orderly_run("multi-dependency", root = src, echo = FALSE)
    orderly1::orderly_commit(x, root = src)
  })

  msg <- testthat::capture_messages(dst <- orderly2outpack(src, tempfile()))
  expect_true(file.exists(dst))
  expect_equal(dir(dst, all.files = TRUE, no.. = TRUE),
               c(".outpack", "orderly_config.yml"))
  expect_true(file.exists(file.path(dst, ".outpack")))

  ids <- orderly::orderly_search(NULL, location = "local", root = dst)
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
    suppressMessages(orderly2outpack(src, tempfile())),
    "Metadata migration failure for 1/21 packets")

  dst <- suppressMessages(orderly2outpack(src, tempfile(), keep_going = TRUE))
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



test_that("can collapse filenames", {
  skip_on_os(c("windows", "mac"))
  tmp <- withr::local_tempdir()
  writeLines("hello", file.path(tmp, "README.md"))
  writeLines("hello", file.path(tmp, "readme.md"))

  expect_null(check_files_remap(c("a", "b"), tmp))
  expect_null(check_files_remap(c("a", "b", "README.md"), tmp))
  expect_equal(
    check_files_remap(c("a", "b", "README.md", "readme.md"), tmp),
    data.frame(from = "readme.md", to = "README.md"))

  writeLines("", file.path(tmp, "readme.md"))
  expect_message(
    res <- check_files_remap(c("a", "b", "README.md", "readme.md"), tmp),
    "but the content is different")
  expect_equal(res, data.frame(from = "readme.md", to = "README.md"))
})


test_that("can apply a remap over a vector of filenames", {
  remap <- data.frame(from = "readme.md", to = "README.md")
  expect_equal(
    apply_files_remap(c("a", "b", "readme.md", "c", "README.md"), remap),
    c("a", "b", "README.md", "c", "README.md"))
})


test_that("can collapse filenames when migrating metadata", {
  skip_on_os(c("windows", "mac"))

  src <- withr::local_tempdir()
  suppressMessages({
    orderly1::orderly_init(src)
    orderly1::orderly_new("example", root = src)
  })

  writeLines('file.copy("file.md", "FILE.md")',
             file.path(src, "src", "example", "script.R"))
  writeLines("hello", file.path(src, "src", "example", "file.md"))
  writeLines("script: script.R
artefacts:
  - staticgraph:
      description: Final FILE
      filenames: FILE.md
resources:
  - file.md", file.path(src, "src", "example", "orderly.yml"))
  suppressMessages(
    id <- orderly1::orderly_run("example", root = src, echo = FALSE))
  suppressMessages(orderly1::orderly_commit(id, root = src))

  dst <- withr::local_tempdir()
  msg <- testthat::capture_messages(dst <- orderly2outpack(src, dst))

  meta <- orderly::orderly_metadata(id, root = dst)
  expect_equal(nrow(meta$files), 3)

  ## Locale will affect the case selected here, so be defensive:
  file_md <- setdiff(meta$files$path, c("orderly.yml", "script.R"))
  expect_true(length(file_md) == 1)
  expect_true(file_md %in% c("FILE.md", "file.md"))
  expect_setequal(meta$files$path, c("orderly.yml", "script.R", file_md))

  expect_equal(meta$custom$orderly$role,
               data.frame(path = c("orderly.yml", "script.R", file_md),
                          role = rep("resource", 3)))
  expect_equal(meta$custom$orderly$artefact$paths[[1]], list(file_md))
})

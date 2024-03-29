test_that("Can migrate orderly demo directory", {
  path <- orderly_demo_src()
  suppressMessages(orderly2outpack_src(path, delete_yml = TRUE, strict = TRUE))

  dat <- orderly1:::read_demo_yml(path)
  for (i in seq_along(dat)) {
    x <- dat[[i]]
    if (!is.null(x$before)) {
      withr::with_dir(path, x$before())
    }
    env <- new.env(parent = .GlobalEnv)
    expect_no_error(suppressMessages(
      dat[[i]]$id <- orderly2::orderly_run(x$name, x$parameters, envir = env,
                                           root = path, echo = FALSE)))
  }
})


test_that("refuse to migrate a directory that does not conain orderly.yml", {
  path <- withr::local_tempdir()
  writeLines("minimum_orderly_version: 1.7.0",
             file.path(path, "orderly_config.yml"))
  expect_error(orderly2outpack_src(path),
               "Did not find any src directories containing 'orderly.yml'")
})


test_that("refuse to migrate directories containing both old and new source", {
  path <- orderly_demo_src()
  dir.create(file.path(path, "src", "new"))
  file.create(file.path(path, "src", "new", "orderly.R"))
  expect_error(
    orderly2outpack_src(path),
    "Some source directories already contain 'orderly.R' files: new")
})


test_that("can preserve original files after migration", {
  path1 <- orderly_demo_src()
  path2 <- orderly_demo_src()

  ## Files we'll keep
  nms <- orderly1::orderly_list(path1)
  keep <- c("orderly_config.yml",
            file.path("src", nms, "orderly.yml"),
            file.path("src", nms, "script.R"))
  kept <- c("orderly_config.yml.orig", keep[-1])
  h1 <- withr::with_dir(path1, tools::md5sum(sub("\\.orig$", "", keep)))

  suppressMessages(orderly2outpack_src(path1, delete_yml = TRUE))
  suppressMessages(orderly2outpack_src(path2, delete_yml = FALSE))
  files1 <- dir(path1, recursive = TRUE, all.files = TRUE, no.. = TRUE,
                include.dirs = FALSE)
  files2 <- dir(path2, recursive = TRUE, all.files = TRUE, no.. = TRUE,
                include.dirs = FALSE)
  expect_equal(setdiff(files1, files2), character())
  expect_setequal(setdiff(files2, files1), kept)

  expect_equal(unname(withr::with_dir(path2, tools::md5sum(kept))),
               unname(h1))
})


test_that("can add strict mode", {
  path <- orderly_demo_src()
  nms <- orderly1::orderly_list(path)
  suppressMessages(orderly2outpack_src(path, delete_yml = TRUE, strict = TRUE))
  str <- vapply(file.path(path, "src", nms, "orderly.R"),
                function(p) readLines(p, n = 1), "",
                USE.NAMES = FALSE)
  expect_equal(str, rep("orderly2::orderly_strict_mode()", length(nms)))
})


test_that("can not add strict mode", {
  path <- orderly_demo_src()
  nms <- orderly1::orderly_list(path)
  suppressMessages(orderly2outpack_src(path, delete_yml = TRUE, strict = FALSE))
  str <- "orderly2::orderly_strict_mode()"
  expect_false(
    any(vapply(file.path(path, "src", nms, "orderly.R"),
               function(p) any(grepl(str, readLines(p), fixed = TRUE)),
               TRUE)))
})


test_that("can migrate empty dependencies", {
  expect_null(src_migrate_depends(list(), list()))
})


test_that("can migrate single, simple, dependency", {
  depends <- data.frame(
    id = "latest",
    index = 1,
    name = "name",
    draft = NA,
    filename = "filename.csv",
    as = "incoming.csv",
    is_pinned = FALSE)
  expect_equal(
    src_migrate_depends(list(), list(depends = depends)),
    paste('orderly2::orderly_dependency("name", "latest",',
          'c(incoming.csv = "filename.csv"))'))
})


test_that("can migrate dependency with long string and quotes", {
  depends <- data.frame(
    id = paste('latest(parameter:some_name == "some_parameter" &&',
               'parameter:another == "another" &&',
               'parameter:a_third == "another one!")'),
    index = 1,
    name = "name",
    draft = NA,
    filename = "filename.csv",
    as = "incoming.csv",
    is_pinned = FALSE)
  cfg <- list(parameters = c("some_name", "another", "a_third"))
  res <- src_migrate_depends(list(), list(depends = depends))
  cmp <- paste('orderly2::orderly_dependency("name",',
               '\'latest(parameter:some_name == "some_parameter" &&',
               'parameter:another == "another" &&',
               'parameter:a_third == "another one!")\',',
               'c(incoming.csv = "filename.csv"))')
  expect_equal(res, cmp)
})


test_that("can migrate single, multi-piece, dependency", {
  depends <- data.frame(
    id = "latest(parameter:x = 1)",
    index = 1,
    name = "name",
    draft = NA,
    filename = c("a.csv", "b.csv"),
    as = c("d/a.csv", "d/b.csv"),
    is_pinned = FALSE)
  expect_equal(
    src_migrate_depends(list(), list(depends = depends)),
    paste("orderly2::orderly_dependency(",
          '  "name",',
          '  "latest(parameter:x = 1)",',
          '  c("d/a.csv" = "a.csv",',
          '    "d/b.csv" = "b.csv"))',
          sep = "\n"))
})


test_that("can migrate multiple, multi-piece, dependency", {
  depends <- data.frame(
    id = "latest(parameter:x = 1)",
    index = c(1, 1, 2),
    name = "name",
    draft = NA,
    filename = c("a.csv", "b.csv", "a.csv"),
    as = c("1.csv", "x.csv", "y.csv"),
    is_pinned = FALSE)
  expect_equal(
    src_migrate_depends(list(), list(depends = depends)),
    c(paste("orderly2::orderly_dependency(",
            '  "name",',
            '  "latest(parameter:x = 1)",',
            '  c("1.csv" = "a.csv",',
            '    x.csv = "b.csv"))',
            sep = "\n"),
      paste('orderly2::orderly_dependency("name", "latest(parameter:x = 1)",',
            'c(y.csv = "a.csv"))')))
})


test_that("can migrate global resources", {
  expect_null(src_migrate_global_resources(list(), list()))
  expect_equal(
    src_migrate_global_resources(
      list(),
      list(global_resources = c(a.csv = "b.csv"))),
    'orderly2::orderly_shared_resource(a.csv = "b.csv")')
  expect_equal(
    src_migrate_global_resources(
      list(),
      list(global_resources = c("path/a.csv" = "b.csv"))),
    'orderly2::orderly_shared_resource("path/a.csv" = "b.csv")')
})


test_that("can migrate parameters", {
  expect_null(src_migrate_parameters(list(), list()))
  expect_equal(
    src_migrate_parameters(
      list(),
      list(parameters = list(a = NULL, b = NULL))),
    'orderly2::orderly_parameters(a = NULL, b = NULL)')
  expect_equal(
    src_migrate_parameters(
      list(),
      list(parameters = list(a = list(default = 1), b = list(default = "x")))),
    'orderly2::orderly_parameters(a = 1, b = "x")')
})


test_that("can migrate invalid-ish parameters", {
  expect_equal(
    src_migrate_parameters(
      list(),
      list(parameters = list(a = 1, b = 2))),
    "orderly2::orderly_parameters(a = 1, b = 2)"  )
  expect_equal(
    src_migrate_parameters(
      list(),
      list(parameters = list(a = 1, b = NULL))),
    "orderly2::orderly_parameters(a = 1, b = NULL)")
})


test_that("can migrate queries", {
  expect_equal(
    src_migrate_query("latest(parameter:a == b)", NULL),
    "latest(parameter:a == b)")
  expect_equal(
    src_migrate_query("latest(parameter:a == b)", "b"),
    "latest(parameter:a == this:b)")
  expect_equal(
    src_migrate_query("latest(parameter:a == b && a == c)", c("a", "b")),
    "latest(parameter:a == this:b && this:a == c)")
})


test_that("can migrate descriptions", {
  expect_null(src_migrate_description(NULL, list()))
  expect_null(src_migrate_description(NULL, list(displayname = NULL)))
  expect_equal(
    src_migrate_description(NULL, list(displayname = "a")),
    'orderly2::orderly_description(\n  display = "a")')
  expect_equal(
    src_migrate_description(NULL, list(displayname = "a",
                                       fields = list(x = 1))),
    'orderly2::orderly_description(\n  display = "a",\n  custom = list(x = 1))')
  expect_equal(
    src_migrate_description(NULL, list(displayname = "a",
                                       fields = list(x = NA))),
    'orderly2::orderly_description(\n  display = "a")')
  expect_null(src_migrate_description(NULL, list(fields = list(x = NA))))
})


test_that("can run in dry run mode", {
  list_all <- function(path) {
    dir(path, recursive = TRUE, all.files = TRUE, no.. = TRUE,
        include.dirs = FALSE)
  }

  path <- orderly_demo_src()
  files <- list_all(path)
  hash <- tools::md5sum(file.path(path, files))
  orderly2outpack_src(path, dry_run = TRUE)

  expect_equal(list_all(path), files)
  expect_equal(tools::md5sum(file.path(path, files)), hash)
})


test_that("throw informative error on parse failure", {
  expect_error(
    src_migrate_validate("foo", "some error"),
    "Generated invalid code for 'foo':")
})

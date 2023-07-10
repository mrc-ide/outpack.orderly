test_that("Can migrate orderly demo directory", {
  path <- orderly_demo_src()
  orderly2outpack_src(path, delete_yml = TRUE, strict = TRUE)

  dat <- orderly1:::read_demo_yml(path)
  for (i in seq_along(dat)) {
    x <- dat[[i]]
    if (!is.null(x$before)) {
      withr::with_dir(path, x$before())
    }
    env <- new.env(parent = .GlobalEnv)
    dat[[i]]$id <- orderly2::orderly_run(x$name, x$parameters, envir = env,
                                         root = path)
  }
})


test_that("refuse to migrate a directory that does not conain orderly.yml", {
  path <- withr::local_tempdir()
  writeLines("minimum_orderly_version: 1.99.9",
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

  orderly2outpack_src(path1, delete_yml = TRUE)
  orderly2outpack_src(path2, delete_yml = FALSE)
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
  orderly2outpack_src(path, delete_yml = TRUE, strict = TRUE)
  str <- vapply(file.path(path, "src", nms, "orderly.R"),
                function(p) readLines(p, n = 1), "",
                USE.NAMES = FALSE)
  expect_equal(str, rep("orderly2::orderly_strict_mode()", length(nms)))
})


test_that("can not add strict mode", {
  path <- orderly_demo_src()
  nms <- orderly1::orderly_list(path)
  orderly2outpack_src(path, delete_yml = TRUE, strict = FALSE)
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
    'orderly2::orderly_global_resource(a.csv = "b.csv")')
  expect_equal(
    src_migrate_global_resources(
      list(),
      list(global_resources = c("path/a.csv" = "b.csv"))),
    'orderly2::orderly_global_resource("path/a.csv" = "b.csv")')
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

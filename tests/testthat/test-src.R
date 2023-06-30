test_that("Can migrate orderly demo directory", {
  path <- orderly_demo_src()
  orderly2outpack_src(path, delete_yml = TRUE, strict = TRUE)

  dat <- orderly:::read_demo_yml(path)
  for (i in seq_along(dat)) {
    x <- dat[[i]]
    if (!is.null(x$before)) {
      withr::with_dir(path, x$before())
    }
    env <- new.env(parent = .GlobalEnv)
    dat[[i]]$id <- orderly3::orderly_run(x$name, x$parameters, envir = env,
                                         root = path)
  }
})


test_that("refuse to migrate a directory that does not conain orderly.yml", {
  path <- withr::local_tempdir()
  file.create(file.path(path, "orderly_config.yml"))
  expect_error(orderly2outpack_src(path),
               "Did not find any src directories containing 'orderly.yml'")
})


test_that("refuse to migrate directories containing both old and new source", {
  path <- orderly::orderly_example("demo")
  file.create(file.path(path, "src", "spaces", "a resource with spaces.csv"))
  dir.create(file.path(path, "src", "new"))
  file.create(file.path(path, "src", "new", "orderly.R"))
  expect_error(orderly2outpack_src(path),
               "Some source directories already contain 'orderly.R' files")
})


test_that("can preserve original files after migration", {
  path1 <- orderly_demo_src()
  path2 <- orderly_demo_src()

  ## Files we'll keep
  nms <- orderly::orderly_list(path1)
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

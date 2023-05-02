test_that("Can migrate orderly demo directory", {
  path <- orderly::orderly_example("demo")
  file.create(file.path(path, "src", "spaces", "a resource with spaces.csv"))

  orderly2outpack_src(path, delete_yml = TRUE, strict = TRUE)

  path_before <- file.path(path, "before.R")
  before <- sub(
    'orderly::orderly_db("source")',
    'list(source = DBI::dbConnect(RSQLite::SQLite(), "source.sqlite"))',
    readLines(path_before),
    fixed = TRUE)
  before <- c(
    before,
    "other_change_script <- function() {",
    '  txt <- readLines("src/other/orderly.R")',
    '  i <- grep("^source", txt)',
    '  stopifnot(txt[[i + 1]] == "")',
    '  txt[[i + 1]] <- "extract$number <- extract$number * 1.2"',
    '  writeLines(txt, "src/other/orderly.R")',
    '}')
  writeLines(before, path_before)

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

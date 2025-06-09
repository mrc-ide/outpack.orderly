orderly_demo_archive_path <- tempfile(fileext = ".zip")
orderly_demo_archive <- function() {
  if (!file.exists(orderly_demo_archive_path)) {
    src <- orderly1::orderly_example("demo", run_demo = FALSE, quiet = TRUE,
                                     git = TRUE)
    on.exit(unlink(src, recursive = TRUE))
    gert::git_remote_set_url("https://github.com/example/orderly",
                             remote = "origin", repo = src)
    orderly1:::run_orderly_demo(src, quiet = TRUE)
    zip::zip(orderly_demo_archive_path,
             dir(src, all.files = TRUE, no.. = TRUE),
             root = src)
  }
  dest <- tempfile()
  zip::unzip(orderly_demo_archive_path, exdir = dest)
  dest
}


orderly_demo_src_path <- tempfile(fileext = ".zip")
orderly_demo_src <- function() {
  if (!file.exists(orderly_demo_src_path)) {
    path <- orderly1::orderly_example("demo")
    file.create(file.path(path, "src", "spaces", "a resource with spaces.csv"))

    path_before <- file.path(path, "before.R")
    before <- sub(
      'orderly1::orderly_db("source")',
      'list(source = DBI::dbConnect(RSQLite::SQLite(), "source.sqlite"))',
      readLines(path_before),
      fixed = TRUE)
    before <- c(
      before,
      "other_change_script <- function() {",
      '  txt <- readLines("src/other/other.R")',
      '  i <- grep("^source", txt)',
      '  stopifnot(txt[[i + 1]] == "")',
      '  txt[[i + 1]] <- "extract$number <- extract$number * 1.2"',
      '  writeLines(txt, "src/other/other.R")',
      '}')
    writeLines(before, path_before)
    on.exit(unlink(path, recursive = TRUE))
    zip::zip(orderly_demo_src_path, dir(path), root = path)
  }
  dest <- tempfile()
  zip::unzip(orderly_demo_src_path, exdir = dest)
  dest
}

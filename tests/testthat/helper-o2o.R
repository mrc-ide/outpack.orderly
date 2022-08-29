orderly_demo_archive_path <- tempfile(fileext = ".zip")
orderly_demo_archive <- function() {
  if (!file.exists(orderly_demo_archive_path)) {
    src <- orderly::orderly_example("demo", run_demo = TRUE, quiet = TRUE)
    on.exit(unlink(src, recursive = TRUE))
    zip::zip(orderly_demo_archive_path, dir(src), root = src)
  }
  dest <- tempfile()
  zip::unzip(orderly_demo_archive_path, exdir = dest)
  dest
}

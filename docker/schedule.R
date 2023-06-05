#!/usr/bin/env Rscript

'orderly2outpack.

Usage:
  orderly2outpack.R <src> <dest> [--once] [--minutes=<x>] [--custom=<schedule>]

Options:
  --once                     Perform migration once.
  --minutes=<x>              Schedule migration every x minutes.
  --custom=<schedule>        Schedule migration with provided cron schedule.
' -> doc

arguments <- docopt::docopt(doc)

if (arguments$once) {
  outpack.orderly::orderly2outpack(arguments$src, arguments$dest)
} else if (!is.null(arguments$custom) || !is.null(arguments$minutes)) {
  if (!is.null(arguments$custom)) {
    freq <- arguments$custom
  } else {
    freq <- paste(paste0("*/", arguments$minutes), "* * * *")
  }
  cat(c(paste("    command: /usr/local/bin/Rscript /orderly2outpack/orderly2outpack.R",
              arguments$src, arguments$dest),
        paste("    schedule: ", freq, "", sep = "\"")),
      sep = "\n"
  )
} else {
  stop("Frequency of migration not specified. Please pass one of --once, --minutes, or --custom.")
}

#!/usr/bin/env Rscript

'orderly2outpack.

Usage:
  orderly2outpack.R <src> <dest> [--once] [--custom=<schedule>]

Options:
  --once                     Perform migration once.
  --custom=<schedule>        Schedule migration with provided cron schedule.

' -> doc

arguments <- docopt::docopt(doc)

if (arguments$once) {
    outpack.orderly::orderly2outpack(arguments$src, arguments$dest)
} else if (!is.null(arguments$custom)) {
    fn <- deparse(substitute(outpack.orderly::orderly2outpack(src, dest),
                             list(src=arguments$src, dest=arguments$dest)))
    write(c("#!/usr/bin/env Rscript", "\n", fn), "/orderly2outpack/orderly2outpack")
    freq <- arguments$custom
    write(paste0("    schedule: \"", freq, "\""),
          "/orderly2outpack/schedule.yml",
          append = TRUE
    )
} else {
    stop("Frequency of migration not specified. Please pass either --once or --custom option.")
}

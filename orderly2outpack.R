#!/usr/bin/env Rscript

'orderly2outpack.

Usage:
  orderly2outpack.R <src> <dest> [--once] [--hourly]

Options:
  --once      Perform migration once.
  --hourly    Schedule hourly migration.

' -> doc

arguments <- docopt::docopt(doc)

if (arguments$once) {
    outpack.orderly::orderly2outpack(arguments$src, arguments$dest)
} else if (arguments$hourly) {
    write(c("#!/usr/bin/env Rscript", "\n", deparse({
        outpack.orderly::orderly2outpack(arguments$src, arguments$dest)
    })), "~/outpack.hourly")
} else {
    stop("Frequency of migration not specified. Please pass either --once or --hourly option.")
}

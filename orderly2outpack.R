#!/usr/bin/env Rscript

'orderly2outpack.

Usage:
  orderly2outpack.R <src> <dest> [--once]

Options:
  --once      Perform migration once.

' -> doc

arguments <- docopt::docopt(doc)
if (!arguments$once) {
    stop("Only one-time migrations supported. Please pass --once option.")
}
outpack.orderly::orderly2outpack(arguments$src, arguments$dest)

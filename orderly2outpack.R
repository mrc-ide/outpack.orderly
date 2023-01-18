#!/usr/bin/env Rscript

'orderly2outpack.

Usage:
  orderly2outpack.R <src> <dest>
' -> doc

arguments <- docopt::docopt(doc)
outpack.orderly::orderly2outpack(arguments$src, arguments$dest)

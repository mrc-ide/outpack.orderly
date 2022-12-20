#!/usr/bin/env Rscript

options <- commandArgs(trailingOnly = TRUE)
outpack.orderly::orderly2outpack(options[[1]], options[[2]])

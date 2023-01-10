#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = TRUE)
orderly::orderly_example("demo", path = arguments[[1]],
                         run_demo = FALSE, quiet = TRUE)

test_that("migrating source then archive is same as archive then source", {
  src1 <- orderly_demo_archive()
  suppressMessages(dst1 <- orderly2outpack(src1, tempfile()))

  src2 <- orderly_demo_src()
  suppressMessages(orderly2outpack_src(src2, delete_yml = TRUE, strict = TRUE))
  dat <- orderly1:::read_demo_yml(src2)
  for (i in seq_along(dat)) {
    x <- dat[[i]]
    if (!is.null(x$before)) {
      withr::with_dir(src2, x$before())
    }
    env <- new.env(parent = .GlobalEnv)
    expect_no_error(suppressMessages(
      dat[[i]]$id <- orderly2::orderly_run(x$name, x$parameters, envir = env,
                                           root = src2, echo = FALSE)))
  }

  ## Need to copy over configuration to the archive migration, because
  ## otherwise we don't get custom deserialisers for orderly.db, which
  ## is a bit annoying.
  file.copy(file.path(src2, "orderly_config.yml"),
            file.path(dst1, "orderly_config.yml"), overwrite = TRUE)

  id1 <- sort(dir(file.path(dst1, ".outpack", "metadata")))
  id2 <- vcapply(dat, "[[", "id")
  expect_length(id2, length(id1))

  for (i in seq_along(id1)) {
    meta1 <- orderly2::orderly_metadata(id1[[i]], root = dst1)
    meta2 <- orderly2::orderly_metadata(id2[[i]], root = src2)
    expect_setequal(names(meta1), names(meta2))
    expect_equal(meta1$schema_version, meta2$schema_version)
    expect_equal(meta1$name, meta2$name)
    expect_equal(meta1$parameters, meta2$parameters)

    ## Can't check id, files, time as we don't expect these to be the
    ## same.  The git cases are empty, and depends needs some work to
    ## remap ids.
    depends2 <- meta2$depends
    depends2$packet <- id1[match(depends2$packet, id2)]
    expect_equal(meta1$depends, depends2)

    expect_equal(meta1$git, meta2$git)

    ## custom metadata
    expect_setequal(names(meta1$custom), names(meta2$custom))

    orderly1 <- meta1$custom$orderly
    orderly2 <- meta2$custom$orderly
    expect_setequal(names(orderly1), names(orderly2))
    expect_equal(orderly1$artefacts, orderly2$artefacts)
    expect_equal(orderly1$shared, orderly2$shared)
    expect_equal(orderly1$description, orderly2$description)
    ## Can't check role or session; these are expected to differ.

    expect_equal(meta1$custom$orderly.db, meta2$custom$orderly.db)
  }
})

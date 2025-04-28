##' Migrate an orderly archive to outpack. This function is subject to
##' change, and does not handle all failure modes. It is quite slow
##' because it involves hashing the contents of your archive several
##' times - if you have GBs of files expect this to take minutes or
##' hours.
##'
##' @title Migrate orderly to outpack
##'
##' @param src Path to the orderly archive to migrate. This must be
##'   "complete" (i.e., containing a full graph and no metadata only
##'   pulls)
##'
##' @param dest The destination to create a new outpack archive. This
##'   will be created with no user-visible archive directory, with a
##'   file store, and requiring a full tree (i.e., in a suitable
##'   configuration to use as an upstream source).
##'
##' @param link Logical, indicating if the file store should be hard
##'   links to the existing orderly archive, rather than copies of the
##'   files. Doing this carries some risk and constraints: must be on
##'   the same file system, can't make files readonly, changes
##'   propagated both ways. As a result, only do this where you are
##'   confident that nothing will alter files in the source archive!
##'   The use case here is in continually migrating an orderly
##'   repository.
##'
##' @param keep_going Logical, indicating if we should try and migrate as
##'   much as possible, skipping over packets that fail.
##'
##' @param parallel Logical, indicating if we should process data in
##'   parallel.  This uses `parallel::mclapply` so it only works on
##'   Linux.  Use the `mc.cores` option or `MC_CORES` environment
##'   variable to control the number of cores used.
##'
##' @return The path to the newly created archive
##' @export
orderly2outpack <- function(src, dest, link = FALSE, keep_going = FALSE,
                            parallel = FALSE) {
  existing <- dir(dest, all.files = TRUE, no.. = TRUE)
  err <- setdiff(existing, c(".outpack", "orderly_config.yml"))
  if (length(err) > 0) {
    stop("Destination directory is not a bare outpack destination")
  }
  orderly2::orderly_init(dest,
                         path_archive = NULL,
                         use_file_store = TRUE,
                         require_complete_tree = TRUE)
  hash_algorithm <- orderly2::orderly_config(dest)$core$hash_algorithm

  cfg_orderly <- orderly1::orderly_config(src)
  src <- cfg_orderly$root
  message("Checking we can migrate this orderly archive")
  check_complete_tree(src)

  known <- orderly2::orderly_search(NULL, location = "local", root = dest)
  contents <- orderly1::orderly_list_archive(src)
  contents <- contents[!(contents$id %in% known), ]
  contents <- contents[order(contents$id), ]

  migrate_metadata1 <- function(p, hash_algorithm) {
    message(p)
    tryCatch(
      list(success = TRUE,
           value = orderly_metadata_to_outpack(p, hash_algorithm)),
      error = function(e) {
        cli::cli_alert_danger("Metadata failure for '{p}': {e}")
        id <- basename(p)
        name <- basename(dirname(p))
        list(success = FALSE, value = list(id = id, name = name), error = e)
      })
  }

  message("Reading metadata")
  paths <- file.path(src, "archive", contents$name, contents$id)
  if (isFALSE(parallel)) {
    res <- lapply(paths, migrate_metadata1, hash_algorithm)
  } else {
    res <- parallel::mclapply(paths, migrate_metadata1, hash_algorithm)
  }

  ok <- vlapply(res, "[[", "success")
  if (!all(ok)) {
    msg <- "Metadata migration failure for {sum(!ok)}/{length(ok)} packet{?s}"
    if (!keep_going) {
      cli::cli_abort(msg)
    }
    cli::cli_alert_danger(msg)
    cli::cli_alert_info(
      "These packets will be skipped and we can try these later")
    ids_skip <- contents$id[!ok]
  } else {
    ids_skip <- character()
  }

  root <- orderly2:::root_open(dest, FALSE)

  if (link) {
    message("Linking files, rather than copying them, into the file store")
    root$files <- file_store_link$new(root$files$path)
  }

  message("Importing packets")
  for (x in res) {
    message(sprintf("%s/%s", x$value$name, x$value$id))
    if (!x$success) {
      cli::cli_alert_danger("skipping due to failure reading metadata")
      next
    }

    uses_skipped_packet <-
      any(ids_skip %in% jsonlite::fromJSON(x$value$json)$depends$packet)

    if (uses_skipped_packet) {
      cli::cli_alert_danger("skipping due to use of skipped dependency")
      ids_skip <- c(ids_skip, x$value$id)
      next
    }

    p <- file.path(src, "archive", x$value$name, x$value$id)
    orderly2:::outpack_insert_packet(p, x$value$json, root)
  }

  if (length(ids_skip) > 0) {
    cli::cli_alert_warning(
      paste("Writing details of {length(ids_skip)} skipped packet{?s}",
            "to '.outpack/import_skipped*'"))
    saveRDS(res[contents$id %in% ids_skip],
            file.path(dest, ".outpack/import_skipped.rds"))
    writeLines(ids_skip, file.path(dest, ".outpack/import_skipped_ids"))
  }

  dest
}


orderly_metadata_to_outpack <- function(path, hash_algorithm) {
  data <- readRDS(file.path(path, "orderly_run.rds"))

  time <- list(start = data$time - data$meta$elapsed,
               end = data$time)
  name <- data$meta$name
  id <- data$meta$id

  files <- c(data$meta$file_info_inputs$filename,
             data$meta$file_info_artefacts$filename,
             data$meta$depends$as)
  hash_expected <- sprintf(
    "md5:%s", c(data$meta$file_info_inputs$file_hash,
                data$meta$file_info_artefacts$file_hash,
                data$meta$depends$hash))
  stopifnot(length(hash_expected) == length(files),
            length(files) > 0)

  hash_found <- withr::with_dir(path, orderly2:::hash_files(files, "md5"))
  hash_err <- hash_expected != hash_found
  if (any(hash_err)) {
    message(paste0(sprintf("Some hashes do not agree for %s/%s:\n", name, id),
                   paste(sprintf("  - %s", files[hash_err]), collapse = "\n")))
  }

  ignore <- c("orderly_run.rds", "orderly.log")
  found <- dir(path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  extra <- setdiff(found, c(files, ignore))

  parameters <- data$meta$parameters
  if (inherits(parameters, "data.frame")) {
    ## Seen in native-201910-201710-compare-impact/20200603-103158-9a8cb992
    parameters <- as.list(parameters)
  }

  depends <- archive_migrate_depends(data$meta$depends, names(parameters))

  script <- data$meta$file_info_inputs$filename[
    data$meta$file_info_inputs$file_purpose == "script"]
  session <- orderly2:::orderly_session_info(data$session_info)

  f_artefacts <- function(x, outputs) {
    list(description = jsonlite::unbox(x$description),
         paths = outputs$filename[outputs$order == x$order])
  }
  artefacts <- lapply(seq_len(nrow(data$meta$artefacts)), function(i) {
    f_artefacts(data$meta$artefacts[i, ], data$meta$file_info_artefacts)
  })

  if (length(data$meta$global_resources) == 0) {
    shared <- list()
  } else {
    shared <- Map(function(here, there) {
      list(here = jsonlite::unbox(here),
           there = jsonlite::unbox(there))
    }, names(data$meta$global_resources), unname(data$meta$global_resources),
    USE.NAMES = FALSE)
  }

  role <- data_frame(
    path = c(data$meta$file_info_inputs$filename,
             data$meta$depends$as),
    role = c(data$meta$file_info_inputs$file_purpose,
             rep("dependency", NROW(data$meta$depends))))
  role$role[role$role %in% c("orderly_yml", "script", "readme", "source")] <-
    "resource"
  role$role[role$role == "global"] <- "shared"

  custom <- data$meta$extra_fields
  custom <- custom[!vlapply(custom, function(x) is.null(x) || is.na(x))]
  if (!is.null(custom)) {
    custom <- lapply(custom, scalar)
  }

  orderly_db <- orderly_db_metadata_to_outpack(path, data)

  oo <- options(outpack.schema_validate = TRUE)
  on.exit(options(oo))

  orderly <- list(
    artefacts = artefacts,
    shared = shared,
    role = role,
    description = list(
      display = scalar(data$meta$displayname),
      long = scalar(data$meta$description),
      custom = custom),
    session = session)
  orderly_json <- orderly2:::to_json(orderly, "orderly/orderly.json")

  custom <- list(list(application = "orderly", data = orderly_json))
  if (!is.null(orderly_db)) {
    orderly_db_json <- orderly2:::to_json(orderly_db,
                                          "orderly.db/orderly.db.json")
    custom$orderly.db <- list(application = "orderly.db",
                              data = orderly_db_json)
  }
  json <- orderly2:::outpack_metadata_create(
    path = path, name = name, id = id, time = time, files = files,
    depends = depends, parameters = parameters, custom = custom,
    file_ignore = NULL, file_hash = NULL,
    hash_algorithm = hash_algorithm)

  list(id = id,
       name = name,
       json = json)
}


## Try to fail fast if we don't have a complete archive
check_complete_tree <- function(path) {
  contents <- orderly1::orderly_list_archive(path)
  p <- file.path(path, "archive", contents$name, contents$id, "orderly_run.rds")
  d <- lapply(p, readRDS)
  used <- lapply(d, function(el) unique(el$meta$depends$id))
  if (!all(unlist(used) %in% contents$id)) {
    stop("orderly graph is incomplete")
  }
}


orderly_db_metadata_to_outpack <- function(path, data) {
  ret <- list()

  view <- data$meta$view
  if (!is.null(view)) {
    ret$view <- lapply(seq_len(nrow(view)), function(i) {
      database <- view$database[[i]]
      list(database = scalar(database),
           instance = scalar(data$meta$instance[[database]]),
           as = scalar(view$name[[i]]),
           query = scalar(view$query[[i]]))
    })
  }

  query <- data$meta$data
  if (!is.null(query)) {
    path_data <- file.path(dirname(dirname(dirname(path))), "data/rds")
    ret$query <- lapply(seq_len(nrow(query)), function(i) {
      d <- readRDS(file.path(path_data, paste0(query$hash[[i]], ".rds")))
      database <- query$database[[i]]
      list(database = scalar(database),
           instance = scalar(data$meta$instance[[database]]),
           name = scalar(query$name[[i]]),
           query = scalar(query$query[[i]]),
           rows = scalar(nrow(d)),
           cols = names(d))
    })
  }

  connection <- data$meta$connection
  if (isTRUE(connection)) {
    ## Turns out we never saved this information properly anyway:
    filename <- file.path(path, "orderly.yml")
    yml <- orderly1:::yaml_read(filename)
    config <- orderly1::orderly_config(file.path(path, "../../.."), FALSE)
    con <- orderly1:::recipe_migrate(yml, config, filename, TRUE)$connection
    ret$connection <- lapply(unname(con), function(database) {
      list(database = scalar(database),
           instance = scalar(data$meta$instance[[database]]))
    })
  }

  if (length(ret) == 0) NULL else ret
}


archive_migrate_depends <- function(depends, parameters) {
  if (is.null(depends)) {
    return(NULL)
  }
  ## Seen in rtm_incoming_serology/20200603-204022-70b8bfa5
  if (is.null(depends$index)) {
    depends$index <- as.integer(factor(paste0(depends$id, depends$id_requested)))
  }
  unname(lapply(
    split(depends, depends$index), function(x) {
      if (x$id_requested[[1]] %in% c("latest", "latest()")) {
        query <- sprintf('latest(name == "%s")', x$name[[1]])
      } else if (grepl("latest\\(", x$id_requested[[1]])) {
        str <- sub(")$", sprintf(' && name == "%s")', x$name[[1]]), x$id_requested[[1]])
        query <- src_migrate_query(str, parameters)
      } else {
        query <- x$id_requested[[1]]
      }
      list(packet = x$id[[1]],
           query = query,
           files = data_frame(here = x$as, there = x$filename))
    }))
}

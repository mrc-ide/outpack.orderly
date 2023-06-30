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
##' @return The path to the newly created archive
##' @export
orderly2outpack <- function(src, dest, link = FALSE) {
  existing <- dir(dest, all.files = TRUE, no.. = TRUE)
  if (length(existing) > 0) {
    if (!identical(existing, ".outpack")) {
      stop("Destination directory is not a bare outpack destination")
    }
    root_outpack <- outpack::outpack_root_open(dest, FALSE)
  } else {
    root_outpack <- outpack::outpack_init(dest,
                                          logging_console = FALSE,
                                          path_archive = NULL,
                                          use_file_store = TRUE,
                                          require_complete_tree = TRUE)
  }
  hash_algorithm <- root_outpack$config$core$hash_algorithm

  cfg_orderly <- orderly::orderly_config(src)
  src <- cfg_orderly$root
  message("Checking we can migrate this orderly archive")
  check_complete_tree(src)

  known <- root_outpack$index()$unpacked
  contents <- orderly::orderly_list_archive(src)
  contents <- contents[!(contents$id %in% known), ]
  contents <- file.path(src, "archive", contents$name, contents$id)

  ## Theoretically we could do this faster in parallel; not sure if
  ## we're CPU or IO bound really.
  message("Reading metadata")
  res <- lapply(contents, function(p) {
    message(p)
    orderly_metadata_to_outpack(p, hash_algorithm)
  })

  res <- res[order(vapply(res, "[[", "", "id"))]

  if (link) {
    message("Linking files, rather than copying them, into the file store")
    root_outpack$files <- file_store_link$new(root_outpack$files$path)
  }

  message("Importing packets")
  for (x in res) {
    message(sprintf("%s/%s", x$name, x$id))
    p <- file.path(src, "archive", x$name, x$id)
    outpack:::outpack_insert_packet(p, x$json, root_outpack)
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

  hash_found <- withr::with_dir(path, outpack:::hash_files(files, "md5"))
  hash_err <- hash_expected != hash_found
  if (any(hash_err)) {
    message(paste0(sprintf("Some hashes do not agree for %s/%s:\n", name, id),
                   paste(sprintf("  - %s", files[hash_err]), collapse = "\n")))
  }

  ignore <- c("orderly_run.rds", "orderly.log")
  found <- dir(path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  extra <- setdiff(found, c(files, ignore))

  if (is.null(data$meta$depends)) {
    depends <- NULL
  } else {
    ## Seen in rtm_incoming_serology/20200603-204022-70b8bfa5
    if (is.null(data$meta$depends$index)) {
      data$meta$depends$index <- as.integer(factor(data$meta$depends$id))
    }
    depends <- unname(lapply(
      split(data$meta$depends, data$meta$depends$index), function(x) {
        list(packet = x$id[[1]],
             query = x$id_requested[[1]],
             files = data_frame(here = x$as,
                                there = x$filename))
      }))
  }

  parameters <- data$meta$parameters
  if (inherits(parameters, "data.frame")) {
    ## Seen in native-201910-201710-compare-impact/20200603-103158-9a8cb992
    parameters <- as.list(parameters)
  }

  script <- data$meta$file_info_inputs$filename[
    data$meta$file_info_inputs$file_purpose == "script"]
  session <- outpack:::outpack_session_info(data$session_info)

  f_artefacts <- function(x, outputs) {
    list(description = jsonlite::unbox(x$description),
         paths = outputs$filename[outputs$order == x$order])
  }
  artefacts <- lapply(seq_len(nrow(data$meta$artefacts)), function(i) {
    f_artefacts(data$meta$artefacts[i, ], data$meta$file_info_artefacts)
  })

  if (length(data$meta$global_resources) == 0) {
    global <- list()
  } else {
    global <- Map(function(here, there) {
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

  ## TODO: this can be updated now, and should be where db bits are
  ## present; we can detect this from the root configuration really.

  ## NOTE: assuming empty custom metadata, seems fair at first. This
  ## is only used to access plugins, and later we might want to
  ## support this properly for VIMC db migrations?
  schema <- orderly3:::custom_metadata_schema(list())

  custom <- data$meta$extra_fields
  if (!is.null(custom)) {
    custom <- lapply(custom, scalar)
  }

  orderly <- list(
    "artefacts" = artefacts,
    "packages" = data$meta$packages %||% character(0),
    "global" = global,
    "role" = role,
    "description" = list(
      "display" = scalar(data$meta$displayname),
      "long" = scalar(data$meta$description),
      "custom" = custom))
  orderly_json <- jsonlite::toJSON(
    orderly, pretty = FALSE, auto_unbox = FALSE,
    json_verbatim = TRUE, na = "null", null = "null")

  outpack:::custom_schema(schema)$validate(orderly_json, error = TRUE)
  custom <- list(list(application = "orderly",
                      data = orderly_json))

  oo <- options(outpack.schema_validate = TRUE)
  on.exit(options(oo))

  json <- outpack:::outpack_metadata_create(
    path = path, name = name, id = id, time = time, files = files,
    depends = depends, parameters = parameters, script = script,
    custom = custom, session = session,
    file_ignore = NULL, file_hash = NULL,
    hash_algorithm = hash_algorithm)

  list(id = id,
       name = name,
       json = json)
}


## Try to fail fast if we don't have a complete archive
check_complete_tree <- function(path) {
  contents <- orderly::orderly_list_archive(path)
  p <- file.path(path, "archive", contents$name, contents$id, "orderly_run.rds")
  d <- lapply(p, readRDS)
  used <- lapply(d, function(el) unique(el$meta$depends$id))
  if (!all(unlist(used) %in% contents$id)) {
    stop("orderly graph is incomplete")
  }
}

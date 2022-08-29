migrate_metadata <- function(root, verbose = TRUE) {
  cfg_orderly <- orderly::orderly_config(root)
  root <- cfg_orderly$root

  if (file.exists(file.path(root, ".outpack"))) {
    root_outpack <- outpack::outpack_root_open(root)
  } else {
    root_outpack <- outpack::outpack_init(root)
  }

  contents <- orderly::orderly_list_archive(root)
  contents <- file.path(path, "archive", contents$name, contents$id)
  res <- lapply(contents, function(p) {
    if (verbose) {
      message(paste0(p, "..."), appendLF = FALSE)
    }
    ret <- tryCatch(
      orderly_metadata_to_outpack(p, root_outpack$config),
      error = identity)
    fail <- inherits(ret, "error")
    if (verbose) {
      message(if (fail) "FAIL" else "ok")
    }
    ret
  })

  browser()

  res <- res[order(vapply(res, "[[", "", "id"))]
  saveRDS(res, "res.rds")


}

orderly_metadata_to_outpack <- function(path, cfg_outpack) {
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
    message(sprintf("*** Some hashes do not agree for %s/%s:", name, id))
    message(paste(sprintf("  - %s", files[hash_err]), collapse = "\n"))
  }

  ignore <- c("orderly_run.rds", "orderly.log")
  found <- dir(path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  extra <- setdiff(found, c(files, ignore))

  if (length(extra) > 0) {
    browser()
  }

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
             files = data_frame(here = x$as,
                                there = x$filename))
      }))
  }

  parameters <- data$meta$parameters
  if (class(parameters) == "data.frame") {
    ## Seen in native-201910-201710-compare-impact/20200603-103158-9a8cb992
    parameters <- as.list(parameters)
  }

  script <- data$meta$file_info_inputs$filename[
    data$meta$file_info_inputs$file_purpose == "script"]
  session <- outpack:::outpack_session_info(data$session_info)

  ## TODO: this is the root, not a config, or we pass just the config
  ## in which is fine
  hash_algorithm <- cfg_outpack$core$hash_algorithm

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

  schema <- orderly2:::custom_metadata_schema()

  orderly <- list(
    "artefacts" = artefacts,
    "packages" = data$meta$packages %||% character(0),
    "global" = global,
    "role" = role,
    "displayname" = scalar(data$meta$displayname),
    "description" = scalar(data$meta$description),
    "custom" = NULL)
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

orderly_to_outpack <- function(path) {
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
  if (any(hash_expected != hash_found)) {
    stop("Some hashes do not agree")
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
      split(data$meta$depends, data$meta$depends$index), function(x)
        list(id = x$id[[1]],
             files = data_frame(here = x$as, there = x$filename))))
  }

  parameters <- data$meta$parameters
  if (class(parameters) == "data.frame") {
    ## Seen in native-201910-201710-compare-impact/20200603-103158-9a8cb992
    parameters <- as.list(parameters)
  }

  script <- data$meta$file_info_inputs$filename[
    data$meta$file_info_inputs$file_purpose == "script"]
  session <- outpack:::outpack_session_info(data$session_info)
  hash_algorithm <- "sha256"

  f_artefacts <- function(x, outputs) {
    list(description = jsonlite::unbox(x$description),
         format = jsonlite::unbox(x$format),
         contents = outputs$filename[outputs$order == x$order])
  }
  artefacts <- lapply(seq_len(nrow(data$meta$artefacts)), function(i)
    f_artefacts(data$meta$artefacts[i, ], data$meta$file_info_artefacts))

  global <- lapply(data$meta$global_resources, jsonlite::unbox)
  if (length(global) == 0) { # ensure serialiation consistent
    global <- setNames(list(), character())
  }

  role <- c(
    setNames(lapply(data$meta$file_info_inputs$file_purpose, jsonlite::unbox),
             data$meta$file_info_inputs$filename),
    setNames(rep(list(scalar("dependency")), NROW(data$meta$depends)),
             data$meta$depends$as))

  jsonlite::toJSON(
    setNames(list(), character()), pretty = FALSE, auto_unbox = FALSE,
    json_verbatim = TRUE, na = "null", null = "null")

  orderly <- list(
    "artefacts" = artefacts,
    "packages" = data$meta$packages %||% character(0),
    "global" = global,
    "role" = role,
    "displayname" = scalar(data$meta$displayname),
    "description" = scalar(data$meta$description))
  orderly_json <- jsonlite::toJSON(
    orderly, pretty = FALSE, auto_unbox = FALSE,
    json_verbatim = TRUE, na = "null", null = "null")
  path_schema <- system.file("schema/orderly.json", package = "outpack.orderly",
                             mustWork = TRUE)
  outpack:::custom_schema(path_schema)$validate(orderly_json, error = TRUE)
  custom <- list(list(application = "orderly",
                      data = orderly_json))

  oo <- options(outpack.schema_validate = TRUE)
  on.exit(options(oo))

  json <- outpack:::outpack_metadata_create(
    path = path, name = name, id = id, time = time, files = files,
    depends = depends, parameters = parameters, script = script,
    custom = custom, session = session, hash_algorithm = hash_algorithm)

  list(id = id,
       name = name,
       json = json)
}

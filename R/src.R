##' Migrate an orderly *source directory* for use with outpack.  This
##' works in place, writing out `orderly.R` files and deleting
##' `orderly.yml` files (optionally). This is intended to be run on a
##' clean clone of your source git repository.
##'
##' @title Migrate orderly source
##'
##' @param path Path to the orderly source directory
##'
##' @param delete_yml Logical, indicating if the `orderly.yml` files
##'   should be deleted.
##'
##' @param strict Logical, indicating if we should enable
##'   [`orderly2::orderly_strict_mode()`] in the resulting source
##'   files. You can always add this later to specific cases (or
##'   remove it).
##'
##' @param dry_run Logical, indicating if we should just run process
##'   all the data, but not write any changes.
##'
##' @return Nothing, called for side effects only
##' @export
orderly2outpack_src <- function(path, delete_yml = FALSE, strict = FALSE,
                                dry_run = FALSE) {
  cfg <- orderly1::orderly_config(path)
  path <- cfg$root

  global_path <- cfg$global_resources
  nms <- orderly1::orderly_list(path)

  i <- file.exists(file.path(path, "src", nms, "orderly.R"))
  if (any(i)) {
    stop("Some source directories already contain 'orderly.R' files: ",
         paste(nms[i], collapse = ", "))
  }

  i <- file.exists(file.path(path, "src", nms, "orderly.yml"))
  if (!any(i)) {
    stop("Did not find any src directories containing 'orderly.yml'")
  }
  nms <- nms[i]

  cfg_new <- src_migrate_cfg(cfg$raw)
  dat_new <- lapply(nms, src_migrate_src, cfg, strict)

  if (dry_run) {
    return(invisible(path))
  }

  if (!delete_yml) {
    fs::file_copy(file.path(path, "orderly_config.yml"),
                  file.path(path, "orderly_config.yml.orig"))
  }
  orderly1:::yaml_write(cfg_new, file.path(path, "orderly_config.yml"))

  for (i in seq_along(nms)) {
    writeLines(dat_new[[i]]$code,
               file.path(path, "src", nms[[i]], "orderly.R"))
  }

  if (delete_yml) {
    file.remove(file.path(path, "src", nms,
                          vapply(dat_new, "[[", "", "script")))
    file.remove(file.path(path, "src", nms, "orderly.yml"))
  }

  if (!is.null(global_path)) {
    fs::file_move(file.path(path, global_path), file.path(path, "shared"))
  }

  orderly2::orderly_init(path, logging_console = FALSE)

  invisible(path)
}


src_migrate_cfg <- function(cfg) {
  ## Can't do: fields, changelog, tags, remote, vault
  ##
  ## of these :
  ##
  ## * vault and remote will get supported later, I think
  ## * fields and tags are obsolete, tags is unused
  ## * changelog needs a total overhaul

  ## Note that this does not at all preserve the comments, and we
  ## should direct the user to do that, but we might be able to do
  ## something on that by the time we use this for real.
  ret <- list(minimum_orderly_version = "1.99.0")
  if (!is.null(cfg$database)) {
    ret$plugins <- list("orderly.db" = cfg$database)
  }

  ret
}


src_migrate_src <- function(name, cfg, strict) {
  ## TODO: not yet handled - changelog (overhauling this),
  ## environment, fields, readme, secrets,
  ## tags - some of these we might just have some general "extra
  ## metadata" field really, but it would also be good to check in
  ## various locations which of these are really used in a meaningful
  ## way.

  ## TODO: some control parameter here to tune 'instance' or not
  ## through views, data, connection; easier once we have variable
  ## interpolation in orderly2 configuration, plus a helper in the db
  ## plugin.

  migrate <- list(
    src_migrate_description,
    src_migrate_parameters,
    src_migrate_global_resources,
    src_migrate_resources,
    src_migrate_depends,
    src_migrate_artefacts,
    src_migrate_db_views,
    src_migrate_db_data,
    src_migrate_db_connection,
    src_migrate_packages,
    src_migrate_sources,
    src_migrate_script)

  dat <- suppressWarnings(orderly1:::orderly_recipe$new(name, cfg, TRUE))
  code <- if (strict) "orderly2::orderly_strict_mode()" else character(0)
  for (f in migrate) {
    code <- add_section(code, f(cfg, dat))
  }
  list(code = code, script = dat$script)
}


src_migrate_description <- function(cfg, dat) {
  args <- list(display = dat$displayname,
               long = dat$description,
               custom = dat$fields[!vlapply(dat$fields, is_blank)])
  args <- args[lengths(args) > 0 & !vlapply(args, is_blank)]
  if (length(args) == 0) {
    return(NULL)
  }
  args_str <- paste(sprintf("\n  %s = %s", names(args),
                            vapply(args, deparse1, "", width.cutoff = 500)),
                    collapse = ",")
  sprintf("orderly2::orderly_description(%s)", args_str)
}


src_migrate_parameters <- function(cfg, dat) {
  if (is.null(dat$parameters)) {
    return(NULL)
  }
  ## TODO: there's some inconsistency here with pluralisation in
  ## orderly2, might need some updating.
  fmt <- "orderly2::orderly_parameters(%s)"
  pars <- vcapply(dat$parameters,
                  function(x) deparse1(if (is.atomic(x)) x else x$default))
  args <- sprintf("%s = %s", names(pars), pars)
  sprintf(fmt, paste(args, collapse = ", "))
}


src_migrate_global_resources <- function(cfg, dat) {
  if (is.null(dat$global_resources)) {
    return(NULL)
  }
  fmt <- "orderly2::orderly_shared_resource(%s)"
  args <- sprintf('%s = "%s"',
                  dquote_if_required(names(dat$global_resources)),
                  unname(dat$global_resources))
  sprintf(fmt, paste(args, collapse = ", "))
}


src_migrate_resources <- function(cfg, dat) {
  if (is.null(dat$resources)) {
    return(NULL)
  }
  fmt <- "orderly2::orderly_resource(%s)"
  if (length(dat$resources) == 1L) {
    args <- dquote(dat$resources)
  } else {
    args <-sprintf("c(%s)", paste(dquote(dat$resources), collapse = ", "))
  }
  sprintf(fmt, args)
}


src_migrate_depends <- function(cfg, dat) {
  if (is.null(dat$depends)) {
    return(NULL)
  }

  parameters <- names(dat$parameters)

  ret <- character()
  for (el in split(dat$depends, dat$depends$index)) {
    name <- el$name[[1]]
    query <- src_migrate_query(el$id[[1]], parameters)
    there <- el$filename
    here <- el$as
    use <- sprintf("%s = %s", dquote_if_required(here), dquote(there))
    if (length(there) == 1) {
      str <- sprintf('orderly2::orderly_dependency("%s", "%s", c(%s))',
                     name, query, use)
    } else {
      str <- sprintf(
        'orderly2::orderly_dependency(\n  "%s",\n  "%s",\n  c(%s))',
        name, query, paste(use, collapse = ",\n    "))
    }
    ret <- c(ret, str)
  }
  ret
}


src_migrate_artefacts <- function(cfg, dat) {
  fmt <- "orderly2::orderly_artefact(%s)"
  ret <- character()
  for (i in seq_len(nrow(dat$artefacts))) {
    description <- dat$artefacts[i, ]$description
    filenames <- dat$artefacts[i, ]$filenames
    if (length(filenames) == 1L) {
      args <- sprintf('"%s", "%s"', description, filenames)
    } else {
      args <- sprintf('\n  "%s",\n  c(%s)', description,
                      paste(dquote(filenames), collapse = ", "))
    }
    ret <- c(ret, sprintf(fmt, args))
  }
  ret
}


src_migrate_packages <- function(cfg, dat) {
  sprintf("library(%s)", dat$packages)
}


src_migrate_sources <- function(cfg, dat) {
  c(sprintf('orderly2::orderly_resource("%s")', dat$sources),
    sprintf('source("%s")', dat$sources))
}


src_migrate_script <- function(cfg, dat) {
  code <- read_lines(file.path(cfg$root, "src", dat$name, dat$script))
  code <- sub("orderly1?::orderly_run_info", "orderly2::orderly_run_info",
              code)
  code
}


src_migrate_db_views <- function(cfg, dat) {
  if (is.null(dat$views)) {
    return(NULL)
  }
  fmt <- "orderly.db::orderly_db_view(\n  %s)"
  ret <- character(0)
  for (i in names(dat$views)) {
    x <- dat$views[[i]]
    args <- c(query = x$query, as = i)
    if (!is.null(x$database)) {
      args[["database"]] <- x$database
    }
    args_str <- paste(sprintf('%s = "%s"', names(args), unname(args)),
                      collapse = ",\n  ")
    ret <- c(ret, sprintf(fmt, args_str))
  }
  ret
}


src_migrate_db_data <- function(cfg, dat) {
  if (is.null(dat$data)) {
    return(NULL)
  }
  fmt <- "orderly.db::orderly_db_query(\n  %s)"
  ret <- character(0)
  for (i in names(dat$data)) {
    x <- dat$data[[i]]
    args <- c(query = x$query, as = i)
    if (!is.null(x$database)) {
      args[["database"]] <- x$database
    }
    args_str <- paste(sprintf('%s = "%s"', names(args), unname(args)),
                      collapse = ",\n  ")
    ret <- c(ret, sprintf(fmt, args_str))
  }
  ret
}


src_migrate_db_connection <- function(cfg, dat) {
  if (is.null(dat$connection)) {
    return(NULL)
  }

  ret <- character(0)
  fmt <- "orderly.db::orderly_db_connection(%s)"
  for (i in names(dat$connection)) {
    ## TODO: some control parameter here to tune 'instance' or not.
    args <- c(as = i, database = dat$connection[[i]])
    args_str <- paste(sprintf('%s = "%s"', names(args), unname(args)),
                      collapse = ", ")
    ret <- c(ret, sprintf(fmt, args_str))
  }
  ret
}


src_migrate_query <- function(query, parameters) {
  rewrite <- function(expr) {
    if (is.recursive(expr)) {
      if (!identical(expr[[1]], as.name(":"))) {
        expr[-1] <- lapply(expr[-1], rewrite)
      }
      expr
    } else if (is.symbol(expr) && deparse(expr) %in% parameters) {
      call(":", quote(this), expr)
    } else {
      expr
    }
  }
  if (length(parameters) == 0 || !grepl("\\(.+\\)", query)) {
    return(query)
  }
  expr <- parse(text = query)[[1]]
  deparse(rewrite(expr))
}


add_section <- function(curr, new) {
  if (length(new) == 0) {
    curr
  } else if (length(curr) == 0) {
    new
  } else {
    c(curr, "", new)
  }
}

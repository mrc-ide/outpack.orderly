## This will be useful for copying over files, we can use this in the
## initial migration too. I don't think that it wants to go into the
## core outpack implementation though as we probably do not want to
## encourage use of hard links...
file_store_link <- R6::R6Class(
  "file_store_link",
  cloneable = FALSE,
  inherit = orderly:::file_store,

  public = list(
    put = function(src, hash, move = FALSE) {
      if (move) {
        stop("Can't move files into a link store")
      }
      orderly:::hash_validate_file(src, hash)
      dst <- self$filename(hash)
      if (!fs::file_exists(dst)) {
        fs::dir_create(dirname(dst))
        fs::link_create(src, dst, symbolic = FALSE)
      }
      invisible(hash)
    }
  ))

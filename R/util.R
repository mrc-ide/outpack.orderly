`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}


dquote <- function(x) {
  sprintf('"%s"', x)
}


dquote_if_required <- function(x) {
  i <- grepl("([-+ /]|^[^A-Za-z])", x)
  if (any(i)) {
    x[i] <- dquote(x[i])
  }
  x
}


deparse1 <- function(x, ...) {
  paste(deparse(x, ...), collapse = "\n")
}


read_lines <- function(...) {
  readLines(..., warn = FALSE)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


vlapply <- function(...) {
  vapply(..., FUN.VALUE = TRUE)
}


is_blank <- function(x) {
  is.null(x) || (length(x) == 1 && is.na(x))
}

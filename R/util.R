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
  if (grepl("[-+ /]", x)) dquote(x) else x
}


deparse1 <- function(x, ...) {
  paste(deparse(x, ...), collapse = "\n")
}

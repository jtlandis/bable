

is_call <- function(x, call = NULL) {
  res <- inherits(x, "call")
  if (is.null(call)) {
    return(res)
  } else {
    return(res && as.character(x[[1L]]) %in% call)
  }
}
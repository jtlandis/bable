

enQuo <- function(arg) {
  arg <- substitute(arg)
  eval(call("substitute", arg, parent.frame()))
}

#' @export
enQuos <- function(...) {
  exprs <- as.list(substitute(.(...))[-1L])
  env <- parent.frame(2L)
  obj <- lapply(
    exprs,
    function(x, env) eval(call('substitute', x, env)),
    env = parent.frame()
  )
  return(obj)

}

#' @export
Quo <- function(expr) {
 substitute(expr)
}

#' @export
Quos <- function(...) {
  as.list(substitute(.(...))[-1L])
}



#---- making indicies
#
# test1 <- function(...) {
#   #browser()
#   ans <- list(...)
#
#   m <- lapply(ans, function(x) match(x, unique(x)))
#   shift <- c(0L, vapply(m, max, integer(1L))[-length(ans)])
#
#   res <- mapply(\(v, s) {(1L + s) * v}, m, shift)
#   res1 <- apply(res, 1, sum)
#   res2 <- res1 - min(res1) + 1L
#   s <- sort(res2, index.return = T)
#   return(match(s$x, unique(s$x))[order(s$ix)])
# }
#
#
# test2 <- function(...) {
#   x <- do.call("paste", list(..., sep = "|"))
#
#   m <- match(x, unique(x))
#   return(m)
#
# }
#
# library(bench)
#
# mark(
#   as.integer(interaction(ldf$x, ldf$y)),
#   test2(ldf$x, ldf$y),
#   check = F
# )

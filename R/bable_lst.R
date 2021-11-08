
`[.bable` <- function(x, i, j, by, drop = FALSE) {
  browser()
  n_arg <- nargs()
  
  if (n_arg == 1L) return(x)
  
  cl <- class(x)
  out <- as_bable_lst(x)
  env <- parent.frame()
  i_arg <- substitute(i)
  j_arg <- substitute(j)
  by_arg <- substitute(by)
  
  if (i_miss <- missing(i))
    i <- i_arg <- NULL
  
  if (j_miss <- missing(j)) 
    j <- j_arg <- NULL
  
  if (by_miss <- missing(by))
    by <- by_arg <- NULL
  
  # performing a select ----
  if (n_arg <= 2L) {
    out <- out[i]
    return(as_bable(out))
  }
  
  if (!j_miss && is_call(j_arg, c(".","list"))) {
    
  }
  
  mask <- data_mask(out, env)
  attr(out, "row.names") <- .set_row_names(length(out[[1L]]))
  class(out) <- cl
  out
}

as_bable <- function(x, ...) {
  UseMethod("as_bable")
}

as_bable.bable_lst <- function(x, .class = NULL, ...) {
  class(x) <- .class %||% c("bable", "data.frame")
  attr(x, "row.names") <- .set_row_names(length(x[[1L]]))
  x
}

as_bable.data.frame <- function(x, .class = NULL, ...) {
  class(x) <- c(.class, "bable", "data.frame")
  x
}

as_bable_lst <- function(x) {
  UseMethod("as_bable_lst")
}

as_bable_lst.data.frame <- function(x) {
  class(x) <- c("bable_lst", "list")
  attr(x, "row.names") <- NULL
  x
}

#' quick and easy simple subset function variant
#' to `bable` - both i and j must be present
`[.bable_lst` <- function(x, i = NULL, j = NULL) {
  
  if (!is.null(j)) {
    x <- .subset(x, j)
    if (anyNA(.names <- names(x))) {
      x <- .subset(x, !is.na(.names))
      warning("Attempting to select columns that don't exist in `x`:\n ",
              paste(j[is.na(.names)], collapse = ", "), "\n")
    }
  }
  
  if (!is.null(i)) {
    for (xi in seq_along(x)) {
      x[[xi]] <- x[[xi]][i]
    }
  }
  class(x) <- c("bable_lst","list")
  return(x)
}

`[.bable_lst_` <- function(x, i = NULL, j = NULL) {
  
  if (!is.null(j)) {
    x <- .subset(x, j)
    if (anyNA(.names <- names(x))) {
      x <- .subset(x, !is.na(.names))
      warning("Attempting to select columns that don't exist in `x`:\n ",
              paste(j[is.na(.names)], collapse = ", "), "\n")
    }
  }
  
  if (!is.null(i)) {
    x <- lapply(x, `[`, i)
  }
  class(x) <- c("bable_lst_","list")
  return(x)
}






df <- bable(x = 1:20, y = iris, z = x + rnorm(20), y_SW = y$Sepal.Width[x])

df <- expand.grid(x = LETTERS, y = LETTERS) |>
  {\(x) paste0(x$y, x$x)}() |>
  {\(y) {
    setNames(lapply(y,\(x) { sample(1:1e6, 1e4, T)}), y)
  }}() |> {\(x) do.call('bable',x)}()
  
df1 <- as_bable_lst(df)
df2 <- df1
.df <- as.data.frame(df1)
.dt <- as.data.table(.df)
class(df2)[1] <- "bable_lst_"
mark(
  orig_ij = df1[c(5,6,3,19,11), c(1,4,3)],
  appl_ij = df2[c(5,6,3,19,11), c(1,4,3)],
  orig_i = df1[c(5,6,3,19,11)],
  appl_i = df2[c(5,6,3,19,11)],
  .df = .df[c(5,6,3,19,11),,drop = F],
  .dt = .dt[c(5,6,3,19,11),],
  orig_j = df1[,c(1,4,3)],
  appl_j = df2[,c(1,4,3)],
  check = F
)
dfo[,c(1,3)]


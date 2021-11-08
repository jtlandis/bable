
# library(parallel)

#' @export
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

  if (by_miss <- missing(by)) {
    if (is.null(attr(x, "by"))) {
      by <- by_arg <- NULL
    } else {
      by_indx <- attr(x, "by_indices")
    }
  } else {
    by_indx <- do.call('make_groups', out[,eval_select(by_arg, out)])
  }


  # performing a select ----
  if (n_arg <= 2L) {
    i <- eval_select(i_arg, data = out)
    out <- out[,i]
    return(as_bable(out))
  }

  # Inplace Operation `:=` on J
  # or evaluating results of J

  if (!j_miss) {
    # performing a mutate of sorts
    # - cols are retained
    if (is_call(j_arg, ":=")) {

      dots <- j_arg[-1L]
      .names <- names(dots)
      if (!by_miss) {
        .indices <- make_indices(by_indx)
        masks <- lapply(.indices, function(i,x) x[i,], x = out) |>
          lapply(data_mask, .env = env)
        masks <- lapply(masks, eval_mask, .dots = .dots, .names = .names)
      } else {
        .n <- length(out[[1L]])
        .mask <- data_mask(.data, env)
        .mask <- eval_mask(.mask, .dots, .names)
      }

    }
    # performing some kind of
    # selection/summary/transmute

    else {

      }
  }
  # if j is missing and not i
  # performing a filter
  else if (!i_miss) {
    .names <- names(out)
    if (!by_miss) {
      .indices <- make_indices(by_indx)

      out <- lapply(.indices, function(i,x) x[i,], x = out)
      masks <- lapply(out, data_mask, .env = env)
      for (i in seq_along(out)) {
        out[[i]] <- out[[i]][eval(i_arg, masks[[i]]),]
      }
      # out <- mapply(function(m, out, i_arg){
      #   out[eval(substitute(i_arg), m),]
      # }, masks, out, MoreArgs = list(i = i_arg), SIMPLIFY = F)
      .m <- length(out[[1L]])
      out <- lapply(seq_len(.m), function(i) {
        do.call("c",lapply(out, `[[`, i))
      })

      attr(out, "row.names") <- .set_row_names(length(out[[1L]]))
      class(out) <- c("bable", "data.frame")
      names(out) <- .names
      return(out)

    } else {
      .mask <- data_mask(out, env)
      i <- eval(i_arg, .mask)
      out <- out[i,]
      return(as_bable(out))
    }

  }




  if (!j_miss && is_call(j_arg, c(".","list"))) {

  }

  mask <- data_mask(out, env)
  attr(out, "row.names") <- .set_row_names(length(out[[1L]]))
  class(out) <- cl
  out
}

#' @export
as_bable <- function(x, ...) {
  UseMethod("as_bable")
}

#' @export
as_bable.bable_lst <- function(x, .class = NULL, ...) {
  class(x) <- .class %||% c("bable", "data.frame")
  attr(x, "row.names") <- .set_row_names(length(x[[1L]]))
  x
}

#' @export
as_bable.data.frame <- function(x, .class = NULL, ...) {
  class(x) <- c(.class, "bable", "data.frame")
  x
}

#' @export
as_bable_lst <- function(x) {
  UseMethod("as_bable_lst")
}

#' @export
as_bable_lst.data.frame <- function(x) {
  class(x) <- c("bable_lst", "list")
  attr(x, "row.names") <- NULL
  x
}

#' quick and easy simple subset function variant
#' to `bable` - both i and j must be present
#' @export
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
    x <- lapply(x, `[`, i)
  }
  class(x) <- c("bable_lst","list")
  return(x)
}

df <- as_bable(iris)

# df <- bable(x = 1:20, y = iris, z = x + rnorm(20), y_SW = y$Sepal.Width[x])
#
# df <- expand.grid(x = LETTERS, y = LETTERS) |>
#   {\(x) paste0(x$y, x$x)}() |>
#   {\(y) {
#     setNames(lapply(y,\(x) { sample(1:1e6, 1e4, T)}), y)
#   }}() |> {\(x) do.call('bable',x)}()
#
# ldf <- bable(x = sample(1:1e5, 1e6, T),
#              y = sample(LETTERS, 1e6, T))
#



# library(data.table)
# library(bench)
# df1 <- as_bable_lst(df)
# df2 <- df1
# .df <- as.data.frame(df1)
# .dt <- as.data.table(.df)
# class(df2)[1] <- "bable_lst_"
# mark(
#   orig_ij = df1[c(5,6,3,19,11), c(1,4,3)],
#   appl_ij = df2[c(5,6,3,19,11), c(1,4,3)],
#   orig_i = df1[c(5,6,3,19,11)],
#   appl_i = df2[c(5,6,3,19,11)],
#   .df = .df[c(5,6,3,19,11),,drop = F],
#   .dt = .dt[c(5,6,3,19,11),],
#   orig_j = df1[,c(1,4,3)],
#   appl_j = df2[,c(1,4,3)],
#   check = F
# )
# dfo[,c(1,3)]
#
# system.time({
#   df1[c(5,6,3,19,11)]
# })


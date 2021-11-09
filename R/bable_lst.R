
# library(parallel)

#' @export
`[.bable` <- function(x, i, j, by, drop = FALSE) {
  
  n_arg <- nargs()

  if (n_arg == 1L) return(x)

  cl <- class(x)
  out <- as_bable_lst(x)
  env <- parent.frame()
  i_arg <- substitute(i)
  j_arg <- substitute(j)
  by_arg <- substitute(by)
  inplace <- FALSE
  if (i_miss <- missing(i))
    i <- i_arg <- NULL

  if (j_miss <- missing(j))
    j <- j_arg <- NULL

  # performing a select ----
  if (n_arg <= 2L) {
    i <- eval_select(i_arg, data = out)
    out <- out[,i]
    return(as_bable(out))
  }
  #browser()
  if (by_miss <- missing(by)) {
    if (is.null(attr(x, "by"))) {
      by_indx <- by <- by_arg <- NULL
      mask <- data_mask(x, .env = env)
    } else {
      by_indx <- attr(x, "by_indices")
      .indicies <- make_indices(by_indx)
      mask <- .indicies |>
        lapply(function(i,x) x[i,], x = out) |>
        lapply(data_mask, .env = env)
    }
  } else {
    by_indx <- do.call('make_groups', out[,eval_select(by_arg, out)])
    .indicies <- make_indices(by_indx)
    mask <- .indicies |>
      lapply(function(i,x) x[i,], x = out) |>
      lapply(data_mask, .env = env)
  }
  n_groups <- max(by_indx)
  keep_names <- orig_names <- names(out)
  if (!i_miss) {
    if (is_list(mask)) {
      res_i <- vector("list", length = max(by_indx))
      for (ii in seq_len(n_groups)) {
        res_i[[ii]] <- eval(i_arg, mask[[ii]])
        for (nm in orig_names)
          eval(substitute(.x <- .x[.i], list(.x = as.name(nm), .i = res_i[[ii]])), mask[[ii]])
        eval(substitute(.data <- .data[.i,], list(.i = res_i[[ii]])), env_parent(mask[[ii]], 2L))

      } 
    } else {
      i <- eval(i_arg, mask)
      for (nm in orig_names)
        eval(substitute(.x <- .x[.i], list(.x = as.name(nm), .i = i)), mask)
      eval(substitute(.data <- .data[.i,], list(.i = i)), env_parent(mask, 2L))
    } 
  }
  


  

  # Inplace Operation `:=` on J
  # or evaluating results of J

  if (!j_miss) {
    # performing a mutate of sorts
    # - cols are retained
    if (is_call(j_arg, ":=")) {
      inplace <- TRUE

      dots <- as.list(j_arg[-1L])
      .names <- names(dots) %||% character(length(dots))
      if (is_list(mask)) {
        mask <- lapply(mask, eval_mask, .dots = dots, .names = .names)
      } else {
        mask <- eval_mask(mask, dots, .names)
      }
      keep_names <- set_union(orig_names, ls(mask, sorted = F, all.names = TRUE))
      
      # performing some kind of
      # selection/summary/transmute
    } else if (is_call(j_arg, c("list", "."))) {
      browser()
      dots <- as.list(j_arg[-1L])
      .names <- names(dots) %||% character(length(dots))
      mask <- eval_mask(mask, dots, .names)
      .lgl <- vapply(dots, length, integer(1L)) == 1L
      .names[.lgl] <- vapply(dots[.lgl], deparse1, character(1L))
      keep_names <- set_union(.names[nchar(.names)>0], set_diff(ls(mask[[1L]], sorted = TRUE, all.names = TRUE), orig_names))
    }
    else {
      j_res <- eval(j_arg, mask)
      as_bable(j_res)
      }
  }



  if (inplace && (!i_miss)) {
    if (is_list(mask)) {
      .ii <- unlist(mapply(`[`, .indices, res_i, SIMPLIFY = F))
      out[.ii, keep_names] <- lapply(keep_names, function(x, m) { do.call('c', lapply(m, `[[`, x)) }, m = mask)
    } else {
      .ii <- unlist(mapply(`[`, .indices, res_i, SIMPLIFY = F))
      out[.ii, keep_names] <- lapply(keep_names, function(x, m) { m[[x]] }, m = mask)
    }
  } else {
    if (is_list(mask)) {
      out <- lapply(keep_names, function(x, m) { do.call('c', lapply(m, `[[`, x)) }, m = mask)
    } else {
      out <- lapply(keep_names, function(x, m) { m[[x]] }, m = mask)
    }
  }
  
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


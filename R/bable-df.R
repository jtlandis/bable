
new_bable <- function(..., .class = NULL, .check = FALSE, .rownames = NULL) {
  obj <- list(...)
  if (.check) {
    lngths <- vapply(obj, length, integer(1L))
    len_1 <- lngths==1L
    max_len <- max(lngths)
    if (!all(len_1|lngths==max_len))
      stop("Not all elements are of the same length\n")
    if (sum(len_1)>0L)
      obj[len_1] <- lapply(obj[len_1], rep_len, length.out = max_len)
  }
  attr(obj, "row.names") <- if (is.null(.rownames))
    .set_row_names(length(obj[[1]]))
  else
    .rownames
  class(obj) <- c(.class, "bable", "data.frame")
  obj
}

split_df <- function(df, indices, .keep_rows = FALSE) {

  rows <- if (.keep_rows)
    rownames(df)
  else
    seq_len(nrow(df))
  lapply(split(x = seq_len(nrow(df)), f = indices),
         function(ind,  row, data) {
           out <- lapply(df, `[`, ind)
           attr(out, "row.names") <- row[ind]
           class(out) <- "data.frame"
           return(out)
           # do.call("new_eager_df",
           #         c(out,
           #           list(.rownames = row[ind])))
         }, row = rows, data = df)
}

#' @title create a data.frame
#' @description The base type for `bplyr` package.
#' Used to power the S3 methods variants of `dplyr`
#' @export
bable <- function(..., .class = NULL) {

  dots <- match.call(expand.dots = F)$...
  dnms <- ...names()
  if(any(.na <- is.na(dnms))) {
    dnms[.na] <- vapply(dots[.na], deparse1, character(1))
  }
  out <- list()
  env <- parent.frame()
  for (i in seq_len(...length())) {
    out[[dnms[i]]] <- eval(dots[[i]], envir = out, env)
  }


  out <- lapply(out, function(x) if (inherits(x, "data.frame")) list(x) else x)
  lgnths <- vapply(out, length, integer(1))
  max_size <- max(lgnths)
  failed_dots <- !lgnths %in% c(1L, max_size)
  if (any(failed_dots)) {
    stop(call. = F,
         "The following expressions either need to evaluate to length 1 or the max size in ... (",max_size,"):\n",
         paste0("\tlength: ",lgnths[failed_dots], "  ",vapply(dots[failed_dots], function(x){
           val <- deparse(x, 35L)
           if (length(val)>1){
             val <- paste0(val[1L],"...")
           }
           return(val)},
           character(1)),"\n"))
  }

  len_1 <- max_size!=1L & lgnths==1L
  if (any(len_1)) {
    out[len_1] <- lapply(out[len_1], rep_len, max_size)
  }

  attr(out, "row.names") <- .set_row_names(max_size)
  attr(out, "names") <- names(dots)
  class(out) <- c(.class, "bable", "data.frame")

  return(out)

}




print.bable <- function(x, n = 12, width = 50, min = 4, max = 10) {

  format(x, n, width)

}


format.bable <- function(x, n, width) {

  browser()

  obs <- nrow(x)
  col <- length(x)
  info_header <- paste(sep = "", "A bable with ", obs, " observation", if(obs>1) "s", " and ", col, " column", if(col>1) "s", "\n")
  cat(info_header)
  cl <- class(x)
  nrows <- nrow(x)
  short_vis <- F
  if (nrows<n) {
    short_vis <- T
    n <- nrows
  }
  x <- as.list(x)

  widths <- cumsum(nchar(names(x))+2) + 4
  est_cols <- seq_len(max(which(widths<width)))
  avail_width <- width - 4
  avail_per <- avail_width %/% max(est_cols)

  col_nms <- names(x)[est_cols]
  col_cls <- mapply(class_abbrv,x = x, width = avail_per)

  if (short_vis) {

    top_half <- lapply(x[est_cols], `[`, seq_len(n))
    top_half <- lapply(top_half, function(x) {
      if(inherits(x, "list"))
        format(x, width = )
    })


  }

  dat_vis <- lapply(x[est_cols], `[`, seq_len(if(short_vis) n else n %/% 2))

  lst <- vector('list', length(dat_vis))
  nms <- names(dat_vis)
  for (i in seq_len(max(est_cols))) {
    dat <- dat_vis[[i]]
    if (inherits(dat, 'list')) {
      dat <- format(dat, width = min(c(width, nchar(header))))
    } else {
      dat <- format(dat)
    }
    lst[[i]] <- format(c(nms[i], dat))
  }


  rows <- format(c("", paste0(rownames(dat_vis))))
  colon <- format(c("", rep(": ", nrow(dat_vis))))


  out <- c(list(paste0(rows, colon)), lst)
  out[[length(out)]] <- paste0(out[[length(out)]],"\n")

  chr <- do.call(paste, c(out, list(sep = " ")))

  cat(chr, sep ="")
  if (nrows>nrow(dat_vis)){
    cat("...\n")
  }


}


format.list <- function(x, ..., width = 6) {
  vapply(x, class_abbrv, FUN.VALUE = character(1), width = width)
}


class_abbrv <- function(x, ...) {
  UseMethod("class_abbrv")
}

class_abbrv.default <- function(x, width = 6L) paste0("<",abbreviate(class(x)[1L], minlength = width-2L),">")
class_abbrv.double <- function(x, ...) "<dbl>"
class_abbrv.integer <- function(x, ...) "<int>"
class_abbrv.factor <- function(x, ...) "<fct>"
class_abbrv.character <- function(x, ...) "<chr>"
class_abbrv.list <- function(x, ...) "<lst>"




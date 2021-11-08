
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



#' @export
print.bable <- function(x, n = 12, width = 100, min = 4, max = 10) {

  format(x, n, width)

}


format.bable <- function(x, n = 12, width = 80L) {

  .dim <- dim(x)
  .obs <- .dim[1L]
  .col <- .dim[2L]
  .nms <- names(x)
  cat("# A bable with ", .obs,
      " observation", if(.obs>1L) "s" else "",
      " and ", .col, " column",
      if(.col>1L) "s" else "", "\n", sep = "")


  short_vis <- FALSE
  if (.obs <= n) {
    short_vis <- TRUE
    n <- .obs
  }

  lst <- as.list(x)

  if (short_vis) {
    data_sub <- lst
    .rows <- format(c("","", paste0(seq_len(n), ": ")), justify = "right")
  } else {
    head_n <- n %/% 2
    tail_n <- n - head_n
    tail_seq <- rev(seq_len(tail_n))
    data_sub <- lapply(lst, `[`, c(seq_len(head_n), .obs + 1L - tail_seq))
    .rows <- format(c("", "",
                      paste0(
                        format(c(seq_len(head_n),
                                 format(c(paste0("n-", tail_seq[-1L]), "n"), justify = "left")),
                               justify = "right"),
                        ":"
                      )), justify = "right")
  }

  w_avail <- width - max(nchar(.rows)) - 1L

  out <- list()

  for (i in seq_len(.col)) {
    out[[.nms[i]]] <- format(c(.nms[i], class_abbrv(data_sub[[i]]),
                  format(data_sub[[i]], justify = "right")), justify = "right")
    w_avail <- w_avail - max(nchar(out[[i]])) - 2L

    if (w_avail < 0) {
      out <- out[-length(out)]
      break
    }
  }



  body <- c(list(.rows), out)

  if (!short_vis) {
    body <- lapply(body,
                   function(x, after) {
                     format(c(x[1L:after],
                              "...", x[(after + 1L):(length(x))]),
                            justify = "centre")},
                   after = head_n + 2L)
  }

  body <- do.call("paste", c(body, list(sep = "  ", collapse = "\n")))

  cat(body, "\n")

  footer_data <- data_sub[setdiff(.nms, names(out))]
  .remain <- length(footer_data)
  if (.remain > 0) {
    footer <- paste0("# ... with ", .remain, " more variable", if(.remain>1) "s" else "", ":\n# ")
    footer <- c(footer,
                paste(names(footer_data), vapply(footer_data, class_abbrv, character(1)), collapse = ", "))
    cat(footer, "\n")
  }


  invisible(x)

}




format.list <- function(x, ..., width = 6) {
  vapply(x, class_abbrv, FUN.VALUE = character(1), width = width)
}

#' @export
class_abbrv <- function(x, ...) {
  UseMethod("class_abbrv")
}

#' @export
class_abbrv.default <- function(x, width = 6L) paste0("<",abbreviate(class(x)[1L], minlength = width-2L),">")
#' @export
class_abbrv.double <- function(x, ...) "<dbl>"
#' @export
class_abbrv.integer <- function(x, ...) "<int>"
#' @export
class_abbrv.factor <- function(x, ...) "<fct>"
#' @export
class_abbrv.character <- function(x, ...) "<chr>"
#' @export
class_abbrv.list <- function(x, ...) "<lst>"
#' @export




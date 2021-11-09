
`%||%` <- function(a, b) if (is.null(a)) b else a

data_mask <- function(.data, .env = parent.frame()){
  .env <- force(.env)
  .data <- as.list(.data)
  bot <- new.env(parent = .env)
  list2env(list(.data = .data, .env = .env, .fn_across = NULL), envir = bot)
  mid <- new.env(parent = bot)
  list2env(
    x = list(across = function(.cols = everything(), .fns = NULL, ..., .names = NULL) {
      browser()
      if (is.null(.fns)) {
        return()
      } else if (is.function(.fns)) {
        fn_nms <- NULL
        .fns <- list(.fns)
      } else {
        fn_nms <- names(.fns) %||% seq_along(.fns)
      }
      dots <- as.list(substitute(.(...))[-1L])
      .cols <- substitute(.cols)
      cols <- eval_select(.cols, as.list(top, T, T))
      n_col <- length(cols)
      n_fun <- length(.fns)

      n_cols <- length(cols)
      n_funs <- length(.fns)
      col_nm <- names(cols)

      .names <- substitute(.names)

      .names <- if (is.null(fn_nms)&&is.null(.names))
        col_nm
      else {
        .grid <- expand.grid(.fn = fn_nms, .col = col_nm)
        if (is.null(.names)){
          paste(.grid$.col, .grid$.fn, sep = "_")
        } else {
          eval(substitute(.names), .grid)
        }
      }
      k <- 1L
      for ( i in seq_len(n_cols)) {
        fn_call <- do.call('call', c(list('.fn_across', as.name(col_nm[[i]])), dots), quote = T)
        for (j in seq_len(n_funs)) {
          bot$.fn_across <- .fns[[j]]
          eval(substitute(`<-`(.x, .y), list(.x = .names[k], .y = fn_call)), envir = top)
          k <- k + 1L
        }
      }
      
      on.exit(substitute(poke_names(.n, .i), list(.n = .names, .i = quote(i))))


    }),
    envir = mid)
  top <- new.env(parent = mid)
  list2env(.data, top)
  top
}

eval_mask <- function(.mask, .dots, .names) {
  setup_names(length(.dots))
  if (is_list(.mask)) {
    seq_m <- seq_along(.mask)
    for (i in seq_along(.dots)) {
      for (j in seq_m) {
        if (nchar(.names[i])==0) {
          eval(.dots[[i]], envir = .mask[[j]])
        } else {
          eval(substitute(`<-`(.x, .y), list(.x = .names[i], .y = .dots[[i]])), envir = .mask[[j]])
          poke_name(.names[i], i)
        }
      }
    }
  } else {
    for (i in seq_along(.dots)) {
      if (nchar(.names[i])==0) {
        eval(.dots[[i]], envir = .mask)
      } else {
        eval(substitute(`<-`(.x, .y), list(.x = .names[i], .y = .dots[[i]])), envir = .mask)
        poke_name(.names[i], i)
      }
    }
  }
  return(.mask)
}

make_groups <- function(...) {
  x <- do.call("paste", list(..., sep = "|"))
  m <- match(x, unique(x))
  return(m)

}

make_indices <- function(x) {
  #on <- as.integer(as.factor(on))
  N <- length(x)
  if (is.character(x)) {
    x <- match(x, unique(x))
  }
  x_sorted <- sort(x, index.return = T)
  lgl <- x_sorted$x[-1L] != x_sorted$x[-N]
  indx <- c(which(lgl), N)
  len <- diff(c(0L, indx))
  #keys <- x_sorted$x[indx]

  start <- cumsum(c(0L, len[-length(indx)]))

  return(Cindicies(x_sorted$ix, start, len))


}

#' @export
#' @useDynLib bable Cindicies_
Cindicies <- function(ind, starts, len) {
  .Call(Cindicies_, ind, starts, len)
}

mutate2 <- function(.data, ...) {
  browser()
  dots <- as.list(substitute(.(...))[-1L])
  .n <- nrow(.data)
  mask <- data_mask(.data)
  .names <- ...names()
  for( i in seq_len(...length())) {
    if (is.na(.names[i]))
      eval(dots[[i]], envir = mask)
    else
      eval(substitute(`<-`(.x, .y), list(.x = .names[i], .y = dots[[i]])), envir = mask)
  }

  res <- as.list(mask, T, T)

  attr(res, "row.names") <- .set_row_names(.n)
  class(res) <- "data.frame"
  return(res)

}


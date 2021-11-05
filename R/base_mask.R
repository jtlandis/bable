
`%||%` <- function(a, b) if (is.null(a)) b else a

data_mask <- function(.data, .env = parent.frame()){
  .env <- force(.env)
  .data <- as.list(.data)
  bot <- new.env(parent = .env)
  list2env(list(.data = .data, .env = .env, .fn_across), envir = bot)
  mid <- new.env(parent = bot)
  list2env(
    x = list(across = function(.cols = everything(), .fns = NULL, ..., .names = NULL) {
      
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
      cols <- do.call('eval_select', list(.cols, as.list(top, T, T)))
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
      
      
    }),
    envir = mid)
  top <- new.env(parent = mid)
  list2env(.data, top)
  top
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


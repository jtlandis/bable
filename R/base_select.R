

eval_select <- function(expr, data) {
  mask <- new_select_mask(data)
  expr <- force(expr)
  vals <- eval(expr, envir = mask)
  return(vals)
}


new_select_mask <- function(data) {
  data <- as.list(data)
  nms <- names(data)
  if (is.null(nms)||any(nchar(nms)==0)) stop ("all elements of `data` must be named for `eval_select`", call. = F)
  bot <- new.env()
  mid <- new.env(parent = bot)
  top <- new.env(parent = mid)
  list2env(
    envir = mid,
    x = list(
      vars = data
    )
  )
  list2env(envir = bot,
           x = list(
             .selection = NULL,
             everything = function(){
               val <- seq_along(mid$vars)
               setNames(val, names(mid$vars))
             },
             all_of = function(x) {
               .nms <- names(mid$vars)
               if (!all(x %in% .nms)) stop("some value of `x` is not present.")
               val <- which(.nms %in% x)
               val
             },
             any_of = function(x) {
               val <- which(names(mid$vars) %in% x)
               if (length(val)==0) stop("No values matching `x`")
               val
             },
             contains = function(match, ignore.case = TRUE) {
               .nms <- names(mid$vars)
               val <- which(grepl(match, x = .nms, ignore.case = ignore.case))
               if (length(val)==0) stop("No values matching `x`")
               val
             },
             starts_with = function(match, ignore.case = TRUE) {
               .nms <- names(mid$vars)
               val <- which(grepl(paste0("^", match), x = .nms, ignore.case = ignore.case))
               if (length(val)==0) stop("No values matching `x`")
               val
             },
             ends_with = function(match, ignore.case = TRUE) {
               .nms <- names(mid$vars)
               val <- which(grepl(paste0(match, "$"), x = .nms, ignore.case = ignore.case))
               if (length(val)==0) stop("No values matching `x`")
               val
             },
             last_col = function(offset = 0L) {
               n <- length(mid$vars)
               n - offset
             },
             `:` = function(e1, e2) {
               e1 <- deparse(substitute(e1))
               e2 <- deparse(substitute(e2))
               .nms <- names(mid$vars)
               e1 <- match(e1, .nms)
               e2 <- match(e2, .nms)
               if (length(c(e1,e2))!=2L||any(is.na(c(e1,e2)))) stop("could not find one of the values")
               seq(e1, e2)
             },
             `&` = function(e1, e2) {
               set_intersect(e1, e2)
             },
             `|` = function(e1, e2) {
               set_union(e1, e2)
             },
             `!` = function(e1) {
               set_diff(bot$everything(), e1)
             },
             #The only operator that needs to know about the
             #current selection...
             `-` = function(e1, e2) {
               if (base::`!`(missing(e2))) stop("`-` in tidyselect is only used as a unary operator.")
               res <- set_diff(bot$.selection %||% bot$everything(), e1)
               bot$.selection <- res
               res
             },
             c = function(...) {
               all_names <- names(mid$vars)
               lst <- enQuos(...)
               .name <- names(lst)
               cur_sel <- bot$.selection
               bot$.selection <- NULL
               for (i in seq_along(lst)) {
                 res <- eval(substitute(sel_expr, list(sel_expr = lst[[i]])), envir = top)
                 names(res) <- if(is.null(.name[i])||nchar(.name[i])==0)
                   all_names[res]
                 else if (length(res) == 1L)
                   .name[i]
                 else
                   paste0(.name[i], seq_along(res))
                 bot$.selection <- set_union(bot$.selection, res)
               }
               out <- bot$.selection
               #Set to NULL for nested c(...) calls
               bot$.selection <- NULL
               set_union(cur_sel, out)
             },
             where = function(fn) {
               val <- which(vapply(mid$vars, fn, FUN.VALUE = logical(1L)))
               val
             }
           ))
  data_indx <- seq_along(data)
  list2env(setNames(split(data_indx, data_indx), nms),
           envir = top)
  top
}

set_union <- function(x, y) {
  if(is.null(x)&&is.null(y)) return(NULL)
  if(is.null(x)) return(y)
  if(is.null(y)) return(x)
  y_i <- !y %in% x
  vec <- c(x, y[y_i])
  names(vec) <- c(names(x), names(y)[y_i])
  vec

}

set_intersect <- function(x, y) {
  if(is.null(x)||is.null(y)) return(NULL)
  x_i <- x %in% y
  vec <- x[x_i]
  names(vec) <- names(x)[x_i]
  vec
}

set_diff <- function(x, y) {
  if(is.null(x)) return(NULL)
  if(is.null(y)) return(x)
  x_i <- ! x %in% y
  vec <- x[x_i]
  names(vec) <- names(x)[x_i]
  vec
}

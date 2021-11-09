
name_env <- new.env(parent = emptyenv())

clear_names <- function() {
  name_env[["names"]] <- NULL
}

setup_names <- function(n) {
  name_env[["names"]] <- vector('list', n)
}

poke_names <- function(x, i) {
  name_env[["names"]][i] <- x
}

get_names <- function() {
  name_env[["names"]]
}

is_call <- function(x, call = NULL) {
  res <- inherits(x, "call")
  if (is.null(call)) {
    return(res)
  } else {
    return(res && as.character(x[[1L]]) %in% call)
  }
}

is_list <- function(x) inherits(x, "list")


env_parent <- function(env = parent.frame(), n = 1L) {
  
  while (n > 0) {
    if (identical(emptyenv(), env)) stop("The envir has no parent")
    
    n <- n - 1L
    env <- parent.env(env)
  }
  env
}

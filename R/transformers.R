#' @export
#' @title geometric weight function
#' @description  used for \link{adstock} function
#' @param l integer. lag index. 
#' @param alpha numeric. It means the decay rate parameter, which must be larger than zero (0) and less than one (1).
#' @param theta numeric. It means the delay parameter, which must be larger than or equatl zero (0) and less than or equalt maximum lag minus one (1).
geo_weight <- function(l, alpha, theta){
  alpha ^ ((l - theta) ^ 2)
}

#' @export
#' @title adstock smoothing function
#' @param x numeric vector. It indicates the amount of spendings, all of which elements must be non-negative.
#' @param weights function. typically, you can use \link{get_weight} function.
#' @param L integer. maximum lag of carryover effect. 
#' @param args list. extra arguments for `weights` function.
#' @param na.rm logical. default is `TRUE`.
adstock <- function(x, weights, L, args, na.rm = T){
  args$l <- 1:L
  w <- do.call(weights, args)
  sum(w * x[1:L], na.rm) / sum(w, na.rm)
}

#' @export
#' @title hill function
#' @param x numeric vector. It indicates the amount of spendings, all of which elements must be non-negative.
#' @param k numeric. It means the half-satuation point parameter, which must be non-negative.
#' @param s numeric. It means the [s]lope or [s]hape parameter, which must be non-negative.
hill <- function(x, k, s){
  1 / (1 + (x / k) ^ -s)
}

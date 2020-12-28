#' Compounded addition
#' @export
#' @param x The object to increment.
#' @param y The value by which to increment.
`%+%` <- function(x, y){
    eval.parent(substitute(x <- x + y))
}

#' Compounded multiplication
#' @export
#' @param x The object to increment.
#' @param y The value by which to increment.
`%**%` <- function(x, y){
    eval.parent(substitute(x <- x * y))
}


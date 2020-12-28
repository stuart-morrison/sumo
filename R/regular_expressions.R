#' SQL-like like wildcard
#' @export
#' @param x The item to match
#' @param pattern The matching regex pattern
#' @return A vector of logicals.
`%like%` <- function(x, pattern) {
    return(grepl(pattern, x))
}

#' SQL-like not-like wildcard
#' @export
#' @param x The item to match
#' @param pattern The matching regex pattern
#' @return A vector of logicals.
`%notlike%` <- function(x, pattern) {
    return(!grepl(pattern, x))
}

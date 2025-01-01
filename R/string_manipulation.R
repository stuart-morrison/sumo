#' Left-right string interpolation with formatting options of sprintf
#' @export
#' @param x The string to interpolate into.
#' @param y A list to interpolate into \code{x}
#' @return The interpolated \code{x}.
`%fmt%` <- function(x, y) {
    if (!is.list(y) & !is.atomic(y)) {
        stop("Right hand side must be a list or vector object.")
    }

    if (is.atomic(y)) {
        y <- as.list(y)
    }

    do.call(sprintf, append(list(x), y))
}

#' Binary string concatenation
#' @export
#' @param x The left-hand string to paste.
#' @param y The right-hand string to paste.
`%&%` <- function(x, y) {
    return(paste0(x, y))
}

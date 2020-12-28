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

#' Function to create regex to gsub replacement before a symbol
#' @export
#' @param string The string to replace in.
#' @param symbol The symbol to find in \code{string}
#' @param replacement The replacement of \code{string} before \code{symbol}.
#' @param ignore.case If FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#' @param perl Logical. Should Perl-compatible regexps be used?
#' @param fixed Logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param useBytes Logical. If TRUE the matching is done byte-by-byte rather than character-by-character. See ‘Details’.
replace_before <- function(string, symbol, replacement,
                           ignore.case = FALSE, perl = FALSE,
                           fixed = FALSE, useBytes = FALSE) {
    # Create the regex pattern
    regex_pattern <- paste0(".*", symbol)

    # replace string before the pattern
    gsub(pattern = regex_pattern, replacement = replacement,
         x = string, ignore.case = ignore.case, perl = perl,
         fixed = fixed, useBytes = useBytes)
}

#' Function to create regex to gsub replacement after a symbol
#' @export
#' @param string The string to replace in.
#' @param symbol The symbol to find in \code{string}
#' @param replacement The replacement of \code{string} after \code{symbol}.
#' @param ignore.case If FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#' @param perl Logical. Should Perl-compatible regexps be used?
#' @param fixed Logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @param useBytes Logical. If TRUE the matching is done byte-by-byte rather than character-by-character. See ‘Details’.
replace_after <- function(string, symbol, replacement,
                          ignore.case = FALSE, perl = FALSE,
                          fixed = FALSE, useBytes = FALSE) {
    # Create the regex pattern
    regex_pattern <- paste0(symbol, ".*")

    # replace string before the pattern
    gsub(pattern = regex_pattern, replacement = replacement,
         x = string, ignore.case = ignore.case, perl = perl,
         fixed = fixed, useBytes = useBytes)
}

#' Binary string concatenation
#' @export
#' @param x The left-hand string to paste.
#' @param y The right-hand string to paste.
`%&%` <- function(x, y) {
    return(paste0(x, y))
}



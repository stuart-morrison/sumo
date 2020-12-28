#' Evaluate the proportion based on a grouped variable
#' @export
#' @param x The vector to find the element-wise proportion
#' @param na_rm Remove NA values before evaluating the proportion?
#' @return A numeric vector that shows the element-wise proportion of \code{x}.
proportion <- function(x, na_rm = TRUE) {
    if (sum(x, na.rm = na_rm) == 0) {
        return(rep(NA_real_, length(x)))
    } else {
        return(x / sum(x, na.rm = na_rm))
    }
}

#' Convert \code{NaN} to \code{NA} in vectors
#' @export
#' @param x The vector in which to replace \code{NaN} values.
#' @param na_type The NA type to replace \code{NaN} values with, eg: \code{NA}; \code{NA_real_}; etc.
#' @return A vector that has \code{NA} values replaced..
nan_to_na <- function(x, na_type = NULL) {
    if (!is.null(na_type)) {
        if (is.na(na_type)) {
            x[is.nan(x)] <- na_type
        } else {
            stop("Argument na_type should be an NA value.")
        }
    } else {
        x[is.nan(x)] <- NA
    }

    return(x)
}

#' Convert excel number to a date
#' @export
#' @param date The numeric excel code to convert to a date object.
#' @return A vector of date type.
excel_date <- function(date) {
    x <- as.Date(as.numeric(date), origin = "1899-12-30")
    return(x)
}

#' Convert excel number to a datetime
#' @export
#' @importFrom lubridate as_datetime
#' @param date The numeric excel code to convert to a datetime object.
#' @return A vector of datetime type.
excel_datetime <- function(date) {
    x <- as_datetime(x = date * (60 * 60 * 24), origin = "1899-12-30", tz = "UTC")
    return(x)
}

#' I'm sick of typing length(unique(x))
#' @export
#' @param x The vector to find the number of unique elements over.
#' @return An integer of the number of unique elements in \code{x}.
len_uni <- function(x) {
    return(length(unique(x)))
}

#' Count the number of NA in an object
#' @export
#' @param x The vector the calculate the number of NA elements
#' @return An integer of the number of \code{NA} values in \code{x}.
sum_na <- function(x) {
    return(sum(is.na(x)))
}

#' Show the subset of \code{NA} in an object
#' @export
#' @param x The vector to identify \code{NA} elements in
#' @return A integer vector of indices of \code{x} with \code{NA} elements
which_na <- function(x) {
    return(which(is.na(x)))
}

#' Opposite of \code{in}
#' @export
#' @param x The element to match.
#' @param table The set to match in.
#' @return A logical vector showing whether \code{x} is not in \code{table}.
`%not in%` <- function(x, table) {
    return(!x %in% table)
}


#' Infix operator to swap the values of variables
#' @export
#' @param x The first element to swap
#' @param y The second element to swap
`%swap%` <- function(x, y) {
    temp_x <- x
    temp_y <- y

    eval.parent(substitute(x <- temp_y))
    eval.parent(substitute(y <- temp_x))
}

#' Large number syntax divider
#' @export
#' @param ... A set of strings of numerics with punctuation marks
#' @return A numeric vector.
#' @examples
#' as_num("123_456.78", "1-000-000")
as_num <- function(...) {
    collector <- list(...)
    return(as.numeric(gsub(x = collector, pattern = "(?!\\.)[[:punct:]]",
                           replacement = "", perl = TRUE)))
}

#' Find the closest match in another vector
#' @export
#' @param x The element vector to look up.
#' @param table The vector of elements in which to find the closest match.
#' @return The element from \code{table} that is closest in least absolute terms in \code{x}.
closest <- function(x, table) {

    # Function that returns for one element in x
    return_closest <- function (x, table) {
        return(table[which.min(x = abs(x - table))])
    }

    # Apply over all of x
    vapply(X = x, FUN = return_closest, FUN.VALUE = as.numeric(length(x)), table)
}

#' Create groups if values are consecutive
#' @export
#' @param x The vector over which to create groups.
#' @return A integer vector showing the consecutive groups of \code{x}.
group_if_consecutive <- function(x) {
    return(cumsum(c(1, abs(x[-length(x)] - x[-1]) > 1)))
}

#' Round down
#' @export
#' @param x The value to round down.
#' @param n The base to round down to.
#' @return An integer of the rounded down \code{x} to the nearest \code{n}.
round_down_to_nearest <- function(x, n) {
    return(x %/% n * n)
}

#' Round up
#' @export
#' @param x The value to round up.
#' @param n The base to round up to.
#' @return An integer of the rounded up \code{x} to the nearest \code{n}.
round_up_to_nearest <- function(x, n) {
    return((x %/% n + 1) * n)
}

#' Round to nearest
#' @export
#' @param x The value to round.
#' @param n The base to round to.
#' @return An integer of the rounded \code{x} to the nearest \code{n}.
round_to_nearest <- function(x, n) {
    up <- (x %/% n + 1) * n
    down <- (x %/% n + 1) * n
    if (abs(up - x) < abs(down - x)) {
        output <- up
    } else {
        output <- down
    }
    return(output)
}

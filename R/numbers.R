#' Factors of a number
#' @export
#' @param x The integer to find the factors of.
#' @param include_original Should \code{x} by contained in the output?
#' @return A vector of integers of the factors of \code{x}.
factors <- function(x, include_original = FALSE) {
    if (include_original) {
        return(c((1:(x / 2))[(x %% 1:(x / 2)) == 0], x))
    } else {
        return((1:(x / 2))[(x %% 1:(x / 2)) == 0])
    }
}

#' Convert an base 26 string to an integer
#' @export
#' @param x The base 26 string to convert to base 10
#' @return An integer  of \code{x} in base 10.
base_26_calc <- function(x) {

    x <- tolower(x)
    do_the_splits <- strsplit(x = x, split = "")[[1]]
    numbers_and_letters <- match(do_the_splits, letters)

    this_column_is <- sum(numbers_and_letters * (26 ^ rev((1:length(numbers_and_letters)) - 1)))

    return(this_column_is)
}

#' Iteratively calculate the digital sum of a number
#' @export
#' @param x The integer to calculate the digital sum of
#' @return An integer of the digitical sum of \code{x}.
digital_sum <- function(x) {
    temp <- x

    while (temp >= 10) {
        digits <- ceiling(log10(temp + 1))
        temp <- sum((temp %% (10 ^ (1:digits))) %/% (10 ^ (0:(digits - 1))))
    }

    return(temp)
}

#' Dice roll simulator
#' @export
#' @importFrom stats rmultinom
#' @param sides An integer on each of the dice.
#' @param times An integer on how many times to roll the dice.
#' @param dice An integer on the number of dice to roll
#' @return A list of vectors for each of the dice that have been rolled.
dice_roller <- function(sides = 6, times = 1, dice = 1) {
    if (dice > 1) {
        return(lapply(X = 1:dice, FUN = function(x) unlist(lapply(1:times, FUN = function(x) which(rmultinom(1, 1, rep((1 / sides), sides)) == 1)))))
    } else {
        return(unlist(lapply(1:times, FUN = function(x) which(rmultinom(1, 1, rep((1 / sides), sides)) == 1))))
    }
}

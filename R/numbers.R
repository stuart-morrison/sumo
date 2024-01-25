
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


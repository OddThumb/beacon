# 1. Calculation ----

#' Calculate maximum of absolute values
#'
#' @param xA numeric vector.
#' @param na.rm A logical (default: TRUE). Whether remove NA values or not.
#' @return A numeric.
#' @export
amax <- function(x, na.rm = T) {
    max(abs(x), na.rm = na.rm)
}

#' Calculate minimum of absolute values
#'
#' @param x A numeric vector.
#' @param na.rm A logical (default: TRUE). Whether remove NA values or not.
#' @return A numeric.
#' @export
amin <- function(x, na.rm = T) {
    min(abs(x), na.rm = na.rm)
}

#' Calculate the order of magnitude
#'
#' @param x A numeric vector.
#' @return A numeric.
#' @export
get.order <- function(x) {
    10^(floor(log10(amax(x))))
}


#' Get limit range from ts by 110 % of maximum value
#'
#' @param x A numeric vector.
#' @param mar.frac A numeric. A margin. This will be multiplied on amax(x).
#' @return A vector. c(-amax, +amax)*mar.frac.
#' @export
get.limit <- function(x, mar.frac = 1.5) {
    c(-amax(x), amax(x)) * mar.frac
}

#' Standard Error for sample
#'
#' @param x A numeric vector.
#' @return A numeric. A sampled standard error.
#' @export
se <- function(x) {
    sd(x) / sqrt(length(x) - 1)
}

#' Median Absolute Error for sample
#'
#' @param x A numeric vector.
#' @return A numeric. A sampled median absolute error.
#' @export
mae <- function(x) {
    mad(x) / sqrt(length(x) - 1)
}

#' Mode
#' @description
#' Get the most frequent value.
#' @param x A numeric vector
#' @return A numeric. A mode value.
mode <- function(x) {
    uniqx <- unique(x)
    uniqx[which.max(tabulate(match(x, uniqx)))]
}

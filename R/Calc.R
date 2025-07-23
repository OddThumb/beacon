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
get_order <- function(x) {
    10^(floor(log10(amax(x))))
}


#' Get limit range from ts by 110 % of maximum value
#'
#' @param x A numeric vector.
#' @param mar.frac A numeric. A margin. This will be multiplied on amax(x).
#' @return A vector. c(-amax, +amax)*mar.frac.
#' @export
get_limit <- function(x, mar.frac = 1.5) {
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


#' Wrapper of unique(diff(x))
#'
#' @export
uniqdif <- function(x, tol = 1e-8) {
    dx <- diff(x)
    ref <- dx[1]
    unique_dx <- unique(dx[abs(dx - ref) > tol])
    if (length(unique_dx) == 0) {
        return(ref)
    } else {
        warning("Non-uniform spacing detected.")
        return(unique(dx))
    }
}

#' Simple initial value
#'
#' @param x A vector.
#' @param n A numeric (default: 1). A number of head.
#' @return Numeric(s). n initial times.
#' @export
vi <- function(x, n = 1) {
    head(x, n)
}

#' Simple final value
#'
#' @param x A vector.
#' @param n A numeric (default: 1). A number of tail.
#' @return Numeric(s). n final times.
#' @export
vf <- function(x, n = 1) {
    tail(x, n)
}

#' Simple value range
#'
#' @param x A vector.
#' @return A vector of length 2 which is equivalent to `c(ti(ts), tf(ts))`.
#' @export
vr <- function(x) {
    c(vi(x), vf(x))
}

#' Simple value length from first data point to last.
#'
#' @param ts A time series (`ts`) object.
#' @return A vector of length 2 which is equivalent to `c(ti(ts), tf(ts))`.
#' @export
vl <- function(x) {
    diff(vr(x))
}

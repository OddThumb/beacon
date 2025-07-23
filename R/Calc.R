#' Calculate maximum of absolute values
#'
#' @param x A numeric vector.
#' @param na.rm A logical (default: TRUE). Whether to remove NA values.
#' @return A numeric. The maximum of absolute values.
#' @export
amax <- function(x, na.rm = T) {
    max(abs(x), na.rm = na.rm)
}

#' Calculate minimum of absolute values
#'
#' @param x A numeric vector.
#' @param na.rm A logical (default: TRUE). Whether to remove NA values.
#' @return A numeric. The minimum of absolute values.
#' @export
amin <- function(x, na.rm = T) {
    min(abs(x), na.rm = na.rm)
}

#' Calculate the order of magnitude
#'
#' @param x A numeric vector.
#' @return A numeric. The order of magnitude of the maximum absolute value.
#' @export
get_order <- function(x) {
    10^(floor(log10(amax(x))))
}


#' Get limit range from ts by scaled maximum value
#'
#' @param x A numeric vector.
#' @param mar.frac A numeric. Scaling factor for the margin. Default is 1.5.
#' @return A vector. c(-amax, +amax) * mar.frac.
#' @export
get_limit <- function(x, mar.frac = 1.5) {
    c(-amax(x), amax(x)) * mar.frac
}

#' Standard Error for sample
#'
#' @param x A numeric vector.
#' @return A numeric. Sample standard error.
#' @export
se <- function(x) {
    sd(x) / sqrt(length(x) - 1)
}

#' Median Absolute Error for sample
#'
#' @param x A numeric vector.
#' @return A numeric. Sample median absolute error.
#' @export
mae <- function(x) {
    mad(x) / sqrt(length(x) - 1)
}

#' Mode
#'
#' @description
#' Get the most frequent value in the input vector.
#' @param x A numeric vector.
#' @return A numeric. The mode value.
#' @export
mode <- function(x) {
    uniqx <- unique(x)
    uniqx[which.max(tabulate(match(x, uniqx)))]
}


#' Wrapper of unique(diff(x)) with tolerance check
#'
#' @param x A numeric vector.
#' @param tol A numeric tolerance for detecting non-uniform spacing. Default is 1e-8.
#' @return A numeric value if uniform, or a vector of unique differences otherwise.
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

#' Simple initial value extractor
#'
#' @param x A vector.
#' @param n A numeric (default: 1). Number of initial values to extract.
#' @return A vector of the first n elements.
#' @export
vi <- function(x, n = 1) {
    head(x, n)
}

#' Simple final value extractor
#'
#' @param x A vector.
#' @param n A numeric (default: 1). Number of final values to extract.
#' @return A vector of the last n elements.
#' @export
vf <- function(x, n = 1) {
    tail(x, n)
}

#' Simple range extractor
#'
#' @param x A vector.
#' @return A vector of length 2, containing the first and last element.
#' @export
vr <- function(x) {
    c(vi(x), vf(x))
}

#' Length from first to last value
#'
#' @param x A vector.
#' @return A numeric. Difference between last and first element.
#' @export
vl <- function(x) {
    diff(vr(x))
}

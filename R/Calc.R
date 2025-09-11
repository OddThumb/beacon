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
    utils::head(x, n)
}

#' Simple final value extractor
#'
#' @param x A vector.
#' @param n A numeric (default: 1). Number of final values to extract.
#' @return A vector of the last n elements.
#' @export
vf <- function(x, n = 1) {
    utils::tail(x, n)
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

#' Arithmetic Mean Ignoring NA
#'
#' @description
#' Compute the arithmetic mean of a numeric vector, ignoring \code{NA} values.
#'
#' @param x A numeric vector.
#'
#' @return A numeric scalar representing the mean.
#'
#' @export
ari_mean <- function(x) {
    mean(x, na.rm = T)
}

#' Harmonic Mean Ignoring NA
#'
#' @description
#' Compute the harmonic mean of a numeric vector, ignoring \code{NA} values.
#'
#' @param x A numeric vector.
#' @param na.rm Logical (default \code{TRUE}). Whether to remove \code{NA} values.
#'
#' @return A numeric scalar representing the harmonic mean.
#'
#' @details
#' Harmonic mean is useful when averaging rates or reciprocal quantities. It is defined as \code{length(x) / sum(1/x)}.
#'
#' @export
har_mean <- function(x, na.rm = T) {
    # if (na.rm) x <- na.omit(x)
    # length(x) / sum(1/x, na.rm = F)
    x <- x[!is.na(x)]
    length(x) / sum(1 / x)
}

#' Geometric Mean Ignoring NA
#'
#' @description
#' Compute the geometric mean of a numeric vector, optionally removing \code{NA} values.
#'
#' @param x A numeric vector.
#' @param na.rm Logical (default \code{TRUE}). Whether to remove \code{NA} values.
#'
#' @return A numeric scalar representing the geometric mean.
#'
#' @details
#' Defined as \code{exp(mean(log(x)))}, assuming all \code{x > 0}.
#' Useful for combining multiplicative factors or log-normal distributions.
#'
#' @export
geo_mean <- function(x, na.rm = T) {
    # if (na.rm) x <- na.omit(x)
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    # prod(x, na.rm=F)^(1/length(x))
    exp(sum(log(x)) / length(x))
}

#' Generate a Tukey Window
#'
#' Computes a Tukey (tapered cosine) window of length \code{n} with taper
#' parameter \code{alpha}. The Tukey window is rectangular when
#' \code{alpha = 0} and becomes a Hann window when \code{alpha = 1}.
#'
#' @param n Integer. Length of the window (number of points).
#' @param alpha Numeric in [0, 1]. Shape parameter controlling the fraction
#'   of the window inside the cosine tapered regions.
#'   \itemize{
#'     \item \code{alpha = 0}: rectangular window.
#'     \item \code{0 < alpha < 1}: Tukey window with cosine tapers.
#'     \item \code{alpha = 1}: Hann window.
#'   }
#'
#' @return A numeric vector of length \code{n} containing the Tukey window values.
#'
#' @details
#' The Tukey window is defined piecewise:
#' \deqn{
#'   w[m] =
#'   \begin{cases}
#'   \tfrac{1}{2}\left[1+\cos\!\left(\pi\left(\tfrac{2m}{\alpha(N-1)}-1\right)\right)\right], & 0 \leq m < \tfrac{\alpha(N-1)}{2}, \\
#'   1, & \tfrac{\alpha(N-1)}{2} \leq m \leq (N-1)(1-\tfrac{\alpha}{2}), \\
#'   \tfrac{1}{2}\left[1+\cos\!\left(\pi\left(\tfrac{2m}{\alpha(N-1)}-\tfrac{2}{\alpha}+1\right)\right)\right], & (N-1)(1-\tfrac{\alpha}{2}) < m \leq N-1.
#'   \end{cases}
#' }
#'
#' where \eqn{N} is the window length. For \code{alpha = 0} the definition
#' reduces to a rectangular window, and for \code{alpha = 1} to a Hann window.
#'
#' @examples
#' \dontrun{
#' # Rectangular window
#' tukey_window(8, alpha = 0)
#'
#' # Hann window
#' tukey_window(8, alpha = 1)
#'
#' # General Tukey window
#' tukey_window(16, alpha = 0.5)
#' }
#' @export
tukey_window <- function(n, alpha) {
    if (n <= 1L) {
        return(rep(1, n))
    }

    # Rectangular
    if (alpha <= 0) {
        return(rep(1, n))
    }
    # Hann
    if (alpha >= 1) {
        m <- seq_len(n) - 1L
        return(0.5 * (1 - cos(2 * pi * m / (n - 1L))))
    }

    r <- n - 1L
    m <- 0L:r
    edge <- floor(alpha * r / 2)

    w <- numeric(n)

    left <- (m <= edge)
    right <- (m >= (r - edge))
    middle <- !(left | right)

    w[left] <- 0.5 * (1 + cos(pi * (2 * m[left] / (alpha * r) - 1)))
    w[middle] <- 1
    w[right] <- 0.5 * (1 + cos(pi * (2 * m[right] / (alpha * r) - 2 / alpha + 1)))

    w
}

#' Floor with Decimal Digits
#'
#' Applies the \code{floor()} function to a number with precision controlled by number of digits.
#'
#' @param x A numeric value or vector.
#' @param digits Integer (default: 0). Number of decimal digits to retain.
#'
#' @return A numeric vector with values floored to specified digits.
#' @export
floor_digit <- function(x, digits = 0) {
    floor(x * 10^digits) / 10^digits
}

#' Ceiling with Decimal Digits
#'
#' Applies the \code{ceiling()} function to a number with precision controlled by number of digits.
#'
#' @param x A numeric value or vector.
#' @param digits Integer (default: 0). Number of decimal digits to retain.
#'
#' @return A numeric vector with values ceiled to specified digits.
#' @export
ceiling_digit <- function(x, digits = 0) {
    ceiling(x * 10^digits) / 10^digits
}

#' Order of Magnitude
#'
#' Computes the order of magnitude (base-10 logarithm floor) of a positive number.
#'
#' @param x A positive numeric value or vector.
#'
#' @return An integer or vector of integers representing \code{floor(log10(x))}.
#' @export
oom <- function(x) {
    floor(log10(abs(x)))
}

#' Rounded Range with Specified Bin Width
#'
#' Computes a range that aligns with a given bin width, rounding outwards to ensure coverage.
#'
#' @param x A numeric vector.
#' @param width Numeric. Desired bin width.
#'
#' @return A numeric vector of length 2 indicating start and end of adjusted range.
#' @export
range_width <- function(x, width) {
    rng <- range(x)
    rng[1] <- floor_digit(
        floor_digit(rng[1], digits = -oom(width)) / width,
        digits = 0
    ) *
        width
    rng[2] <- ceiling_digit(
        ceiling_digit(rng[2], digits = -oom(width)) / width,
        digits = 0
    ) *
        width
    rng
}

#' Sample from a fitted distribution based on a population
#'
#' Fits an appropriate distribution to the given population using \pkg{gamlss}
#' and draws samples from the fitted model.
#'
#' @param nsample An integer. Number of samples to generate.
#' @param pop A numeric vector. Population data to fit a distribution to.
#' @param return_fit Logical (default: TRUE). If \code{TRUE}, return the fitted distribution object.
#' @param seed An optional numeric. Random seed for reproducibility.
#'
#' @details
#' This function uses \pkg{gamlss} to automatically fit the best candidate distribution
#' (from the \code{"realAll"} family) to the non-missing values in \code{pop}.
#' It then draws \code{nsample} values from this distribution.
#' Values are filtered to ensure they are non-zero and below the maximum of \code{pop}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{\code{sample}}{A numeric vector of sampled values.}
#'   \item{\code{fit}}{(Optional) A fitted distribution object from \code{gamlss::fitDist}.}
#' }
#'
#' @examples
#' \dontrun{
#' pop_data <- rlnorm(1000, meanlog = 1, sdlog = 0.5)
#' result <- sample_dist(nsample = 100, pop = pop_data, seed = 42)
#' hist(result$sample)
#' }
#'
#' @note Requires the \pkg{gamlss} and \pkg{gamlss.dist} packages.
#' @export
sample_dist <- function(nsample, pop, return_fit = TRUE, seed = NULL) {
    # Upper limit by GWTC
    max.pop <- max(pop, na.rm = T)

    # Fit
    suppressALL({
        fit.dist <- gamlss::fitDist(
            na.omit(pop),
            pop = 2,
            type = "realAll",
            trace = FALSE,
            try.gamlss = TRUE
        )
    })

    # Extract fitted distribution function
    rfit <- eval(parse(text = paste0("gamlss.dist::r", fit.dist$family[1L])))

    # Apply the distribution function
    dist.tmp <- with(set.seed(seed), {
        do.call(
            "rfit",
            c(c(
                n = nsample * 10,
                sapply(
                    fit.dist$parameters,
                    function(par) {
                        fit.dist[[par]]
                    },
                    simplify = F
                )
            ))
        )
    })

    # Filter by non-zero and the upper limit
    # And select the number that is desired
    dist <- dist.tmp[dist.tmp != 0L & dist.tmp < max.pop][1L:nsample]

    if (return_fit) {
        fit.res <- fit.dist
    } else {
        fit.res <- NULL
    }

    return(list("sample" = dist, "fit" = fit.res))
}

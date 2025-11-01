#' Convert processed data frame to time series
#'
#' Constructs a \code{ts} object from a data frame containing time and value columns.
#'
#' @param proc.df A data frame containing processed time-series data.
#' @param val_col A character string. Name of the column to use as the signal values (default: \code{"observed"}).
#' @param time_col A character string. Name of the column to use as the start time in GPS seconds (default: \code{"GPS"}).
#' @param frequency A numeric value. Sampling frequency in Hz (default: 4096).
#'
#' @return A \code{ts} object constructed from the selected value column, starting at the time specified in \code{time_col}.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(GPS = 1000000000 + 0:(4095) / 4096, observed = rnorm(4096))
#' ts_obj <- proc2ts(df)
#' }
#'
#' @seealso \code{\link{ts}}
#' @export
proc2ts <- function(
    proc.df,
    val_col = "observed",
    time_col = "GPS",
    frequency = 4096) {
    ts(
        proc.df[, val_col, drop = T],
        start = proc.df[1, time_col],
        frequency = 4096
    )
}

#' Reshape a list-of-lists by transposing elements
#'
#' This function reshapes a list of named lists into a named list of lists,
#' effectively performing a transpose operation. It is equivalent to `purrr::list_transpose()`,
#' but implemented in base R with minimal code.
#'
#' @param list A list of named lists. Each sub-list must have the same names.
#'
#' @return A named list of lists, with outer and inner structure transposed.
#'
#' @examples
#' \dontrun{
#' x <- list(
#'     list(a = 1, b = 2),
#'     list(a = 3, b = 4)
#' )
#' transpose.List(x)
#' # Returns: list(a = list(1, 3), b = list(2, 4))
#' }
#' @export
transpose.List <- function(list) {
    # Same result as purrr::list_transpose()
    # But shorter codes
    nm <- methods::el(lapply(list, names))
    setNames(lapply(nm, \(i) lapply(list, "[[", i)), nm)
}

#' Compute temporal overlap for ARIMA-based filtering
#'
#' Calculates the number of samples to discard at the head and tail of a time series
#' when applying ARIMA(p, d, q) filtering, in order to avoid boundary artifacts.
#'
#' @param d An integer or vector. Differencing order(s). Typically 0, 1, or 2.
#' @param p An integer or vector. Autoregressive order(s).
#' @param q An integer or vector. Moving average order(s).
#' @param split Logical (default: \code{FALSE}). If \code{TRUE}, returns a named vector of head and tail overlap lengths.
#'
#' @return If \code{split = FALSE}, a single numeric value indicating the total overlap length (\code{Mh + Mt}).
#'         If \code{split = TRUE}, a named numeric vector with components:
#'         \code{Mh} (head overlap) and \code{Mt} (tail overlap).
#'
#' @details
#' The head overlap is given by \code{Mh = max(d) + max(p) + floor(q / 2)},
#' and the tail overlap is \code{Mt = floor(q / 2)}.
#'
#' @examples
#' \dontrun{
#' tr_overlap(d = 1, p = 500, q = 1000)
#' tr_overlap(d = 1, p = 500, q = 1000, split = TRUE)
#' }
#' @export
tr_overlap <- function(d, p, q, split = F) {
    max.d <- max(d)
    max.p <- max(p)
    max.q <- max(q)
    if ((max.q %% 2) == 0) {
        # Even case
        Mh <- max.d + max.p + max.q / 2
        Mt <- max.q / 2
    } else {
        # Odd case
        Mh <- max.d + max.p + (max.q - 1) / 2
        Mt <- (max.q - 1) / 2
    }
    if (split) {
        return(c("Mh" = Mh, "Mt" = Mt))
    } else {
        return(Mh + Mt)
    }
}

#' Split a time series into fixed-length batches
#'
#' Divides a \code{ts} object into equal-length segments (batches) of duration \code{t_bch} seconds.
#' If a \code{dqmask} attribute is present and \code{has.DQ = TRUE}, it is also split and attached to each batch.
#'
#' @param ts A \code{ts} object with optional \code{dqmask} attribute.
#' @param t_bch A numeric. Desired batch length in seconds (default: 1).
#' @param has.DQ Logical (default: \code{TRUE}). Whether to also batch the \code{dqmask} attribute if present.
#'
#' @return A named list of \code{ts} objects, each representing a batch. If \code{has.DQ = TRUE} and \code{dqmask} exists,
#'         each batch will have its own \code{dqmask} and \code{dq.lev} attributes.
#'
#' @details
#' The function computes the number of batches as \code{round(tl(ts) / t_bch)} where \code{tl(ts)} is the total duration.
#' The time series is evenly split based on index, assuming uniform sampling.
#'
#' If \code{dqmask} is present in the input \code{ts}, it is divided row-wise or element-wise depending on its structure,
#' and aligned with the corresponding time range of each batch.
#'
#' @examples
#' \dontrun{
#' batched <- batching(my_ts, t_bch = 2)
#' length(batched) # number of batches
#' attr(batched[[1]], "dqmask") # DQ mask for first batch
#' }
#'
#' @seealso \code{\link{window_to}}, \code{\link{tl}}
#' @export
batching <- function(ts, t_bch = 1, has.DQ = T) {
    batching.dq <- function(dqmask, n_bch, dq.level) {
        if (is.null(dim(dqmask))) {
            ind.split <- split(
                seq_along(dqmask),
                cut(seq_along(dqmask), breaks = n_bch, labels = F)
            )
        } else {
            ind.split <- split(
                seq(nrow(dqmask)),
                cut(seq(nrow(dqmask)), breaks = n_bch, labels = F)
            )
        }

        invisible(lapply(seq(n_bch), function(ind) {
            ind.dq <- ind.split[[ind]]
            if (length(ind.dq) == 1) {
                time_dqmask <- time(dqmask)[c(ind.dq, ind.dq)]
            } else {
                time_dqmask <- time(dqmask)[ind.dq]
            }
            window_to(dqmask, vr(time_dqmask))
        }))
    }
    n_bch <- round(tl(ts) / t_bch)
    ind.split <- split(
        seq_along(ts),
        cut(seq_along(ts), breaks = n_bch, labels = F)
    )
    time.stamp <- time(ts)
    batch <- lapply(ind.split, function(ind) {
        ts(ts[ind], start = time.stamp[ind[1]], frequency = frequency(ts))
    })
    names(batch) <- paste0("batch", sprintf("%04d", seq(n_bch)))

    if (has.DQ) {
        # Add dqmask attributes
        dqmask <- get_dqmask(ts)
        dq.level <- attr(dqmask, "level")
        batch.dq <- batching.dq(dqmask, n_bch, dq.level)

        invisible(lapply(seq(n_bch), function(ind) {
            attr(batch[[ind]], "dqmask") <<- batch.dq[[ind]]
            attr(batch[[ind]], "dq.lev") <<- attr(dqmask, "level")
        }))
    }
    batch
}

#' Perform batching and NA filtering for multiple ts objects
#'
#' @param ts.list A named list of `ts` objects, one per detector
#' @param t_bch Batch length in seconds (default: 1)
#' @param has.DQ Logical; whether to use DQ mask in batching (default: TRUE)
#'
#' @return A reshaped list of valid (non-NaN) batches from all detectors
#' @export
batching.network <- function(ts.list, t_bch = 1, has.DQ = TRUE) {
    # Step 1: batching per detector
    batch_net <- lapply(ts.list, function(tsobj) {
        batching(tsobj, t_bch = t_bch, has.DQ = has.DQ)
    })

    # Step 2: remove batches with any NaN values
    batch_net <- lapply(batch_net, function(det_batches) {
        lapply(det_batches, function(batch) {
            if (any(is.nan(batch))) NA else batch
        })
    })

    # Step 3: reshape list-of-lists into time-wise structure
    batch_set <- transpose.List(batch_net)

    return(batch_set)
}


#' Estimate frequency/trend windows from ACF with spectral fallback
#'
#' Computes data-driven window lengths (in **seconds**) for seasonal
#' (`frequency`) and trend (`trend`) components used by
#' \code{anomalize::time_decompose} (works for both \code{method = "stl"} and
#' \code{method = "twitter"}). The primary estimator selects the earliest
#' statistically significant **local maximum** in the autocorrelation function
#' (ACF). If no such peak exists, a fallback uses the dominant peak of the
#' periodogram (excluding near-DC). The resulting base period is then scaled and
#' clamped to avoid pathological window sizes.
#'
#' @details
#' - Assumes the input \code{ACF()} returns \code{lag} in **seconds** (as in
#'   your current implementation). The effective sampling interval \eqn{\Delta t}
#'   is inferred as \code{median(diff(lag))}.
#' - **Primary rule**: pick the first lag \eqn{\tau > 0} such that
#'   \eqn{\mathrm{ACF}(\tau)} is a local maximum and exceeds the white-noise
#'   95\% CI.
#' - **Fallback**: choose \eqn{\tau = (1/f_\mathrm{dom}) \Delta t} from the
#'   dominant periodogram frequency (excluding a small region near DC).
#' - Windows are constructed as
#'   \deqn{\mathrm{frequency} = \max(2\Delta t, \min(f_\mathrm{fac}\,\tau, \tau_\max))}
#'   \deqn{\mathrm{trend}     = \max(1.1\,\mathrm{frequency}, \min(t_\mathrm{fac}\,\tau, \tau_\max))}
#'   where \eqn{\tau_\max = \min(\max(\mathrm{lag}),\, \mathrm{max\_period\_frac}\cdot N\Delta t)}.
#'
#' @param ts A numeric or \code{ts} time series. If \code{ts}, its
#'   \code{frequency(ts)} is used upstream when computing the ACF; here we only
#'   consume the ACF output assuming lags are already in seconds.
#' @param fac.f Numeric scalar. Multiplier for the seasonal window
#'   (default \code{1.0}). For the Twitter method, \code{1.0} aligns with the
#'   base period; increase to widen the seasonal window.
#' @param fac.t Numeric scalar. Multiplier for the trend window
#'   (default \code{1.5}). Typical Twitter heuristic is \code{~1.5–2.0}.
#' @param max_period_frac Numeric in \code{(0,1)}. Upper bound on the candidate
#'   period as a fraction of the series duration (default \code{0.2}). Prevents
#'   overly long periods relative to batch length.
#' @param lag.max Integer. Maximum lag forwarded to \code{ACF()} (default:
#'   \code{min(4096, length(ts)-1)}).
#' @param use_fft_fallback Logical. If \code{TRUE} (default), use
#'   \code{stats::spec.pgram} fallback when no significant ACF peak is found.
#'
#' @return A list with two numeric scalars (in **seconds**):
#' \itemize{
#'   \item \code{freq}  — seasonal window length for \code{frequency=}
#'   \item \code{trend} — trend window length for \code{trend=}
#' }
#' These strings are typically passed as \code{paste(value, "seconds")} to
#' \code{anomalize::time_decompose()}.
#'
#' @examples
#' \dontrun{
#' ft <- decomp_freq_trend(ts, fac.f = 1.0, fac.t = 1.5)
#' anomalize::time_decompose(df, observed,
#'     method = "twitter",
#'     frequency = paste(ft$freq, "seconds"),
#'     trend = paste(ft$trend, "seconds")
#' )
#' }
#'
#' @seealso \code{\link[stats]{acf}}, \code{\link[stats]{spec.pgram}},
#'   \code{\link[anomalize]{time_decompose}}
#'
#' @export
decomp_freq_trend <- function(ts,
                              fac.f = 1.0, # seasonal window = 1.0 * period
                              fac.t = 1.5, # trend window    = 1.5 * period
                              max_period_frac = 0.2, # cap period <= 20% of series duration
                              lag.max = NULL,
                              use_fft_fallback = TRUE) {
    N <- length(ts)
    if (is.null(lag.max)) lag.max <- min(4096L, N - 1L)

    # ACF object from your existing ACF(); 'lag' is already in seconds
    acf.obj <- ACF(ts, lag.max = lag.max, plot = FALSE)
    lag_sec <- as.numeric(c(acf.obj$lag)) # seconds
    acv <- as.numeric(c(acf.obj$acf))
    ci <- acf.obj$white95ci

    # Effective sampling interval (sec) inferred from ACF lags
    dt <- suppressWarnings(median(diff(lag_sec), na.rm = TRUE))
    if (!is.finite(dt) || dt <= 0) dt <- 1

    # Search range: exclude lag=0, bound by max_period_frac of series duration
    total_duration_sec <- N * dt
    max_allow_sec <- min(max(lag_sec, na.rm = TRUE), total_duration_sec * max_period_frac)
    k <- which(lag_sec > 0 & lag_sec < max_allow_sec)

    # Find first significant local maximum: acv[i] > acv[i-1], >= acv[i+1], and > CI
    locs <- k[k > min(k) & k < max(k)]
    is_locmax <- acv[locs] > acv[locs - 1] & acv[locs] >= acv[locs + 1] & acv[locs] > ci
    cand <- locs[is_locmax]

    if (length(cand) > 0L) {
        period_sec <- lag_sec[cand[1L]]
    } else if (use_fft_fallback) {
        # Fallback via dominant spectral peak (exclude near-DC)
        sp <- stats::spec.pgram(ts, taper = 0.1, fast = TRUE, plot = FALSE)
        f <- sp$freq # cycles per sample
        S <- sp$spec
        exc <- max(1L, floor(length(f) * 0.01))
        if (length(S) > exc + 1L) {
            idx <- which.max(S[(exc + 1L):length(S)]) + exc
            f_dom <- f[idx]
            if (is.finite(f_dom) && f_dom > .Machine$double.eps) {
                period_sec <- (1 / f_dom) * dt # samples->seconds
            } else {
                period_sec <- 0.05 * total_duration_sec
            }
        } else {
            period_sec <- 0.05 * total_duration_sec
        }
    } else {
        period_sec <- 0.05 * total_duration_sec
    }

    # Build windows and clamp to sensible bounds
    min_window_sec <- 2 * dt
    max_window_sec <- max_allow_sec
    freq_sec <- max(min_window_sec, min(fac.f * period_sec, max_window_sec))
    trend_sec <- max(freq_sec * 1.1, min(fac.t * period_sec, max_window_sec))

    # Keep neat numeric formatting (seconds)
    freq_sec <- as.numeric(signif(freq_sec, 4))
    trend_sec <- as.numeric(signif(trend_sec, 4))

    list(freq = freq_sec, trend = trend_sec)
}

#' Identify outliers using robust IQR method
#'
#' Detects anomalous observations in a numeric vector using an IQR-based rule with enhanced robustness and
#' optional verbose reporting. The cutoff threshold is scaled by a tunable factor \eqn{(0.15/\alpha)}.
#'
#' @param x A numeric vector.
#' @param alpha A numeric (default: 0.05). Controls the sensitivity of outlier detection. Smaller values yield broader thresholds.
#' @param max_anoms A numeric between 0 and 1 (default: 0.2). Maximum fraction of data points to be flagged as outliers.
#' @param verbose Logical (default: \code{FALSE}). If \code{TRUE}, returns a detailed outlier report; otherwise returns a binary vector.
#'
#' @return
#' If \code{verbose = FALSE}, returns an integer vector of 0s and 1s (same length as \code{x}), where 1 indicates an outlier.
#'
#' If \code{verbose = TRUE}, returns a list with the following elements:
#' \describe{
#'   \item{\code{outlier}}{Binary vector of outlier flags (1 = outlier).}
#'   \item{\code{outlier_idx}}{Indices of detected outliers.}
#'   \item{\code{outlier_vals}}{Values of detected outliers.}
#'   \item{\code{outlier_direction}}{Direction of anomaly ("Up" or "Down").}
#'   \item{\code{critical_limits}}{Named vector with lower and upper bounds used for outlier detection.}
#'   \item{\code{outlier_report}}{Tibble containing values, limits, and direction annotations.}
#' }
#'
#' @details
#' This function is based on the IQR-based approach used in \pkg{anomalize}, but modifies the output to return binary flags
#' (1 = outlier, 0 = normal) instead of string labels ("Yes", "No"). It also removes the dependency on pipe operators and
#' expresses the logic using explicit data manipulation functions for clarity and standalone usage.
#'
#' The detection threshold is defined as:
#' \deqn{[Q1 - (0.15 / \alpha) \cdot IQR, \; Q3 + (0.15 / \alpha) \cdot IQR]}
#' where \code{IQR = Q3 - Q1}. Among points beyond this range, only the top \code{max_anoms × length(x)} are retained
#' based on their magnitude of deviation.
#'
#' @references
#' This implementation is adapted from the \code{anomalize::iqr()} function:
#' \url{https://business-science.github.io/anomalize/index.html}
#'
iqr2 <- function(x, alpha = 0.05, max_anoms = 0.2, verbose = FALSE) {
    quantile_x <- stats::quantile(x, prob = c(0.25, 0.75), na.rm = TRUE)
    iq_range <- quantile_x[[2]] - quantile_x[[1]]
    limits <- quantile_x + (0.15 / alpha) * iq_range * c(-1, 1)

    vals_tbl <- tibble::tibble(value = x)
    vals_tbl <- tibble::rownames_to_column(vals_tbl, var = "index")
    vals_tbl <- dplyr::mutate(
        vals_tbl,
        limit_lower = limits[1],
        limit_upper = limits[2],
        abs_diff_lower = ifelse(value <= limit_lower, abs(value - limit_lower), 0),
        abs_diff_upper = ifelse(value >= limit_upper, abs(value - limit_upper), 0),
        max_abs_diff = ifelse(
            abs_diff_lower > abs_diff_upper,
            abs_diff_lower,
            abs_diff_upper
        )
    )
    vals_tbl <- dplyr::select(vals_tbl, index, dplyr::everything())
    vals_tbl <- dplyr::select(vals_tbl, -c(abs_diff_lower, abs_diff_upper))
    vals_tbl <- dplyr::mutate(
        vals_tbl,
        centerline = (limit_upper + limit_lower) / 2,
        sorting = abs(value - centerline)
    )
    vals_tbl <- dplyr::arrange(vals_tbl, dplyr::desc(sorting))
    vals_tbl <- dplyr::select(vals_tbl, -c(centerline, sorting))
    vals_tbl <- tibble::rownames_to_column(vals_tbl, var = "rank")
    vals_tbl <- dplyr::mutate(
        vals_tbl,
        rank = as.numeric(rank),
        index = as.numeric(index)
    )
    vals_tbl <- dplyr::arrange(vals_tbl, dplyr::desc(max_abs_diff))
    vals_tbl <- dplyr::mutate(
        vals_tbl,
        outlier = ifelse(max_abs_diff > 0L, 1L, 0L),
        below_max_anoms = ifelse(
            dplyr::row_number() / dplyr::n() > max_anoms,
            0L,
            1L
        ),
        outlier_reported = ifelse(outlier == 1L & below_max_anoms == 1L, 1L, 0L),
        direction = dplyr::case_when(
            (outlier_reported == 1L) & (value > limit_upper) ~ "Up",
            (outlier_reported == 1L) & (value < limit_lower) ~ "Down",
            TRUE ~ "NA"
        ),
        direction = ifelse(direction == "NA", NA, direction)
    )

    vals_tbl_filtered <- dplyr::filter(vals_tbl, below_max_anoms == 1L)
    vals_tbl_filtered <- dplyr::select(
        vals_tbl_filtered,
        -c(max_abs_diff:below_max_anoms)
    )
    vals_tbl_filtered <- dplyr::rename(
        vals_tbl_filtered,
        outlier = outlier_reported
    )

    if (any(vals_tbl$outlier == 0L)) {
        limit_tbl <- dplyr::filter(vals_tbl, outlier == 0L)
        limit_tbl <- dplyr::slice(limit_tbl, 1)
        limits_vec <- c(
            limit_lower = limit_tbl$limit_lower,
            limit_upper = limit_tbl$limit_upper
        )
    } else {
        limit_tbl <- dplyr::slice(vals_tbl, n())
        limits_vec <- c(
            limit_lower = limit_tbl$limit_lower,
            limit_upper = limit_tbl$limit_upper
        )
    }

    if (verbose) {
        outlier_list <- list(
            outlier = dplyr::pull(dplyr::arrange(vals_tbl, index), outlier_reported),
            outlier_idx = dplyr::pull(
                dplyr::filter(vals_tbl, outlier_reported == 1L),
                index
            ),
            outlier_vals = dplyr::pull(
                dplyr::filter(vals_tbl, outlier_reported == 1L),
                value
            ),
            outlier_direction = dplyr::pull(
                dplyr::filter(vals_tbl, outlier_reported == 1L),
                direction
            ),
            critical_limits = limits_vec,
            outlier_report = vals_tbl_filtered
        )
        return(outlier_list)
    } else {
        return(dplyr::pull(dplyr::arrange(vals_tbl, index), outlier_reported))
    }
}

#' Detect outliers using robust GESD method
#'
#' Applies the Generalized Extreme Studentized Deviate (GESD) test for detecting one or more outliers
#' in a univariate numeric vector, using a robust formulation with median and MAD.
#'
#' @param x A numeric vector.
#' @param alpha A numeric (default: 0.05). Significance level for detecting outliers.
#' @param max_anoms A numeric between 0 and 1 (default: 0.2). Maximum fraction of values that may be flagged as outliers.
#' @param verbose Logical (default: \code{FALSE}). If \code{TRUE}, returns a detailed outlier report.
#'
#' @return
#' If \code{verbose = FALSE}, returns an integer vector (same length as \code{x}) with 0 for normal values and 1 for outliers.
#'
#' If \code{verbose = TRUE}, returns a list with:
#' \describe{
#'   \item{\code{outlier}}{Binary vector of 0/1 flags.}
#'   \item{\code{outlier_idx}}{Indices of detected outliers.}
#'   \item{\code{outlier_vals}}{Values of detected outliers.}
#'   \item{\code{outlier_direction}}{Direction of anomaly ("Up" or "Down").}
#'   \item{\code{critical_limits}}{Named vector with lower and upper bounds.}
#'   \item{\code{outlier_report}}{A tibble summarizing the detection statistics.}
#' }
#'
#' @details
#' This function implements a robust version of the GESD procedure, replacing mean and standard deviation
#' with median and MAD (median absolute deviation), as commonly used in anomaly detection for heavy-tailed or skewed data.
#'
#' At each iteration, it removes the most extreme observation (with highest robust z-score), recalculates the test statistic,
#' and compares it to a dynamically computed critical value. Observations are reported as outliers only if their z-statistics
#' exceed the threshold.
#'
#' This implementation is adapted from \pkg{anomalize}'s \code{gesd()} method. The return type has been simplified to use binary
#' flags (1 = outlier, 0 = normal), and the function is implemented without using pipe operators for clarity and compatibility.
#'
#' @references
#' Adapted from \code{anomalize::gesd()}:
#' \url{https://business-science.github.io/anomalize/reference/gesd.html}
#'
#' Original method: Rosner, B. (1983). “Percentage points for a generalized ESD many-outlier procedure.” Technometrics.
#'
gesd2 <- function(x, alpha = 0.05, max_anoms = 0.2, verbose = FALSE) {
    n <- length(x)
    r <- trunc(n * max_anoms)
    R <- numeric(length = r)
    lambda <- numeric(length = r)
    outlier_ind <- numeric(length = r)
    outlier_val <- numeric(length = r)
    m <- 0
    x_new <- x
    median_new <- numeric(length = r)
    mad_new <- numeric(length = r)
    for (i in seq_len(r)) {
        median_new[i] <- median(x_new)
        mad_new[i] <- mad(x_new)
        z <- abs(x_new - median(x_new)) / (mad(x_new) + .Machine$double.eps)
        max_ind <- which(z == max(z), arr.ind = T)[1]
        R[i] <- z[max_ind]
        outlier_val[i] <- x_new[max_ind]
        outlier_ind[i] <- which(x_new[max_ind] == x, arr.ind = T)[1]
        x_new <- x_new[-max_ind]
        p <- 1 - alpha / (2 * (n - i + 1))
        t_pv <- qt(p, df = (n - i - 1))
        lambda[i] <- ((n - i) * t_pv) / (sqrt((n - i - 1 + t_pv^2) * (n - i + 1)))
        if (!is.na(R[i]) & !is.na(lambda[i])) {
            if (R[i] > lambda[i]) {
                m <- i
            }
        }
    }
    vals_tbl <- tibble::tibble(
        rank = as.numeric(1:r),
        index = outlier_ind,
        value = outlier_val,
        test_statistic = R,
        critical_value = lambda,
        median = median_new,
        mad = mad_new,
        limit_lower = median_new - lambda * mad_new,
        limit_upper = lambda * mad_new + median_new
    )
    vals_tbl <- dplyr::mutate(
        vals_tbl,
        outlier = ifelse(test_statistic > critical_value, 1L, 0L),
        direction = dplyr::case_when(
            (outlier == 1L) & (value > limit_upper) ~ "Up",
            (outlier == 1L) & (value < limit_lower) ~ "Down",
            TRUE ~ "NA"
        ),
        direction = ifelse(direction == "NA", NA, direction)
    )
    vals_tbl <- dplyr::select(vals_tbl, -c(test_statistic:mad))

    outlier_index <- dplyr::filter(vals_tbl, outlier == 1L)
    outlier_index <- dplyr::pull(outlier_index, index)

    outlier_idx <- seq_along(x) %in% outlier_index
    outlier_response <- ifelse(outlier_idx == TRUE, 1L, 0L)
    if (any(vals_tbl$outlier == 0L)) {
        limit_tbl <- dplyr::filter(vals_tbl, outlier == 0L)
        limit_tbl <- dplyr::slice(limit_tbl, 1)
        limits_vec <- c(
            limit_lower = limit_tbl$limit_lower,
            limit_upper = limit_tbl$limit_upper
        )
    } else {
        limit_tbl <- dplyr::slice(vals_tbl, n())
        limits_vec <- c(
            limit_lower = limit_tbl$limit_lower,
            limit_upper = limit_tbl$limit_upper
        )
    }
    if (verbose) {
        outlier_vals <- dplyr::pull(dplyr::filter(vals_tbl, outlier == 1L), value)
        outlier_direction <- dplyr::pull(
            dplyr::filter(vals_tbl, outlier == 1L),
            direction
        )

        outlier_list <- list(
            outlier = outlier_response,
            outlier_idx = outlier_index,
            outlier_vals = outlier_vals,
            outlier_direction = outlier_direction,
            critical_limits = limits_vec,
            outlier_report = vals_tbl
        )
        return(outlier_list)
    } else {
        return(outlier_response)
    }
}

#' Detect anomalies in a data frame column using robust statistical methods
#'
#' Applies outlier detection to a specified column of a data frame using either
#' the IQR-based or robust GESD-based method. Returns the original data frame
#' augmented with anomaly indicators and detection thresholds.
#'
#' @param data A \code{data.frame} or \code{tibble}. Input dataset containing a numeric column to be analyzed.
#' @param target Unquoted column name. Target column to apply anomaly detection to.
#' @param method A character. Outlier detection method. One of \code{"iqr"} or \code{"gesd"}.
#' @param alpha A numeric (default: 0.05). Significance level used in the detection threshold.
#' @param max_anoms A numeric between 0 and 1 (default: 0.2). Maximum fraction of data points allowed to be anomalous.
#' @param verbose Logical (default: \code{FALSE}). If \code{TRUE}, returns a list including detailed results.
#'
#' @return
#' If \code{verbose = FALSE}, returns the input data frame with:
#' \describe{
#'   \item{\code{<target>_l1}}{Lower bound of anomaly detection range.}
#'   \item{\code{<target>_l2}}{Upper bound of anomaly detection range.}
#'   \item{\code{anomaly}}{Binary indicator: 1 for anomaly, 0 otherwise.}
#' }
#'
#' If \code{verbose = TRUE}, returns a list with:
#' \describe{
#'   \item{\code{anomalized_tbl}}{Augmented data frame as described above.}
#'   \item{\code{anomaly_details}}{Full output from \code{iqr2()} or \code{gesd2()}.}
#' }
#'
#' @details
#' This function is adapted from \pkg{anomalize}’s \code{anomalize()} and internally uses
#' custom implementations \code{iqr2()} and \code{gesd2()} for robust outlier detection.
#'
#' Unlike the original version, this implementation:
#' \itemize{
#'   \item Returns binary flags (\code{0} or \code{1}) for anomalies instead of \code{"Yes"/"No"} strings.
#'   \item Is implemented without pipe operators for simplicity and compatibility.
#' }
#'
#' The two available methods are:
#' \itemize{
#'   \item \strong{IQR}: Anomaly detection based on interquartile range thresholds.
#'   \item \strong{GESD}: Generalized Extreme Studentized Deviate test with robust statistics (median, MAD).
#' }
#'
#' @seealso \code{\link[anomalize]{anomalize}},
#'   \code{\link[anomalize]{iqr}},
#'   \code{\link[anomalize]{gesd}}
#'
#' @references
#' Adapted from \code{anomalize::anomalize()}:
#' \url{https://business-science.github.io/anomalize/reference/anomalize.html}
#'
anomalize2 <- function(
    data,
    target,
    method = c("iqr", "gesd"),
    alpha = 0.05,
    max_anoms = 0.2,
    verbose = FALSE) {
    if (missing(target)) {
        stop(
            "Error in anomalize(): argument \"target\" is missing, with no default",
            call. = FALSE
        )
    }
    target_expr <- rlang::enquo(target)
    method <- tolower(method[[1]])
    x <- dplyr::pull(data, !!target_expr)
    if (method == "iqr") {
        outlier_list <- iqr2(
            x = x,
            alpha = alpha,
            max_anoms = max_anoms,
            verbose = TRUE
        )
    } else if (method == "gesd") {
        outlier_list <- gesd2(
            x = x,
            alpha = alpha,
            max_anoms = max_anoms,
            verbose = TRUE
        )
    } else {
        stop("The `method` selected is invalid.", call. = FALSE)
    }
    outlier <- outlier_list$outlier
    limit_lower <- outlier_list$critical_limits[[1]]
    limit_upper <- outlier_list$critical_limits[[2]]
    ret <- dplyr::mutate(
        data,
        `:=`(
            !!paste0(dplyr::quo_name(target_expr), "_l1"),
            limit_lower
        ),
        `:=`(
            !!paste0(dplyr::quo_name(target_expr), "_l2"),
            limit_upper
        )
    )
    ret <- tibble::add_column(ret, anomaly = outlier)

    if (verbose) {
        ret <- list(anomalized_tbl = ret, anomaly_details = outlier_list)
        return(ret)
    } else {
        return(ret)
    }
}

# Within arch()

#' Detect anomalies in time series using robust methods
#'
#' Applies anomaly detection to a \code{ts} object using either direct or decomposition-based methods.
#' Internally uses a customized version of \pkg{anomalize} logic with robust thresholding.
#'
#' @param ts A \code{ts} object. Input time series data.
#' @param max.anom An integer (default: 100). Maximum number of anomalies allowed.
#' @param scale A numeric (default: 1.5). Controls the IQR scaling factor. Corresponds to standard 1.5×IQR rule.
#'              Internally converted to \code{alpha = 0.15 / scale} for compatibility with \code{anomalize2()}.
#' @param method A character. Outlier detection method passed to \code{\link{anomalize2}}.
#'               One of \code{"iqr"} or \code{"gesd"}.
#' @param decomp A character or \code{NULL}. Optional decomposition method for time series.
#'               One of \code{"stl"}, \code{"twitter"}, etc., or \code{NULL} for no decomposition.
#'               When specified, decomposes the time series using \pkg{anomalize}'s \code{time_decompose()}.
#' @param tzero A numeric (default: 0). Ignored. Included only for compatibility with other pipeline functions.
#'
#' @return
#' A \code{tibble} with the original time series and added columns:
#' \describe{
#'   \item{\code{observed}}{Original input values.}
#'   \item{\code{<observed>_l1}, \code{<observed>_l2}}{Lower and upper bounds for anomaly detection.}
#'   \item{\code{anomaly}}{Anomaly flag: \code{1} for anomaly, \code{0} otherwise.}
#'   \item{(Optional)}{Additional decomposition columns: \code{trend}, \code{season}, \code{remainder} if \code{decomp} is used.}
#' }
#'
#' @details
#' This function builds on \pkg{anomalize} by replacing the default anomaly detection with
#' customized functions \code{\link{iqr2}} and \code{\link{gesd2}}, which return binary anomaly indicators (0 or 1).
#' Pipe operators are not used. The \code{scale} parameter is internally translated into a significance level
#' using the formula: \code{alpha = 0.15 / scale}.
#'
#' If \code{decomp} is specified, the function decomposes the time series into trend, seasonal, and remainder components,
#' estimates the trend period automatically via ACF using \code{\link{decomp_freq_trend}}, and applies anomaly detection
#' to the remainder component only. The final result is then recomposed using \code{time_recompose()}.
#'
#' @seealso \code{\link{anomalize2}}, \code{\link{iqr2}}, \code{\link{gesd2}}, \code{\link{decomp_freq_trend}}
#'
#' @examples
#' \dontrun{
#' x <- ts(c(rnorm(100), 10, 15), frequency = 1)
#' anomaly(x, method = "iqr")
#' anomaly(x, method = "gesd", scale = 2)
#' }
#' @export
anomaly <- function(
    ts,
    max.anom = 100,
    scale = 1.5, # Q1-1.5*IQR / Q3+1.5*IQR
    method = "iqr",
    decomp = NULL,
    tzero = 0) {
    # Translate input arguments
    expr_alpha <- 0.15 / scale # Definition of alpha in anomalize() function
    expr_max_anoms <- signif(max.anom / length(ts), 2L)

    # Convert ts to tibbletime
    tbt <- dplyr::rename(as_tbt(ts), observed = x)

    # Anomaly detection (time decomposition is an option)
    if (!is.null(decomp)) {
        ft <- decomp_freq_trend(ts,
            fac.f = 2.0, fac.t = 10.0,
            # fac.f=1.0, fac.t=1.5 are the Twitter design philosophy
            max_period_frac = 1.0, lag.max = min(4096L, length(ts) - 1L)
        )
        expr_decomp_freq <- paste(ft$freq, "seconds")
        expr_decomp_trend <- paste(ft$trend, "seconds")

        # freq_trend <- decomp_freq_trend(ts)
        # decomp_freq <- freq_trend$freq * (1 / frequency(ts))
        # decomp_trend <- freq_trend$trend * (1 / frequency(ts))
        # expr_decomp_freq <- paste(decomp_freq, "seconds")
        # expr_decomp_trend <- paste(decomp_trend, "seconds")

        anomalized <- anomalize::time_decompose(
            tbt,
            observed,
            method = decomp,
            frequency = expr_decomp_freq,
            trend = expr_decomp_trend,
            message = FALSE
        )
        anomalized <- anomalize2(
            anomalized,
            remainder,
            method = method,
            alpha = expr_alpha,
            max_anoms = expr_max_anoms
        )
        anomalized <- anomalize::time_recompose(anomalized)
    } else {
        anomalized <- anomalize2(
            tbt,
            observed,
            method = method,
            alpha = expr_alpha,
            max_anoms = expr_max_anoms
        )
    }
    return(anomalized)
}

#' Add GPS time column to a data frame
#'
#' Adds a \code{GPS} column to a data frame by extracting timestamps from a reference \code{ts} object.
#'
#' @param df A \code{data.frame} or \code{tibble}. The data to which the GPS time will be added.
#' @param ref.ts A \code{ts} object. Reference time series from which GPS timestamps are extracted.
#'
#' @return A \code{tibble} identical to \code{df} but with an additional \code{GPS} column inserted after the \code{time} column.
#'
get_gps <- function(df, ref.ts) {
    dplyr::mutate(df, GPS = c(time(ref.ts)), .after = time)
}

#' Run DBSCAN clustering on detected anomalies
#'
#' Applies DBSCAN clustering to anomalous points based on time and observed value.
#'
#' @param anom.df A \code{data.frame} or \code{tibble} containing anomaly detection results,
#'   including at least columns for time, observed value, and a binary \code{anomaly} flag.
#' @param time_col A character string. Name of the time column (default: \code{"GPS"}).
#' @param val_col A character string. Name of the observed value column (default: \code{"observed"}).
#' @param eps A numeric. Maximum neighborhood radius for DBSCAN (default: \code{0.01}).
#' @param minPts An integer. Minimum number of points to form a cluster (default: \code{1}).
#' @param cluster.col A character string. Column name for cluster ID assignment (default: \code{"cluster"}).
#' @param ... Additional arguments passed to \code{dbscan::dbscan()}.
#'
#' @return A modified version of \code{anom.df} with a new \code{cluster.col} column indicating cluster membership.
#'   Anomalies not assigned to any cluster will receive a value of 0. Non-anomalous points will be assigned \code{NA}.
#'
#' @details
#' Only rows where \code{anomaly == 1L} are included in the clustering.
#' Clustering is performed on a 2D space defined by the specified time and value columns.
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(
#'     GPS = seq(0, 1, length.out = 100),
#'     observed = sin(2 * pi * GPS * 5),
#'     anomaly = sample(0:1, 100, replace = TRUE)
#' )
#' run_dbscan(df, eps = 0.02)
#' }
#' @export
run_dbscan <- function(
    anom.df,
    time_col = "GPS",
    val_col = "observed",
    eps = 0.01,
    minPts = 1,
    cluster.col = "cluster",
    ...) {
    dbs.input <- dplyr::select(
        dplyr::filter(anom.df, anomaly == 1L),
        dplyr::all_of(c(time_col, val_col))
    )
    if (nrow(dbs.input) == 0) {
        anom.df[, "cluster"] <- NA
    } else {
        dbs.res <- dbscan::dbscan(dbs.input, eps = eps, minPts = minPts, ...)
        anom.df[anom.df$anomaly == 1L, cluster.col] <- dbs.res$cluster
    }
    anom.df
}


#' Run full architecture: denoising, anomaly detection, and clustering
#'
#' Applies the full analysis pipeline to a time series: denoising via \code{seqarima()},
#' anomaly detection via \code{anomaly()}, and clustering of anomalies via \code{run_dbscan()}.
#'
#' @param ts A time series (`ts`) object.
#' @param params A list containing configuration parameters. The following fields are supported:
#'   \itemize{
#'     \item{\code{d_max}: Differencing order.}
#'     \item{\code{p_max}: AR order.}
#'     \item{\code{q_max}: MA order.}
#'     \item{\code{fl}, \code{fu}: Lower and upper cutoff frequencies for band-pass filter.}
#'     \item{\code{nmax}: Maximum number of anomalies to detect (default: 100).}
#'     \item{\code{scale}: IQR multiplier for outlier threshold (default: 1.5).}
#'     \item{\code{method}: Anomaly detection method; one of \code{"iqr"} or \code{"gesd"}.}
#'     \item{\code{decomp}: Optional time decomposition method; one of \code{"stl"}, \code{"twitter"}, or \code{NULL}.}
#'     \item{\code{eps}: Epsilon value for DBSCAN clustering.}
#'   }
#'
#' @return A \code{tibble} containing:
#'   \itemize{
#'     \item{time, observed: Processed time and signal values.}
#'     \item{raw: Original (pre-denoising) signal.}
#'     \item{anomaly: Anomaly indicator (1 = anomalous, 0 = normal).}
#'     \item{cluster: DBSCAN cluster label (NA for non-anomalous points).}
#'     \item{GPS: GPS time stamps.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Generate synthetic time series
#' fs <- 4096
#' t <- seq(0, 1, by = 1 / fs)
#' x <- sin(2 * pi * 60 * t) + rnorm(length(t), sd = 0.5)
#' ts_obj <- ts(x, start = t[1], frequency = fs)
#'
#' # Get default parameter set
#' params <- config_pipe()
#'
#' # Run full analysis architecture
#' result <- arch(ts_obj, params)
#' head(result)
#' }
#' @export
arch <- function(ts, params) {
    N_anom_max <- ifelse(is.null(params$nmax), 100, params$nmax)
    iqr.factor <- ifelse(is.null(params$scale), 1.5, params$scale)

    # Run seqARIMA
    deno <- seqarima(
        ts,
        d = params$d,
        p = params$p,
        q = params$q,
        fl = params$fl,
        fu = params$fu,
        verbose = F
    )

    # Run anomaly detection
    anom <- anomaly(
        deno,
        max.anom = N_anom_max,
        scale = iqr.factor,
        method = params$method, # "iqr" or "gesd"
        decomp = params$decomp # "stl" or "twitter" or NULL
    )

    # Add GPS time column
    anom <- get_gps(anom, deno)

    # Run DBSCAN
    anom <- run_dbscan(anom, eps = params$eps)
    anom <- dplyr::left_join(anom, as_tbt(deno, val_col = "raw"), by = "time")
    anom <- dplyr::relocate(anom, anomaly, .before = cluster)
    anom <- dplyr::relocate(anom, raw, .before = observed)
    return(anom)
}

#' Get default pipeline configuration
#'
#' Returns a list of default options for the full anomaly detection and coincidence pipeline.
#' Can be used to initialize parameter settings prior to calling \code{arch()} or \code{pipe()}.
#'
#' @param t_batch A time length of batch data (Default is 1 in second).
#' @param replace Optional named list to override default values.
#'
#' @return A named list containing the following fields:
#' \itemize{
#'   \item{\code{d}, \code{p}, \code{q}: Differencing and ARMA model orders.}
#'   \item{\code{fl}, \code{fu}: Frequency bounds for band-pass filtering.}
#'   \item{\code{arch}: Architecture function (default: \code{arch}).}
#'   \item{\code{nmax}, \code{scale}: Anomaly detection thresholds.}
#'   \item{\code{method}, \code{decomp}: Anomaly detection method and decomposition.}
#'   \item{\code{eps}: DBSCAN epsilon for clustering anomalies.}
#'   \item{\code{DQ}: Data quality flag (default: \code{"BURST_CAT2"}).}
#'   \item{\code{window_size}, \code{overlap}, \code{mean.func}: Coincidence analysis parameters.}
#'   \item{\code{P_update}: Update probability for coincidence test.}
#'   \item{\code{n_missed}: Number of samples missed due to differencing/ARMA lag.}
#' }
#'
#' @examples
#' \dontrun{
#' # Get default configuration
#' opt <- config_pipe()
#'
#' # Override specific values
#' opt2 <- config_pipe(list(p = 512, q = 10, fl = 16))
#' }
#' @export
config_pipe <- function(t_batch = 1L, replace = NULL) {
    def <- list(
        # seqARIMA parameters
        d = "auto",
        d_max = 2,
        p = 1024L,
        q = seq.int(20),
        fl = 32L,
        fu = 512L,

        # Anomaly Cluster parameters
        arch = arch,
        nmax = 100L * t_batch,
        scale = 1.5,
        method = "iqr",
        decomp = NULL,
        eps = 1 / 4096,
        DQ = "BURST_CAT2",

        # Coincidence parameters
        window_size = 128L,
        overlap = 0.0,
        mean.func = har_mean,

        # Pipeline option
        P_update = 0.05
    )

    if (!is.null(replace)) {
        stopifnot(is.list(replace))
        for (i in seq_along(replace)) {
            def[[names(replace)[i]]] <- replace[[i]]
        }
    }

    n_missed <- tr_overlap(def$d_max, def$p, def$q, split = T)
    def[["n_missed"]] <- n_missed

    def
}

#' Concatenate with previous tail to form new ts
#'
#' Concatenates the tail of the previous time series (`prev`) with the current one (`curr`),
#' keeping only the last `n_former` points from `prev`. If `prev` is `NA`, returns `curr` unchanged.
#'
#' @param prev A `ts` object or `NA` (default). Previous time series.
#' @param curr A `ts` object. Current time series.
#' @param n_former An integer. Number of tail points from `prev` to keep.
#'
#' @return A new concatenated `ts` object.
#'
#' @examples
#' \dontrun{
#' prev <- ts(1:100, start = 0, frequency = 1)
#' curr <- ts(101:105, start = 100, frequency = 1)
#' concat_ts(prev, curr, n_former = 5)
#' }
#' @export
concat_ts <- function(prev = NA, curr, n_former) {
    if (all(is.na(prev))) {
        curr
    } else {
        former_ts <- crop_to(prev, c(length(prev) - n_former, length(prev)))
        unique_vec <- apply(
            ts.union(former = former_ts, curr),
            MARGIN = 1,
            FUN = function(x) unique(x[!is.na(x)])
        )
        ts(unique_vec, start = ti(former_ts), frequency = frequency(curr))
    }
}

#' Check if anomalies were detected
#'
#' Returns `TRUE` if the given `proc` data frame contains any rows with `anomaly == 1`.
#'
#' @param proc A data frame that includes an `anomaly` column with binary flags (0 or 1).
#'
#' @return A logical value: `TRUE` if any anomalies are detected, `FALSE` otherwise.
is.anomdet <- function(proc) {
    n_anom <- nrow(dplyr::filter(proc, anomaly == 1))
    n_anom != 0
}

#' Adjust anomaly detection results for current batch
#'
#' Filters and shifts the anomaly detection results (`proc`) so that they align with the time window
#' of the current batch. Optionally includes some earlier values to compensate for tail-side loss
#' due to moving average smoothing.
#'
#' Specifically:
#' - Crops `proc` to include only time values within the current batch range
#'   (optionally extended backward by `n_missed[["Mt"]]` points).
#' - If anomalies exist, shifts cluster labels to ensure they start from 1.
#'
#' @param proc A data frame returned from anomaly detection, including `GPS` and `cluster` columns.
#' @param curr_batch A `ts` object for the current batch, used to define time bounds.
#' @param n_missed A list indicating how many time points to include before the current batch
#'        (e.g., `n_missed[["Mt"]]` due to moving average tail loss).
#'
#' @return A filtered and adjusted data frame with corrected cluster indices and time span.
#'
adjust_proc <- function(
    proc,
    curr_batch,
    n_missed) {
    # Crop within only current batch times
    #  + add `n_missed[["Mt"]]` amount of prev batch at the front
    # n_missed[["Mt"]]: lost values by MA at tail.
    proc <- dplyr::filter(
        proc,
        GPS >= ti(curr_batch) - (deltat(curr_batch) * n_missed[["Mt"]]) &
            GPS <= tf(curr_batch)
    )

    # Shift cluster number to be starting from 1
    if (is.anomdet(proc)) {
        cl.shift <- unique(na.omit(proc$cluster))[1] - 1
        proc$cluster <- proc$cluster - cl.shift
    }
    proc
}

#' Add DQ information to the processed data
#'
#' Adds data quality (DQ) mask information to a `proc` data frame, based on the
#' DQ information stored in the current batch (`curr_batch`).
#' The function aligns DQ values by flooring the GPS timestamp and performs sampling
#' rate adjustments if necessary.
#'
#' @param proc A data frame containing the processed anomaly or signal data, which must include a `GPS` column.
#' @param curr_batch A `ts` object with `dqmask` and `dq.lev` attributes, representing the DQ mask and level.
#'
#' @return A `data.frame` with additional columns corresponding to DQ flags.
#' If \code{dq.lev = "all"}, all available DQ channels are appended. Otherwise, only the specified level is added.
add_DQ <- function(proc, curr_batch) {
    dq.bat <- attr(curr_batch, "dqmask")
    dq.lev <- attr(curr_batch, "dq.lev")

    # Handling different sampling frequency between data and dq
    t_floor <- floor(proc$GPS)
    dq.df <- as.data.frame(dq.bat)
    rownames(dq.df) <- as.numeric(time(dq.bat))
    dq.col <- dq.df[as.character(t_floor), ]

    # Add DQ columns into data (proc)
    if (dq.lev == "all") {
        proc[, names(DQ_ShortNames())] <- dq.col
    } else {
        proc[, dq.lev] <- dq.col
    }
    proc
}

#' Add P0_DQ mask to processed data
#'
#' Adds a masked version of the \code{P0} column (e.g., false alarm probability)
#' based on the duty cycle specified by the \code{DQ} flags. If a DQ flag indicates
#' downtime (\code{DQ == 0}), the corresponding \code{P0} value is replaced with \code{NA}.
#'
#' @param proc A data frame containing the processed anomaly data with a \code{P0} column and one or more DQ columns.
#' @param DQ A character vector of one or more DQ channel names. Defaults to \code{"BURST_CAT2"}.
#'
#' @return A `data.frame` with added column(s) \code{P0_<DQ>} that contain \code{P0} values masked by duty cycle flags.
add_P0_DQ <- function(proc, DQ = "BURST_CAT2") {
    if (!all(is.na(proc))) {
        # Check proc is NA due to the duty cycle
        if (length(DQ) > 1) {
            for (i in seq_along(DQ)) {
                proc <- mutate(
                    proc,
                    "P0_{DQ[i]}" := ifelse(!!sym(DQ[i]) == 0, NA, P0)
                )
            }
        } else {
            proc <- mutate(proc, "P0_{DQ}" := ifelse(!!sym(DQ) == 0, NA, P0))
        }
    }
    proc
}

#' Append NA placeholders to result list
#'
#' Appends placeholder entries with `NA` values to a result list for standard structure.
#' Typically used when anomaly detection results are absent.
#'
#' @param res.list A named list to be appended with NA values.
#'
#' @return The input list with additional elements: \code{stat}, \code{lamb}, \code{prob},
#' \code{proc}, \code{updated_stat}, and \code{current_stat}, all set to `NA` or `list(N = NA, c = NA)`.
append_NA <- function(res.list) {
    res.list <- list.append(res.list, "stat", NA)
    res.list <- list.append(res.list, "lamb", list(a = NA, c = NA))
    res.list <- list.append(res.list, "prob", NA)
    res.list <- list.append(res.list, "proc", NA)
    res.list <- list.append(res.list, "updated_stat", NA)
    res.list <- list.append(res.list, "current_stat", NA)
    res.list
}

#' Append or assign a value into a nested list
#'
#' Adds a new element to a nested list, either as an unnamed entry appended to the end
#' or as a named entry assigned under a specified key.
#'
#' @param list A list object with a sublist at \code{where}.
#' @param where A character string indicating the name of the sublist to modify.
#' @param value The value to be appended or assigned.
#' @param name Optional. If provided, the value is assigned under this name. Otherwise, it is appended as unnamed.
#'
#' @return The modified list object.
list.append <- function(list, where, value, name = NULL) {
    if (is.null(name)) {
        list[[where]][[length(list[[where]]) + 1L]] <- value
    } else {
        list[[where]][[name]] <- value
    }
    list
}

#' Initialize pipeline environment
#'
#' Initializes global pipeline variables required for the anomaly detection pipeline,
#' including previous batch placeholders, detector-wise result containers, and coincidence list.
#' This function is designed to be run at the beginning of a `pipe()` execution.
#'
#' @param dets A character vector of detector names. Defaults to \code{c("H1", "L1")}.
#'
#' @return None. This function assigns \code{prev_batch}, \code{res.net}, and \code{coinc.lis}
#' into the calling environment.
#' @export
init_pipe <- function(dets = c("H1", "L1")) {
    # assign("i", 1L, envir = parent.frame())
    prev_batch <- vector("list", length = length(dets))
    names(prev_batch) <- dets

    res.det <- list(
        "proc" = list(),
        "stat" = list(),
        "lamb" = list(),
        "ustat" = list(
            list(
                "last_tcen" = NA,
                "stats" = list(
                    "t_batch" = 0,
                    "N_cl" = 0,
                    "N_anom" = 0,
                    "lambda_a" = NA,
                    "lambda_c" = NA
                )
            )
        )
    )
    res.net <- vector("list", length = length(dets))
    names(res.net) <- dets
    for (det in dets) {
        res.net[[det]] <- res.det
    }

    # assign("prev_batch", prev_batch, envir = parent.frame())
    # assign("res.net", res.net, envir = parent.frame())
    # assign("coinc.lis", list(), envir = parent.frame())
    list(prev_batch, res.net, list())
}

#' Compute anomaly cluster statistics
#'
#' Given a `proc` data frame containing anomaly labels and cluster assignments, this function
#' calculates summary statistics for each cluster, including the cluster center time, the number of anomalies,
#' and the inter-cluster lag time. It also computes global statistics such as the estimated anomaly rate
#' per cluster (\eqn{\lambda_a}) and the cluster rate per second (\eqn{\lambda_c}).
#'
#' @param proc A data frame containing anomaly detection results with at least the columns `anomaly`, `cluster`, and `GPS`.
#' @param last_tcen Optional numeric. The center time of the last cluster from the previous batch (used to compute `t_lag`).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{table}{A data frame of per-cluster statistics: `cluster`, `t_cen` (cluster center time), `N_anom` (number of anomalies), and `t_lag` (inter-cluster lag).}
#'   \item{stats}{A list of global statistics: `t_batch` (batch duration in seconds), `N_cl` (number of clusters), `N_anom` (total anomalies), `lambda_a`, and `lambda_c`.}
#'   \item{last_tcen}{The last computed cluster center time (used for streaming).}
#' }
stat_anom <- function(proc, last_tcen = NULL) {
    # Compute statistic table
    tab <- dplyr::filter(proc, anomaly == 1L)
    tab <- dplyr::group_by(tab, cluster)
    tab <- dplyr::mutate(tab, t_cen = median(GPS), .after = cluster) # compute t_cen
    tab <- dplyr::select(tab, anomaly, cluster, t_cen)
    tab <- dplyr::bind_rows(tab, dplyr::tibble(cluster = 0L, t_cen = last_tcen)) # add last_tcen
    tab <- dplyr::arrange(tab, cluster)
    tab <- dplyr::mutate(tab, t_lag = t_cen - dplyr::lag(t_cen)) # compute t_lag
    tab <- dplyr::filter(tab, cluster != 0L)
    tab <- dplyr::group_by(tab, cluster)
    tab <- dplyr::reframe(tab, t_cen = t_cen, N_anom = dplyr::n(), t_lag = t_lag)
    tab <- dplyr::distinct(tab, cluster, .keep_all = TRUE)

    # Organize output
    if (nrow(tab) == 0L) {
        # The same as initialized object in `init_pipe()`
        list(
            "table" = data.frame(
                "cluster" = NA,
                "t_cen" = NA,
                "N_anom" = NA,
                "t_lag" = NA
            ),
            "stats" = list(
                "t_batch" = 0,
                "N_cl" = 0,
                "N_anom" = 0,
                "lambda_a" = NA,
                "lambda_c" = NA
            ),
            "last_tcen" = NA
        )
    } else {
        stats <- list(
            t_batch = nrow(proc) / 4096,
            N_cl = nrow(tab),
            N_anom = sum(tab$N_anom)
        )
        lambda_c <- nrow(tab) / stats$t_batch
        lambda_a <- mean(tab$N_anom)
        stats[["lambda_c"]] <- lambda_c
        stats[["lambda_a"]] <- lambda_a
        list("table" = tab, "stats" = stats)
    }
}

#' Update accumulated cluster-level statistics
#'
#' Updates the global cluster statistics by combining the previous accumulated statistics (`upd`)
#' and the current batch statistics (`cur`). The quantities updated include batch duration, number of clusters,
#' number of anomalies, and the derived rates \eqn{\lambda_c} (cluster rate) and \eqn{\lambda_a} (anomaly rate).
#'
#' @param upd A list containing previous cumulative statistics (from `stat_anom()`), must contain `stats`.
#' @param cur A list containing statistics of the current batch (from `stat_anom()`), must contain `stats`.
#'
#' @return A list with a single element `stats`, containing:
#' \describe{
#'   \item{t_batch}{Cumulative batch duration.}
#'   \item{N_cl}{Cumulative number of detected clusters.}
#'   \item{N_anom}{Cumulative number of detected anomalies.}
#'   \item{lambda_c}{Updated cluster rate per second.}
#'   \item{lambda_a}{Updated average anomaly count per cluster.}
#' }
update_stat <- function(upd, cur) {
    # Compute the MOST updated statistics with updated statistics (upd)
    #   and current statistics (cur)
    t_batch.upd <- upd$stats$t_batch + cur$stats$t_batch # total batch time
    N_cl.upd <- upd$stats$N_cl + cur$stats$N_cl # total cluster number
    N_anom.upd <- upd$stats$N_anom + cur$stats$N_anom # total anomaly number

    lambda_c.upd <- N_cl.upd / (t_batch.upd) # Update lambda_c
    lambda_a.upd <- N_anom.upd / N_cl.upd # Update lambda_a

    # Return
    #   `last_tcen` will be added later in pipe()
    list(
        "stats" = list(
            "t_batch" = t_batch.upd,
            "N_cl" = N_cl.upd,
            "N_anom" = N_anom.upd,
            "lambda_c" = lambda_c.upd,
            "lambda_a" = lambda_a.upd
        )
    )
}

#' Update logic for statistics with optional P0-based filtering
#'
#' Updates the pipeline statistics using both accumulated (`updated`) and current (`current`) values.
#' If `P_update` is specified, it filters the anomalies in `proc` based on whether their significance
#' (`P0`) is below the threshold, and recomputes `current` before the update.
#'
#' This function assumes that `proc` (the current batch output with `P0`) and `prev_tcen` are
#' available in the parent frame.
#'
#' @param updated A list of previously accumulated statistics (from `update_stat()`), or `NA` for first batch.
#' @param current A list of current statistics (from `stat_anom()`), or `NA`.
#' @param P_update A numeric threshold. If not `NULL`, anomalies with `P0 < P_update` are discarded before updating.
#'
#' @return A list of updated statistics after merging `updated` and `current`.
update_logic <- function(updated, current, P_update) {
    # updated_stat cannot be NA except for the first batch
    if (all(is.na(updated))) {
        # use current_stat
        return(current)
    } else {
        if (all(is.na(current))) {
            # use only updated_stat so far
            return(updated)
        } else {
            # Perform updating procedure
            if (!is.null(P_update)) {
                # Gathering info from outside
                proc <- get("proc", envir = parent.frame(1))
                prev_tcen <- get("prev_tcen", envir = parent.frame(1))

                # Filtering by P_update
                #   If computed P0 < P_update, it's less-likely from noise fluctuations
                #   Thus, exclude those P0 < P_update by changing
                #        anomaly==1 into anomaly==0
                #   Then following function `stat_anom` will filter out anomaly==0 in
                #     calculating statistics.
                current.filtered <- stat_anom(
                    dplyr::mutate(proc, anomaly = ifelse(P0 < P_update, 0, anomaly)),
                    last_tcen = prev_tcen
                )

                # Update statistics with filtered statistics
                updated_new <- update_stat(upd = updated, cur = current.filtered)
            } else {
                # Ordinary updating procedure w/o any filtering
                updated_new <- update_stat(upd = updated, cur = current)
            }
            return(updated_new)
        }
    }
}

#' Extract the most recent cluster center time
#'
#' Retrieves the central time (`t_cen`) of the last (i.e., most recent) cluster
#' from a processed anomaly dataframe. If no cluster exists, it attempts to retrieve
#' `prev_tcen` from the parent environment.
#'
#' @param proc A dataframe output from the anomaly detection pipeline, containing
#' at least the columns `cluster` and `t_cen`.
#'
#' @return A numeric value representing the last cluster's center time (`t_cen`).
#' @note Assumes that `prev_tcen` is defined in the parent frame if no clusters are found.
get_last_tcen <- function(proc) {
    last_tcen <- dplyr::pull(
        dplyr::filter(
            dplyr::distinct(proc, cluster, .keep_all = T),
            cluster == max(proc$cluster, na.rm = T)
        ),
        t_cen
    )
    if (length(last_tcen) == 0) {
        last_tcen <- get("prev_tcen", envir = parent.frame(1))
    }
    last_tcen
}


#' Join cluster-level statistics into proc dataframe
#'
#' Merges the output of `stat_anom()` into the anomaly dataframe based on `cluster` ID.
#' The added columns include `t_cen`, `N_anom`, and `t_lag`, which are relocated to appear
#' immediately after the `cluster` column.
#'
#' @param proc A dataframe of anomaly detection results, containing a `cluster` column.
#' @param stat_table A dataframe containing cluster-level statistics from `stat_anom()$table`.
#'
#' @return A dataframe with added cluster statistics aligned by cluster ID.
add_stat <- function(proc, stat_table) {
    dplyr::relocate(
        dplyr::left_join(proc, stat_table, by = "cluster"),
        t_cen,
        N_anom,
        t_lag,
        .after = cluster
    )
}

#' Compute anomaly-tail Poisson probability
#'
#' Calculates the complementary cumulative distribution function (CCDF) of a Poisson
#' distribution with rate parameter \eqn{\lambda}, starting from a threshold \eqn{q}.
#' This function is designed to evaluate the probability of observing \eqn{n \ge q}
#' anomalies in a cluster, normalized so that \eqn{P(n \ge 1) = 1} when \eqn{\lambda} is used.
#'
#' @param q An integer or vector of integers representing the number of observed anomalies.
#' @param lambda A positive numeric value representing the expected number of anomalies (\eqn{\lambda}).
#'
#' @return A numeric vector of normalized CCDF values \eqn{P(n \ge q \mid \lambda)}.
ppois.anom <- function(q, lambda) {
    # NOTES:
    # - `lower.tail=F` gives 'survival function' or 'complementary cumulative
    #     distribution function (CCDF)' (e.g. CCDF = 1-CDF).
    # - Since
    #     `lower.tail=T` : P(n <= N) (default) and
    #     `lower.tail=F` : P(n >  N) (we use this),
    #   N is not included. Which means:
    #     ppois(0, ..., lower.tail=F)
    #        >> implies : P(n >  0)
    #        >> means   : P(n >= 1) (which is what we want to compute),
    #   because n is discrete.
    # - Since we count from 1 for N_anom, we use
    #     "ppois(q-1, ..., lower.tail=F)"
    #            ^~~ q-1 will convert N_anom = {1,2,3,...} into {0,1,2,...}
    #                as an input of ppois(..., lower.tail=F)
    # - Normalize by ppois(0, lambda, lower.tail=F) since it does not give
    #     exact 1.
    ppois(q - 1, lambda, lower.tail = F) / ppois(0, lambda, lower.tail = F)
}

#' Add Poisson anomaly-tail probability to dataframe
#'
#' Computes and appends a new column `Ppois` to the processed anomaly dataframe,
#' representing the Poisson-tail probability \eqn{P(n \ge N_{\rm anom})}
#' using the current estimated rate \eqn{\lambda_a} from `updated_stat`.
#'
#' @param proc A dataframe from the pipeline output containing `N_anom` per cluster.
#' @param updated_stat A list containing `stats$lambda_a`, typically returned from `update_stat()` or `update_logic()`.
#'
#' @return The input dataframe `proc` with an added `Ppois` column.
add_Ppois <- function(proc, updated_stat) {
    dplyr::mutate(proc, Ppois = ppois.anom(N_anom, updated_stat$stats$lambda_a))
}

#' Add exponential inter-cluster probability
#'
#' Computes and adds an exponential CDF-based column `Pexp` to the anomaly dataframe,
#' representing the probability \eqn{P(t \le T)} for each cluster's inter-cluster gap \eqn{t_{\rm lag}}.
#'
#' This is calculated as \code{pexp(t_lag, lambda_c)} where \code{lambda_c} is the cluster rate
#' estimated from \code{updated_stat}.
#'
#' @param proc A dataframe from the anomaly pipeline containing `t_lag`.
#' @param updated_stat A list containing `stats$lambda_c`, typically returned from `update_stat()` or `update_logic()`.
#'
#' @return The input dataframe with an added `Pexp` column.
add_Pexp <- function(proc, updated_stat) {
    # Different from Ppois, Pexp doesn't need further treatment,
    #   since we consider P(t <= T) by pexp(..., lower.tail=T (default) ).
    dplyr::mutate(proc, Pexp = pexp(t_lag, updated_stat$stats$lambda_c))
}

#' Add combined significance probability P0
#'
#' Computes the joint significance probability \eqn{\mathbb{P}_0 = P_{\text{Pois}} \cdot P_{\text{Exp}}}
#' for each anomalous cluster. This is only computed for rows with `anomaly == 1`.
#'
#' After computation, the rowwise structure is removed to avoid unintended behavior in subsequent operations.
#'
#' @param proc A dataframe with `Ppois`, `Pexp`, and `anomaly` columns.
#'
#' @return The input dataframe with an added `P0` column.
add_P0 <- function(proc) {
    proc <- dplyr::mutate(
        dplyr::rowwise(proc),
        P0 = ifelse(anomaly == 0, NA, prod(Ppois, Pexp, na.rm = T))
    )

    # Un-rowwise
    # "rowwise df" can raise some incorrect computation in the future.
    class(proc) <- c("tbl_df", "tbl", "data.frame")
    proc
}

#' Add full statistical significance fields (Ppois, Pexp, P0)
#'
#' Adds all cluster-level significance probabilities—Poisson (`Ppois`), exponential (`Pexp`),
#' and their product (`P0`)—to the anomaly dataframe in one step.
#'
#' This function wraps \code{add_Ppois()}, \code{add_Pexp()}, and \code{add_P0()}.
#'
#' @param proc A dataframe with detected anomaly clusters.
#' @param stat A list containing lambda statistics, such as output from \code{update_stat()} or \code{update_logic()}.
#'
#' @return The input dataframe with added `Ppois`, `Pexp`, and `P0` columns.
add_Pstats <- function(proc, stat) {
    proc <- add_Ppois(proc, stat)
    proc <- add_Pexp(proc, stat)
    proc <- add_P0(proc)
    return(proc)
}

#' Main anomaly detection pipeline for a single detector
#'
#' This function processes one batch of time-series data for a single detector using a
#' specified architecture and updates accumulated statistics over batches.
#'
#' The pipeline includes:
#' \itemize{
#'   \item Concatenating previous and current batches
#'   \item Running the detection algorithm (`arch`)
#'   \item Filtering and aligning results with the current batch
#'   \item Adding DQ flags and computing anomaly statistics
#'   \item Computing significance probabilities (Poisson, Exponential, P0)
#'   \item Updating cumulative statistics across batches
#' }
#'
#' @param curr_batch A `ts` object representing the current batch of data.
#' @param prev_batch A `ts` object from the previous batch; used for boundary-aware processing.
#' @param res.list A named list containing results so far, typically from `init_pipe()`.
#' @param arch_params A named list or Rist containing architecture-related parameters:
#'   \describe{
#'     \item{arch}{Main detection function for the pipeline.}
#'     \item{n_missed}{List with `Mh` and `Mt`, specifying how many points to include before and after.}
#'     \item{DQ}{Optional. DQ flag to include, e.g., `"BURST_CAT2"`.}
#'     \item{P_update}{Optional. Threshold for P0 filtering in update logic.}
#'   }
#' @param verb Logical; if `TRUE`, prints progress messages.
#'
#' @return An updated `res.list` with fields:
#'   \item{proc}{Processed dataframe for the current batch.}
#'   \item{stat}{Current batch statistics.}
#'   \item{lamb}{Lambda estimates (a, c) after update.}
#'   \item{ustat}{Updated cumulative statistics including `last_tcen`.}
#'
#' @examples
#' \dontrun{
#' # Assume you have curr_batch and prev_batch as ts objects
#' dets <- c("H1", "L1")
#' arch_params <- config_pipe()
#' init <- init_pipe(dets = dets)
#' prev_batch <- init[[1]]
#' res.net <- init[[2]]
#'
#' result <- pipe(curr_batch, prev_batch[["H1"]], res.net[["H1"]], arch_params)
#' }
#' @export
pipe <- function(
    curr_batch,
    prev_batch,
    res.list,
    arch_params,
    verb = T) {
    if (all(is.na(curr_batch))) {
        res.list <- append_NA(res.list)
        message_verb(
            "WARNING: The current batch is NA might be due to the duty cycle",
            v = verb
        )
    } else {
        # Processing
        arch <- arch_params$arch
        n_missed <- arch_params$n_missed
        DQ <- arch_params$DQ
        P_update <- arch_params$P_update
        proc <- arch(
            concat_ts(
                prev = prev_batch,
                curr = curr_batch,
                n_former = n_missed[["Mh"]]
            ),
            params = arch_params
        )
        proc <- adjust_proc(proc, curr_batch = curr_batch, n_missed = n_missed)
        if (!is.null(DQ)) {
            proc <- add_DQ(proc, curr_batch = curr_batch)
        }

        # Compute statistics on current batch
        prev_updated_stat <- dplyr::last(res.list$ustat)
        prev_tcen <- prev_updated_stat$last_tcen
        current_stat <- stat_anom(proc, last_tcen = prev_tcen)
        proc <- add_stat(proc, stat_table = current_stat$table)

        # Compute probabilities based on prev_updated_stat
        proc <- add_Pstats(proc, prev_updated_stat)
        if (!is.null(DQ)) {
            proc <- add_P0_DQ(proc, DQ = DQ)
        }

        # Update statistics with previous updated & current one
        # if P_update != NULL, `update_logic()` will use prev_tcen and proc inside
        updated_stat <- update_logic(
            updated = prev_updated_stat,
            current = current_stat,
            P_update = P_update
        )

        # Extract the last cluster's t_cen for the next batch
        updated_stat[["last_tcen"]] <- get_last_tcen(proc)

        # Store results
        res.list <- list.append(res.list, "stat", current_stat)
        res.list <- list.append(
            res.list,
            "lamb",
            list(a = updated_stat$stats$lambda_a, c = updated_stat$stats$lambda_c)
        )
        res.list <- list.append(res.list, "ustat", updated_stat)
        # res.list <- list.append(res.list, "proc", proc)
        res.list[["proc"]] <- proc
    }
    return(res.list)
}

#' Coincidence Analysis Using P0 Statistics
#'
#' @description
#' Perform coincidence analysis between two detectors (e.g., H1 and L1) by computing bin-wise joint probability statistics based on false alarm probabilities (P0). This is useful for detecting temporally aligned anomalies across detectors.
#'
#' @param shift.proc A \code{data.table} of detection statistics from the detector to be time-shifted (e.g., H1). Must contain \code{time} and a P0 column.
#' @param ref.proc A \code{data.table} of detection statistics from the reference detector (e.g., L1). Must contain \code{time} and a P0 column.
#' @param n_shift Optional integer. Number of samples to cyclically shift \code{shift.proc} in time.
#' @param window_size Integer. The size of each coincidence window (number of samples).
#' @param overlap Numeric (default: 0.5). Overlap ratio between consecutive windows. Must be between 0 and 1.
#' @param step_size Integer. Step size between windows. Defaults to \code{(1 - overlap) * window_size}.
#' @param mean.func A function or a named list of functions to aggregate P0 values within a window (e.g., \code{har.mean}, \code{mean}).
#' @param p_col Character. Name of the P0 column in both \code{shift.proc} and \code{ref.proc}.
#' @param return Integer. Controls the output format:
#' \code{1}: return the aggregated coincidence result only.
#' \code{2}: return a list with both \code{joined} time-aligned samples and the final \code{result}.
#'
#' @return Depending on the value of \code{return}, either a data.table of coincidence results, or a list containing both \code{joined} and \code{result} tables.
#'
#' @details
#' The function first aligns and optionally shifts the time series from one detector, joins them on the time column, bins the data into overlapping windows, applies an aggregation function to the P0 values, and computes the joint P0 by multiplying the aggregated values.
#'
#' @export
coincide_P0 <- function(
    shift.proc,
    ref.proc,
    n_shift = NULL,
    window_size,
    overlap = 0.5,
    step_size = (1 - overlap) * window_size,
    mean.func = har.mean,
    p_col = "P0_BURST_CAT2",
    return = 1L) {
    if (length(mean.func) > 1L) {
        if (is.null(attr(mean.func, "names"))) {
            stop(
                "(InputError) If multiple `mean.func` is given, it must be named vector."
            )
        }
    }
    if (!return %in% c(1L, 2L)) {
        stop(
            "InputError: 'return' must be one of (1, 2, 3)\n  1: returns 'result' only  \n  2: returns list of 'joined' and 'result'"
        )
    }

    data.table::setDTthreads(1L) # Multi-threads in data.table (DEPRECATED IN THE MOMENT)

    # (H1 must be shifted)
    # Shifting
    if (!is.null(n_shift)) {
        shift.proc$time <- shift.proc$time[c(
            (n_shift + 1L):nrow(shift.proc),
            1L:n_shift
        )]
    }

    # Joining
    ## Select only required columns
    shift.proc <- shift.proc[, .SD, .SDcols = c("time", p_col)]
    ref.proc <- ref.proc[, .SD, .SDcols = c("time", p_col)]
    ## Rename
    data.table::setnames(shift.proc, c("time", p_col), c("time", "P0_H1"))
    data.table::setnames(ref.proc, c("time", p_col), c("time", "P0_L1"))

    ## Set time as key
    data.table::setkey(shift.proc, "time")
    data.table::setkey(ref.proc, "time")

    ## Join by time
    if (nrow(shift.proc) > nrow(ref.proc)) {
        joined_shift <- shift.proc[ref.proc, on = "time"]
    } else {
        joined_shift <- ref.proc[shift.proc, on = "time"]
    }

    # Aggregation + Joint
    ## Make starting indicies with 'window_size' and 'step_size' (or overlap)
    ind_starts <- seq(1, nrow(joined_shift) - window_size + 1, by = step_size)

    ## Divided joined_shift by bin_id in list
    bin_list <- lapply(seq_along(ind_starts), function(i) {
        idx <- ind_starts[i]:(ind_starts[i] + window_size - 1)
        data.table::data.table(bin_id = i, joined_shift[idx])
    })

    ## Rebind
    joined_overlap <- data.table::rbindlist(bin_list)

    ## Aggregate by bin_id
    result <- joined_overlap[,
        .(
            time_bin = median(time),
            P0_H1_bin = mean.func(P0_H1),
            P0_L1_bin = mean.func(P0_L1)
        ),
        by = bin_id
    ]

    ## Compute coincident P0 by product
    result[,
        P0_net := prod(.SD),
        .SDcols = !c("bin_id", "time_bin"),
        by = bin_id
    ]

    # setDTthreads(1L) (DEPRECATED)

    # Returning
    if (return == 1L) {
        result
    } else if (return == 2L) {
        list(
            "joined" = joined_overlap[result, on = "bin_id"][,
                .SD,
                .SDcols = c("time", "bin_id", "P0_H1", "P0_L1", "P0_net")
            ],
            "result" = result
        )
    }
}

#' Run anomaly detection pipeline over detector network
#'
#' This function applies the single-detector \code{pipe()} function across a network of detectors
#' (e.g., H1 and L1), managing parallel execution, results updating, and coincidence analysis.
#'
#' @param batch_net A named list of current batches per detector. Each element is a \code{ts} object.
#' @param prev_batch A named list of previous batches per detector (used for continuity in AR/MA).
#' @param res.net A named list of results (updated per detector).
#' @param coinc.lis A list of previous coincidence results (will be appended).
#' @param arch_params A list of pipeline configuration parameters; see \code{\link{config_pipe}}.
#' @param update_model Logical. Whether to update internal statistics across batches.
#' @param verb Logical. If \code{TRUE}, print lambda diagnostics.
#' @param debug Logical. If \code{TRUE}, runs sequentially for debugging (no parallelism).
#'
#' @return This function updates the following in the parent environment:
#' \itemize{
#'   \item \code{res.net}: Updated detection results per detector.
#'   \item \code{prev_batch}: Updated previous batch list for the next call.
#'   \item \code{coinc.lis}: Appended coincidence analysis result.
#' }
#' These objects are expected to be initialized beforehand (e.g., via \code{\link{init_pipe}}).
#'
#' @details
#' For each detector, the function calls the internal \code{\link{pipe}} function with the current batch,
#' previous batch, and accumulated results. Parallel execution is performed via \code{foreach + dopar}
#' unless \code{debug = TRUE}, in which case it runs sequentially.
#'
#' Coincidence analysis is conducted using \code{\link{coincide_P0}} when all detectors have valid results.
#'
#' @examples
#' # Run detection pipeline over current batch
#' \dontrun{
#' pipe_net(batch_net, prev_batch, res.net, coinc.lis, arch_params)
#' }
#' @importFrom foreach %dopar% foreach
#' @import data.table
#' @export
pipe_net <- function(
    batch_net,
    prev_batch,
    res.net,
    coinc.lis,
    arch_params,
    update_model = TRUE,
    verb = TRUE,
    debug = FALSE) {
    # Get detector names
    dets <- names(batch_net)

    # Run pipe in parallel over detectors
    if (debug) {
        # For debugging
        res.net <- lapply(dets, function(det) {
            pipe(
                curr_batch = batch_net[[det]],
                prev_batch = prev_batch[[det]],
                res.list = res.net[[det]],
                arch_params = arch_params,
                verb = verb
            )
        })
        names(res.net) <- dets
    } else {
        res.net <- foreach::foreach(det = dets, .combine = "list") %dopar% {
            pipe(
                curr_batch = batch_net[[det]],
                prev_batch = prev_batch[[det]],
                res.list = res.net[[det]],
                arch_params = arch_params,
                verb = verb
            )
        }
        names(res.net) <- dets
    }

    # Store current batch as prev_batch for the next iteration
    for (det in dets) {
        prev_batch[[det]] <- batch_net[[det]]
    }

    # Monitoring lambdas
    if (verb) {
        for (det in dets) {
            cat(paste0(
                "  ",
                det,
                ": \u03bb_c=",
                sprintf("%.03f", dplyr::last(res.net[[det]]$lamb)$c),
                ", \u03bb_N=",
                sprintf("%.03f", dplyr::last(res.net[[det]]$lamb)$a),
                "\n"
            ))
        }
    }

    # Coincidence analysis
    if (length(dets) > 1L) {
        if (any(sapply(res.net, function(det) all(is.na(det$proc))))) {
            coinc.res <- NA
        } else {
            coinc.res <- coincide_P0(
                shift.proc = data.table::data.table(res.net$H1$proc),
                ref.proc = data.table::data.table(res.net$L1$proc),
                n_shift = NULL,
                window_size = arch_params$window_size,
                overlap = arch_params$overlap,
                mean.func = arch_params$mean.func,
                p_col = ifelse(is.null(arch_params$DQ), "P0", paste0("P0_", arch_params$DQ)),
                return = 2L
            )
        }
        coinc.lis[[length(coinc.lis) + 1]] <- coinc.res
    }

    # Assign them into parent frame (outside of pipe.net)
    assign("res.net", res.net, envir = parent.frame())
    assign("prev_batch", prev_batch, envir = parent.frame())
    assign("coinc.lis", coinc.lis, envir = parent.frame())
}


#' Run full anomaly detection stream over multiple batches
#'
#' Executes the anomaly detection pipeline across a sequence of batches for multiple detectors.
#' For each batch, it applies the \code{\link{pipe_net}} function, accumulates detection results,
#' performs coincidence analysis, and optionally incorporates pre-trained models.
#'
#' @param batch_set A list of batches, each element being a named list of \code{ts} objects per detector.
#' @param arch_params A list of architecture and configuration parameters.
#'   Typically created by \code{\link{config_pipe}}.
#' @param use_model (Optional) A named list of pre-trained per-detector statistics
#'   to initialize \code{ustat}. Default is \code{NA}, meaning no pretrained model.
#'
#' @return A named list containing:
#' \describe{
#'   \item{\code{res.net}}{Final detector-wise detection results.}
#'   \item{\code{coinc.lis}}{List of coincidence analysis results per batch.}
#'   \item{\code{model}}{Final per-detector accumulated statistics (i.e., last \code{ustat}).}
#'   \item{\code{arch_params}}{The configuration object used in the run.}
#'   \item{\code{lambda_plot}}{List of plots for \eqn{\lambda_a} and \eqn{\lambda_c}.}
#'   \item{\code{summary}}{Final per-detector statistical summary as a data.frame.}
#'   \item{\code{eta}}{List of elapsed times (in seconds) for each batch.}
#' }
#'
#' @details
#' This function initializes the pipeline via \code{\link{init_pipe}}, registers
#' parallel execution with \code{foreach} using a SOCK cluster, and iteratively
#' calls \code{\link{pipe_net}} on each batch.
#'
#' After processing all batches, it aggregates per-detector statistics,
#' terminates parallel backend, and visualizes lambda trajectories via
#' \code{\link{plot_lambda}}.
#'
#' The output \code{model} can be reused as \code{use_model} for transfer learning.
#'
#' @examples
#' \dontrun{
#' # Assume batch_set and arch_params are prepared
#' result <- stream(batch_set, arch_params)
#' result$summary # Show summary statistics
#' result$model # Save model for reuse
#' }
#'
#' @export
stream <- function(
    batch_set,
    arch_params,
    use_model = NA) {
    # Initialize pipeline
    dets <- names(batch_set[[1]])
    init <- init_pipe(dets = dets)
    prev_batch <- init[[1]]
    res.net <- init[[2]]
    coinc.lis <- init[[3]]

    # Pre-trained model
    if (!all(is.na(use_model))) {
        for (det in dets) {
            res.net[[det]][["ustat"]] <- list(use_model[[det]])
        }
    }

    # foreach config
    cl <- snow::makeCluster(length(dets), type = "SOCK", outfile = "/dev/null")
    invisible({
        snow::clusterEvalQ(cl, expr = {
            suppressPackageStartupMessages(library(beacon))
        })
    })
    doSNOW::registerDoSNOW(cl)

    # Run pipe_net()
    eta.lis <- vector(mode = "list", length = length(batch_set))
    for (ibch in seq_along(batch_set)) {
        cat(paste0(ibch, "-th batch:\n"))
        eta.lis[[ibch]] <- system.time({
            pipe_net(
                batch_net = batch_set[[ibch]],
                prev_batch = prev_batch,
                res.net = res.net,
                coinc.lis = coinc.lis,
                arch_params = arch_params,
                debug = T
            )
        })
    }

    # End of foreach
    foreach::registerDoSEQ()
    snow::stopCluster(cl)

    # Plot lambdas
    p.lamba <- plot_lambda(
        res.net,
        lambda = "a",
        t_from = ti(batch_set[[1]][[1]])
    )
    p.lambc <- plot_lambda(
        res.net,
        lambda = "c",
        t_from = ti(batch_set[[1]][[1]])
    )

    # Summary
    res.net_updated_stat_summary <- dplyr::bind_rows(
        as.data.frame(dplyr::last(res.net[[1]][["ustat"]])),
        as.data.frame(dplyr::last(res.net[[2]][["ustat"]]))
    )
    rownames(res.net_updated_stat_summary) <- dets

    # Model
    model <- lapply(res.net, function(x) dplyr::last(x$ustat))

    list(
        "res.net" = res.net,
        "coinc.lis" = coinc.lis,
        "model" = model,
        "arch_params" = arch_params,
        "lambda_plot" = list("a" = p.lamba, "c" = p.lambc),
        "summary" = res.net_updated_stat_summary,
        "eta" = eta.lis
    )
}

#' Reproduce the `proc` and coincidence result for a specific batch
#'
#' @description
#' Recompute the processing pipeline for a specific batch index or time, including statistics and coincidence analysis.
#' This is useful when the pipeline stores only the latest `proc` due to memory constraints.
#'
#' @param batch_set A list of batch Rist objects per detector (output of `batching_network()`).
#' @param batch_at Optional numeric. GPS time (in seconds) indicating the batch to be reproduced.
#' @param batch_num Optional integer. Index of the batch to be reproduced.
#' @param result A list returned by \code{stream()}, containing \code{res.net}, \code{arch_params}, and other pipeline results.
#' @param window_size Optional numeric. Coincidence window size (in seconds). If \code{NULL}, taken from \code{result$arch_params}.
#' @param overlap Optional numeric. Overlap size (in seconds) for coincidence binning. If \code{NULL}, taken from \code{result$arch_params}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{res.net}}{Updated pipeline result for the specified batch.}
#'   \item{\code{coinc.res}}{Coincidence analysis result for the specified batch.}
#'   \item{\code{batch_num}}{The index of the reproduced batch.}
#' }
#'
#' @details
#' Only one of \code{batch_at} or \code{batch_num} must be provided. The function reconstructs the previous state
#' of the pipeline up to the selected batch, and re-executes the pipeline and coincidence analysis for that batch.
#'
#' @examples
#' \dontrun{
#' # Reproduce the 3rd batch
#' out <- reproduce(batch_set = my_batches, batch_num = 3, result = result)
#'
#' # Or reproduce by time
#' out <- reproduce(batch_set = my_batches, batch_at = 1126259462.5, result = result)
#' }
#'
#' @export
reproduce <- function(
    batch_set,
    batch_at = NULL,
    batch_num = NULL,
    result,
    window_size = NULL,
    overlap = NULL) {
    # Find which batch is going to be reproduced
    if (methods::hasArg(batch_at) & is.null(batch_num)) {
        # Find which batch contains the time "batch_at"
        logical_bch <- sapply(batch_set, function(bnet_i) {
            trange <- tr(bnet_i[[1]])
            batch_at >= trange[1] & batch_at <= trange[2]
        })
        i_bch <- which(logical_bch)
    } else if (methods::hasArg(batch_num) & is.null(batch_at)) {
        i_bch <- batch_num
    } else {
        stop("InputError) Only one of batch_at and batch_num must be provided")
    }

    # Collect batches at (i_bch - 1) and (i_bch)
    if (i_bch == 1) {
        prev_batch <- `names<-`(
            as.vector(rep(NA, length(batch_set[[1]])), mode = "list"),
            names(batch_set[[1]])
        )
    } else {
        prev_batch <- batch_set[[i_bch - 1]]
    }
    curr_batch <- batch_set[[i_bch]]

    # Collect result list from 1 to i_bch - 1
    dets <- names(curr_batch)
    for (det in dets) {
        i_bch.prev <- ifelse(i_bch == 1, 1, i_bch - 1)
        result$res.net[[det]]$stat <- result$res.net[[det]]$stat[1:i_bch.prev]
        result$res.net[[det]]$lamb <- result$res.net[[det]]$lamb[1:i_bch.prev]
        result$res.net[[det]]$ustat <- result$res.net[[det]]$ustat[1:i_bch]
    }
    model <- lapply(result$res.net, function(x) dplyr::last(x$ustat))
    result$model <- model

    # Re do pipe
    arch_params <- result$arch_params
    res.net <- lapply(dets, function(det) {
        pipe(
            curr_batch = curr_batch[[det]],
            prev_batch = prev_batch[[det]],
            res.list = result$res.net[[det]],
            arch_params = arch_params,
            verb = F
        )
    })
    names(res.net) <- dets

    # Coincidence analysis
    if (is.null(window_size)) {
        window_size <- result$arch_params$window_size
    }
    if (is.null(overlap)) {
        overlap <- result$arch_params$overlap
    }
    coinc.res <- coincide_P0(
        shift.proc = data.table::data.table(res.net$H1$proc),
        ref.proc = data.table::data.table(res.net$L1$proc),
        n_shift = NULL,
        window_size = window_size,
        overlap = overlap,
        mean.func = result$arch_params$mean.func,
        p_col = ifelse(is.null(result$arch_params$DQ),
            "P0",
            paste0("P0_", result$arch_params$DQ)
        ),
        return = 2L
    )
    list("res.net" = res.net, "coinc.res" = coinc.res, "batch_num" = i_bch)
}

#' Detection Significance from Probability
#'
#' @description
#' Compute detection significance from probability using a logarithmic scale.
#'
#' @param P A numeric vector of probability values.
#' @param a A positive scalar (default: 2.3). Scaling factor for significance.
#'
#' @return A numeric vector representing detection statistic, \eqn{-a \cdot \log_{10}(P)}.
#'
#' @details
#' This function transforms small probabilities into large detection scores. Commonly used in significance evaluation.
#'
#' @export
Significance <- function(P, a = 2.3) {
    -a * log10(P)
}

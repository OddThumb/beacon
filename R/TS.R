#' Initial timestamp of a time series
#'
#' @param x A `ts` object.
#' @param n A numeric (default: 1). Number of initial timestamps to return.
#' @return A numeric vector of length `n`.
#' @export
ti <- function(x, n = 1) {
    utils::head(c(time(x)), n)
}

#' Final timestamp of a time series
#'
#' @param x A `ts` object.
#' @param n A numeric (default: 1). Number of final timestamps to return.
#' @return A numeric vector of length `n`.
#' @export
tf <- function(x, n = 1) {
    utils::tail(c(time(x)), n)
}

#' Time range of a time series
#'
#' @param x A `ts` object.
#' @return A numeric vector of length 2: \code{c(start, end)}.
#' @export
tr <- function(x) {
    c(ti(x), tf(x))
}

#' Duration of a time series
#'
#' @param x A `ts` object.
#' @return A numeric value indicating duration in seconds.
#' @export
tl <- function(x) {
    diff(tr(x)) + deltat(x)
}

#' Convert GPS time to UTC
#'
#' @param x A numeric. GPS time.
#' @param origin A POSIXct (default: as.POSIXct("1980-01-06 00:00:00", tz = "UTC"))
#' @return A POSIXct of UTC time.
#' @export
gps2utc <- function(x, origin = as.POSIXct("1980-01-06 00:00:00", tz = "UTC")) {
    origin + as.numeric(x)
}

#' Convert UTC to GPS time
#'
#' @param y A character. UTC time string.
#' @param origin A POSIXct (default: as.POSIXct("1980-01-06 00:00:00", tz = "UTC"))
#' @return A numeric. GPS time.
#' @export
utc2gps <- function(y, origin = as.POSIXct("1980-01-06 00:00:00", tz = "UTC")) {
    as.numeric(difftime(y, origin, units = "secs"))
}

#' Convert to `ts` object using a reference
#'
#' @param obj A vector, list, or data frame.
#' @param ref A `ts` object, time vector, or list specifying reference time.
#' @param sampling.freq Optional frequency if `ref` is numeric.
#' @param na.rm Logical. If \code{TRUE}, remove NA values (default: TRUE).
#' @return A `ts` object or list of `ts` objects.
#' @export
tsfy <- function(obj, ref, sampling.freq = NULL, na.rm = T) {
    # vector to ts
    tsref <- function(x, ref, na.rm = na.rm) {
        out <- ts(x, start = time(ref)[1], frequency = frequency(ref))
        if (na.rm) {
            out <- na.omit(out)
        }
    }

    # data.frame to ts
    df2ts <- function(df, data.col, time.col = "time") {
        ts(
            df[, data.col],
            start = df[1, time.col],
            deltat = unique(diff(df[, "time"]))
        )
    }

    # is.data.frame(obj) == TRUE
    if (is.data.frame(obj)) {
        # is.character(ref) == TRUE
        # (the column name of time axis)
        if (is.character(ref)) {
            data <- unname(unlist(dplyr::select(obj, -ref)))
            ti <- obj[1, ref]
            delta.t <- unique(diff(obj[, ref, drop = T]))
            out <- ts(c(data), start = ti, deltat = delta.t)
        } else if (is.list(ref)) {
            if (!all(c("data", "time") %in% names(ref))) {
                stop(
                    "If 'obj' is data.frame and 'ref' is a list,\n 'ref' should contain 'data' and 'time' which are the column names of data and time stamps in the 'obj'."
                )
            }
            out <- df2ts(
                obj,
                data.col = ref[["data"]],
                time.col = ref[["time"]]
            )
        }
    } else {
        # is.list(obj) == TRUE
        if (is.list(obj)) {
            tsfy.ready <- sapply(obj, length) == length(ref)
            tsfy.done <- lapply(
                obj[tsfy.ready],
                tsref,
                ref = ref,
                na.rm = na.rm
            )
            out <- c(tsfy.done, obj[!tsfy.ready])
        } else if (is.vector(obj)) {
            # is.vector(obj) == TRUE
            # is.ts(ref) == TRUE
            if (is.ts(ref)) {
                out <- ts(obj, start = time(ref)[1], frequency = frequency(ref))
            } else if (is.vector(ref)) {
                # is.vector(ref) == TRUE
                #
                if (length(ref) != 2 || is.null(names(ref))) {
                    stop(
                        "If obj is vector and ref is also vector, the length of ref should be 2 and it should be named with 'start' and 'freq'."
                    )
                }
                out <- ts(
                    obj,
                    start = ref[["start"]],
                    frequency = ref[["freq"]]
                )
            }
        } else {
            # is.data.frame(obj) == FALSE
            #   & is.list(obj) == FALSE
            #   & is.vector(obj) == FALSE
            stop("(Error) Check data type of 'obj'")
        }
    }

    if (na.rm) {
        out <- na.omit(out) # tseries::na.remove()
    }

    return(out)
}

#' Convert `ts` to data frame with time column
#'
#' @param x A `ts` object.
#' @param tzero A numeric (default: 0). Time shift.
#' @param val.name A character. Name for value column (default: "x").
#' @return A data frame with columns \code{time} and value.
#' @export
ts_df <- function(x, tzero = 0, val.name = "x") {
    if (!is.ts(x)) {
        stop("Input object class is not 'ts'")
    }

    if (!is.null(dim(x))) {
        tmp <- dplyr::relocate(
            dplyr::mutate(
                as.data.frame(x),
                time = c(time(x) - tzero)
            ),
            time,
            .before = 1
        )
    } else {
        tmp <- data.frame(time = c(time(x) - tzero), c(x))
        colnames(tmp) <- c("time", val.name)
    }
    return(tmp)
}

#' Convert `ts` to tibbletime object
#'
#' @param x A `ts` object.
#' @param time_col A character. Column name for time (default: "time").
#' @param val_col A character. Column name for value (default: "x").
#' @return A \code{tbl_time} object.
#' @export
as_tbt <- function(x, time_col = "time", val_col = "x") {
    timerange <- as.POSIXct(
        tr(x),
        origin = as.Date("1980-01-06"),
        tz = "UTC",
        format = "%Y-%m-%d %H:%M:%OS"
    )
    tbt <- dplyr::rename(
        dplyr::mutate(
            tibbletime::create_series(
                timerange[1] ~ timerange[2],
                period = paste(as.character(1 / frequency(x)), "second")
            ),
            "{val_col}" := c(x)
        ),
        "{time_col}" := date
    )
    attr(tbt, "index_quo") <- dplyr::quo(time)
    tbt
}

#' Inverse Fourier transform of `fs` object
#'
#' @param x A `fs` object.
#' @param start A numeric. Start time (default: 0 or `attr(x, "ti")`).
#' @param delta_t A numeric. Time step. If \code{NULL}, use natural resolution.
#'
#' @return A `ts` object in time domain.
#' @export
to_ts <- function(x, start = 0, delta_t = NULL) {
    nat_delta_t <- 1.0 / ((length(x) - 1) * 2) / deltaf(x)
    if (is.null(delta_t)) {
        delta_t <- nat_delta_t
    }

    if (!is.null(attr(x, "ti"))) {
        start <- attr(x, "ti")
    }

    # add 0.5 to round integer
    tlen <- as.integer(1.0 / deltaf(x) / delta_t + 0.5)
    flen <- as.integer(tlen / 2 + 1)

    if (flen < length(x)) {
        stop(
            " ValueError: The value of delta_t (",
            delta_t,
            ") would be undersampled. Maximum delta_t is ",
            nat_delta_t,
            "."
        )
    }

    tmp <- fs(
        double(tlen),
        df = deltaf(x),
        sampling.freq = attr(x, "sampling.freq")
    )
    tmp[1:length(x)] <- x
    ifft.res <- fftw::IFFT(tmp, plan = fftw::planFFT(length(tmp)))

    ts.out <- Re(ifft.res)
    ts.out <- ts(ts.out, start = start, deltat = delta_t)

    attr(ts.out, "assoc.fs") <- deparse(substitute(x))
    attr(ts.out, "delta_f") <- deltaf(x)
    attr(ts.out, "flen") <- length(x)
    return(ts.out)
}


#' Handy window function
#'
#' @param x A time series (`ts`) object.
#' @param ref A time series (`ts`) or a vector containing both an initial time and a final time.
#' @return A windowed time series (`ts`) object.
#' @export
window_to <- function(ts, ref) {
    if (is.ts(ref)) {
        window(x, start = time(ref)[1], end = utils::tail(time(ref), 1))
    } else if (length(ref) == 2) {
        window(x, start = ref[1], end = ref[2])
    } else {
        stop("Check input 'ref': ts object or time range")
    }
}

#' Handy window function with index
#'
#' @param x        A time series (`ts`) object.
#' @param ind.range A vector of length of 2. A first and second element refer to initial and final index, respectively.
#' @return A cropped time series (`ts`) object.
#' @export
crop_to <- function(x, ind.range) {
    if (!inherits(x, "ts")) {
        stop("(InputError) Given data is not a class of `ts`")
    }
    if (!all(ind.range %in% seq_along(x))) {
        stop("(InputError) A given `ind.range` includes an index out of `ts`")
    }
    window_to(x, time(x)[ind.range])
}


#' Shift time series by time offset
#'
#' @param x A `ts` object.
#' @param t_shift A numeric. Amount to shift (in seconds).
#' @return A time-shifted `ts`.
#' @export
shift <- function(x, t_shift) {
    stopifnot(inherits(x, "ts"))
    f <- frequency(x)
    k <- t_shift * f
    if (abs(k - round(k)) > .Machine$double.eps^0.5) {
        stop("t_shift must be an integer multiple of 1/frequency.")
    }
    tsp(x) <- tsp(x) + c(t_shift, t_shift, 0)
    x
}

#' Cyclically shift a vector
#'
#' @param x A numeric vector.
#' @param n An integer. Number of elements to shift.
#' @return A cyclic-shifted vector.
#' @export
cyclic <- function(x, n) {
    if (n == 0) {
        x
    } else {
        c(utils::tail(x, -n), utils::head(x, n))
    }
}

#' Cyclically shift a `ts`
#'
#' @param x A `ts` object.
#' @param t_cyclic A numeric. Time to shift cyclically.
#' @return A cyclic-shifted `ts` object.
#' @export
shift_cyclic <- function(x, t_cyclic) {
    stopifnot(inherits(x, "ts"))
    f <- frequency(x)
    N <- NROW(x)
    if (N == 0L) {
        return(x)
    }

    n <- as.integer(round(t_cyclic * f))
    n <- ((n %% N) + N) %% N
    if (n == 0L) {
        return(x)
    }

    if (is.matrix(x)) {
        x2 <- apply(x, 2, function(col) cyclic(col, n))
    } else {
        x2 <- cyclic(x, n)
    }
    tsp(x2) <- tsp(x)
    class(x2) <- class(x)
    x2
}


#' Align phase of a `ts` to another
#'
#' @param x A `ts` object to be shifted.
#' @param ref A reference `ts` object (optional).
#' @param phase A numeric phase shift (optional).
#' @return A phase-aligned `ts` object with attributes for applied shift.
#' @export
shift_phase <- function(x, ref = NULL, phase = NULL) {
    sampling.freq <- frequency(x)

    if (is.null(ref) & is.null(phase)) {
        stop("InputError: At least, one of `ref` or `phase` is required.")
    } else if (!is.null(phase)) {
        d_phi <- phase
    } else {
        # ref provided
        if (sampling.freq != frequency(ref)) {
            stop(
                "Error) Frequencies are different between given time-series and reference time-series"
            )
        }
        # NOTE: If you meant statistical mode, DO NOT use base::mode()
        # Replace with your own `Mode()` or another estimator.
        phi_ref <- mode(time(ref) %% (1 / sampling.freq))
        phi_ts <- mode(time(x) %% (1 / sampling.freq))
        d_phi <- phi_ref - phi_ts
    }

    # shift time axis by exactly d_phi (start & end both move by the same amount)
    tsp(x) <- tsp(x) + c(d_phi, d_phi, 0)

    attr(x, "d_phi") <- d_phi
    attr(x, "corrected_by") <- "time + d_phi; d_phi = phi_ref - phi_ts"
    x
}


#' Resize a `ts` to target length
#'
#' @param x A `ts` object.
#' @param nlen An integer. Desired length.
#' @param align Character. One of "left", "center", "right".
#'              Determines how the series is aligned when padding/truncating.
#'              Default: "left".
#' @return A resized `ts` object (padded with zeros or truncated).
#' @export
resize <- function(x, nlen, align = c("left", "center", "right")) {
    stopifnot(inherits(x, "ts"))
    align <- match.arg(align)
    f <- frequency(x)
    st <- start(x)
    is_mat <- is.matrix(x)
    n <- NROW(x)

    slice <- function(i) if (is_mat) x[i, , drop = FALSE] else x[i]
    pad0 <- function(k) {
        if (is_mat) matrix(0, nrow = k, ncol = NCOL(x)) else rep(0, k)
    }

    if (n >= nlen) {
        if (align == "left") {
            x_new <- slice(1:nlen)
        } else if (align == "right") {
            x_new <- slice((n - nlen + 1):n)
        } else {
            s <- floor((n - nlen) / 2) + 1
            e <- s + nlen - 1
            x_new <- slice(s:e)
        }
    } else {
        pad <- nlen - n
        if (align == "left") {
            x_new <- if (is_mat) rbind(x, pad0(pad)) else c(x, pad0(pad))
        } else if (align == "right") {
            x_new <- if (is_mat) rbind(pad0(pad), x) else c(pad0(pad), x)
        } else {
            lp <- floor(pad / 2)
            rp <- pad - lp
            x_new <- if (is_mat) {
                rbind(pad0(lp), x, pad0(rp))
            } else {
                c(pad0(lp), x, pad0(rp))
            }
        }
    }

    out <- ts(x_new, start = st, frequency = f)
    if (is_mat && !is.null(colnames(x))) {
        colnames(out) <- colnames(x)
    }
    out
}


#' Pad a `ts` with zeros
#'
#' @param x A `ts` object.
#' @param tstart A numeric. Start time of output.
#' @param tend A numeric. End time of output.
#' @param at A numeric. Starting time to inject original `x`, no matter `x` has its own time stamps.
#'
#' @return A padded `ts` object.
#' @examples
#' \dontrun{
#' # Original signal from t = 0 with 0.1 sec spacing
#' x <- ts(1:5, deltat = 0.1)
#'
#' # Pad x into a 1-second series starting at t = 1.3
#' padded <- pad(x, tstart = 1, tend = 2, at = 1.3)
#' # shows time indices from 1.0 to 2.0
#' time(padded)
#' # shows inserted values at 1.3, 1.4, ..., 1.7
#' padded
#'
#' # If x has time of -2, -1, 0, 1, 2,
#' x <- ts(1:5, start = -2)
#' # And pad with at = 0,
#' padded <- pad(x, tstart = -10, tend = 10, at = 0)
#' # x will start from 0 time not -2
#' }
#' @export
pad <- function(x, tstart, tend, at = 0) {
    stopifnot(inherits(x, "ts"))
    sampling_freq <- frequency(x)

    # Time grid for the padded series
    times <- seq(tstart, tend, by = 1 / sampling_freq)
    ngrid <- length(times)

    # Compute insertion range in samples
    shift_samples <- round((at - tstart) * sampling_freq)
    n_rows <- NROW(x)
    start_idx <- shift_samples + 1L
    end_idx <- start_idx + n_rows - 1L

    if (start_idx < 1L || end_idx > ngrid) {
        stop(
            "Padded series would go out of bounds. Check 'at', 'tstart', 'tend'."
        )
    }

    # Zero base with proper shape
    if (is.matrix(x)) {
        zerobase <- matrix(0, nrow = ngrid, ncol = NCOL(x))
        zerobase[start_idx:end_idx, ] <- x
        out <- ts(zerobase, start = tstart, frequency = sampling_freq)
        colnames(out) <- colnames(x)
    } else {
        zerobase <- numeric(ngrid)
        zerobase[start_idx:end_idx] <- x
        out <- ts(zerobase, start = tstart, frequency = sampling_freq)
    }
    out
}

#' Ensure even-length `ts`
#'
#' @param x A `ts` object.
#' @return A `ts` object with even length (truncates last sample if needed).
#' @export
evenify <- function(x) {
    if (length(x) %% 2 == 1) {
        window_to(x, tr(x) + c(0, -1 / frequency(x)))
    } else {
        message("Given ts has already even number of length: ", length(x))
        x
    }
}

#' Normalize a `ts` to unit scale
#'
#' @param x A `ts` object.
#' @return A normalized `ts` object. Scale factor is stored in \code{attr(..., "order")}.
#' @export
unit_normalize <- function(x) {
    order.val <- get_order(x)
    norm <- x / order.val
    attr(norm, "order") <- order.val
    return(norm)
}

#' Denormalize a unit-normalized `ts` object
#'
#' Restores the original scale of a normalized time series. The scale factor is retrieved
#' from the \code{"order"} attribute of the input \code{x}, or optionally provided directly.
#'
#' @param x A normalized `ts` object.
#' @param order Optional numeric. If provided, used as the scale factor.
#'
#' @return A `ts` object restored to its original scale.
#' @export
unit_denormalize <- function(x, order = NULL) {
    scale_factor <- if (!is.null(order)) {
        order
    } else {
        attr(x, "order", exact = TRUE)
    }

    if (is.null(scale_factor)) {
        stop("No 'order' attribute found and no scale factor provided.")
    }

    x * scale_factor
}

#' Apply median filter to a `ts`
#'
#' @param x A `ts` object.
#' @param order An odd integer. Window size of the median filter.
#' @return A smoothed `ts` object.
#' @export
mmed <- function(x, order) {
    meds <- runmed(x, order)
    ts(meds, start = ti(x), frequency = frequency(x))
}

#' Apply band-pass filter to `ts`
#'
#' @param x A `ts` object.
#' @param fl A numeric. Lower cutoff frequency.
#' @param fu A numeric. Upper cutoff frequency.
#' @param resp A character. Filter type ("FIR" or "IIR").
#' @param filt_order A numeric. Filter order (default: 512 for FIR, 8 for IIR).
#' @param verbose Logical. If \code{TRUE}, print filter info.
#'
#' @return A band-pass filtered `ts` object with attributes.
#' @export
BandPass <- function(
    x,
    fl = NULL,
    fu = NULL,
    resp = "FIR",
    filt_order = NULL,
    verbose = T
) {
    if (resp == "FIR") {
        filt.name <- "signal::fir1"
        n <- ifelse(is.null(filt_order), 512, filt_order)
    } else if (resp == "IIR") {
        filt.name <- "signal::butter"
        n <- ifelse(is.null(filt_order), 8, filt_order)
    }
    filt.func <- eval(parse(text = filt.name))
    window.func <- bspec::welchwindow

    sampling.freq <- frequency(x)
    nyq.freq <- sampling.freq / 2

    if (!is.null(fl) & !is.null(fu)) {
        filter.type <- "pass"
        FiltFun <- filt.func(
            n,
            c(fl / nyq.freq, fu / nyq.freq),
            window = window.func(n + 1),
            type = filter.type
        )
        message_verb(
            "|> Band-pass (",
            resp,
            ") of fl=",
            fl,
            " and fu=",
            fu,
            " with filter order of ",
            n,
            v = verbose
        )
    } else if (!is.null(fl) & is.null(fu)) {
        filter.type <- "high"
        FiltFun <- filt.func(
            n,
            fl / nyq.freq,
            window = window.func(n + 1),
            type = filter.type
        )
        message_verb(
            "|> High-pass (",
            resp,
            ") of fl=",
            fl,
            " with filter order of ",
            n,
            v = verbose
        )
    } else if (is.null(fl) & !is.null(fu)) {
        filter.type <- "low"
        FiltFun <- filt.func(
            n,
            fu / nyq.freq,
            window = window.func(n + 1),
            type = filter.type
        )

        message_verb(
            "|> Low-pass (",
            resp,
            ") of fu=",
            fu,
            " with filter order of ",
            n,
            v = verbose
        )
    }
    out <- tsfy(
        signal::filtfilt(filt = FiltFun, x = x),
        ref = x
    )

    attr(out, "type") <- filter.type
    attr(out, "order") <- n
    attr(out, "window.name") <- "bspec::welchwindow"
    attr(out, "filter.name") <- filt.name
    attr(out, "cutoff") <- c(fl, fu)
    return(out)
}

#' Spectral Whitening
#'
#' @param x A `ts` object.
#' @param sl A numeric. The segment length for estimating `psd`.
#' @param fl A numeric. The lower-frequency-cutoff for `bandpass`.
#' @param fu A numeric. The upper-frequency-cutoff for `bandpass`.
#' @param ... Additional arguments for `bandpass()`.
#' @return A `ts` object. Whitened time series.
#'
#' @export
whiten <- function(x, sl, fl, fu, ...) {
    PSD <- psd(x, sl, fl)
    BandPass(to_ts((to_fs(x) / (PSD^0.5))), fl, fu, verbose = F, ...)
}

#' Initial timestamp of a time series
#'
#' @param ts A `ts` object.
#' @param n A numeric (default: 1). Number of initial timestamps to return.
#' @return A numeric vector of length `n`.
#' @export
ti <- function(ts, n = 1) {
    head(c(time(ts)), n)
}

#' Final timestamp of a time series
#'
#' @param ts A `ts` object.
#' @param n A numeric (default: 1). Number of final timestamps to return.
#' @return A numeric vector of length `n`.
#' @export
tf <- function(ts, n = 1) {
    tail(c(time(ts)), n)
}

#' Time range of a time series
#'
#' @param ts A `ts` object.
#' @return A numeric vector of length 2: \code{c(start, end)}.
#' @export
tr <- function(ts) {
    c(ti(ts), tf(ts))
}

#' Duration of a time series
#'
#' @param ts A `ts` object.
#' @return A numeric value indicating duration in seconds.
#' @export
tl <- function(ts) {
    diff(tr(ts)) + deltat(ts)
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
#' @param ts A `ts` object.
#' @param tzero A numeric (default: 0). Time shift.
#' @param val.name A character. Name for value column (default: "x").
#' @return A data frame with columns \code{time} and value.
#' @export
ts_df <- function(ts, tzero = 0, val.name = "x") {
    if (!is.ts(ts)) {
        stop("Input object class is not 'ts'")
    }

    if (!is.null(dim(ts))) {
        tmp <- dplyr::relocate(
            dplyr::mutate(
                as.data.frame(ts),
                time = c(time(ts) - tzero)
            ),
            time,
            .before = 1
        )
    } else {
        tmp <- data.frame(time = c(time(ts) - tzero), c(ts))
        colnames(tmp) <- c("time", val.name)
    }
    return(tmp)
}

#' Convert `ts` to tibbletime object
#'
#' @param ts A `ts` object.
#' @param time_col A character. Column name for time (default: "time").
#' @param val_col A character. Column name for value (default: "x").
#' @return A \code{tbl_time} object.
#' @export
as_tbt <- function(ts, time_col = "time", val_col = "x") {
    timerange <- as.POSIXct(
        tr(ts),
        origin = as.Date("1980-01-06"),
        tz = "UTC",
        format = "%Y-%m-%d %H:%M:%OS"
    )
    tbt <- dplyr::rename(
        dplyr::mutate(
            tibbletime::create_series(
                timerange[1] ~ timerange[2],
                period = paste(as.character(1 / frequency(ts)), "second")
            ),
            "{val_col}" := c(ts)
        ),
        "{time_col}" := date
    )
    attr(tbt, "index_quo") <- dplyr::quo(time)
    tbt
}

#' Inverse Fourier transform of `fs` object
#'
#' @param fs A `fs` object.
#' @param start A numeric. Start time (default: 0 or `attr(fs, "ti")`).
#' @param delta_t A numeric. Time step. If \code{NULL}, use natural resolution.
#'
#' @return A `ts` object in time domain.
#' @export
to_ts <- function(fs, start = 0, delta_t = NULL) {
    nat_delta_t <- 1.0 / ((length(fs) - 1) * 2) / deltaf(fs)
    if (is.null(delta_t)) {
        delta_t <- nat_delta_t
    }

    if (!is.null(attr(fs, "ti"))) {
        start <- attr(fs, "ti")
    }

    # add 0.5 to round integer
    tlen <- as.integer(1.0 / deltaf(fs) / delta_t + 0.5)
    flen <- as.integer(tlen / 2 + 1)

    if (flen < length(fs)) {
        stop(
            " ValueError: The value of delta_t (",
            delta_t,
            ") would be undersampled. Maximum delta_t is ",
            nat_delta_t,
            "."
        )
    }

    tmp <- fs(double(tlen), df = deltaf(fs), sampling.freq = attr(fs, "sampling.freq"))
    tmp[1:length(fs)] <- fs
    ifft.res <- fftw::IFFT(tmp, plan = fftw::planFFT(length(tmp)))

    ts.out <- Re(ifft.res)
    ts.out <- ts(ts.out, start = start, deltat = delta_t)

    attr(ts.out, "assoc.fs") <- deparse(substitute(fs))
    attr(ts.out, "delta_f") <- deltaf(fs)
    attr(ts.out, "flen") <- length(fs)
    return(ts.out)
}


#' Handy window function
#'
#' @param ts A time series (`ts`) object.
#' @param ref A time series (`ts`) or a vector containing both an initial time and a final time.
#' @return A windowed time series (`ts`) object.
#' @export
window_to <- function(ts, ref) {
    if (is.ts(ref)) {
        window(ts, start = time(ref)[1], end = tail(time(ref), 1))
    } else if (length(ref) == 2) {
        window(ts, start = ref[1], end = ref[2])
    } else {
        stop("Check input 'ref': ts object or time range")
    }
}

#' Handy window function with index
#'
#' @param ts        A time series (`ts`) object.
#' @param ind.range A vector of length of 2. A first and second element refer to initial and final index, respectively.
#' @return A cropped time series (`ts`) object.
#' @export
crop_to <- function(ts, ind.range) {
    if (class(ts) != "ts") {
        stop("(InputError) Given data is not a class of `ts`")
    }
    if (!all(ind.range %in% seq_along(ts))) {
        stop("(InputError) A given `ind.range` includes an index out of `ts`")
    }
    window_to(ts, time(ts)[ind.range])
}


#' Shift time series by time offset
#'
#' @param ts A `ts` object.
#' @param t_shift A numeric. Amount to shift (in seconds).
#' @return A time-shifted `ts`.
#' @export
shift <- function(ts, t_shift) {
    ts(data = c(ts), start = c(ti(ts)) + t_shift, deltat = deltat(ts))
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
        c(tail(x, -n), head(x, n))
    }
}

#' Cyclically shift a `ts`
#'
#' @param ts A `ts` object.
#' @param t_cyclic A numeric. Time to shift cyclically.
#' @return A cyclic-shifted `ts` object.
#' @export
shift_cyclic <- function(ts, t_cyclic) {
    x <- c(ts)
    n <- trunc(t_cyclic * frequency(ts))
    ts(cyclic(x, n), start = ti(ts), frequency = frequency(ts))
}


#' Align phase of a `ts` to another
#'
#' @param ts A `ts` object to be shifted.
#' @param ref A reference `ts` object (optional).
#' @param phase A numeric phase shift (optional).
#' @return A phase-aligned `ts` object with attributes for applied shift.
#' @export
shift_phase <- function(ts, ref = NULL, phase = NULL) {
    sampling.freq <- frequency(ts)

    if (is.null(ref) & is.null(phase)) {
        stop("InputError: At least, one of `ref` or `phase` is required.")
    } else if (!is.null(phase)) {
        d_phi <- phase
    } else if (is.null(phase) & !is.null(ref)) {
        if (sampling.freq != frequency(ref)) {
            stop(
                "Error) Frequencies are different between given time-series and reference time-series"
            )
        }

        # Phases of time-series
        phi_ref <- mode(time(ref) %% (1 / sampling.freq))
        phi_ts <- mode(time(ts) %% (1 / sampling.freq))

        # Calculate difference of phase
        d_phi <- phi_ref - phi_ts
    }

    # Correct ts's phase as reference's phase by adding phase difference (ref - ts)
    ts.shift <- ts(ts, start = time(ts)[1] + d_phi, frequency = sampling.freq)

    # Assign attributes of chages
    attr(ts.shift, "d_phi") <- d_phi
    attr(
        ts.shift,
        "corrected_by"
    ) <- "time(ts) + d_phi; d_phi = phi_ref - phi_ts"

    return(ts.shift)
}


#' Resize a `ts` to target length
#'
#' @param ts A `ts` object.
#' @param nlen An integer. Desired length.
#' @return A resized `ts` object (padded with zeros or truncated).
#' @export
resize <- function(ts, nlen) {
    if (length(ts) >= nlen) {
        ts(c(ts)[1:nlen], start = ti(ts), frequency = frequency(ts))
    } else {
        ts(
            c(ts, rep(0, nlen - length(ts))),
            start = ti(ts),
            frequency = frequency(ts)
        )
    }
}


#' Pad a `ts` with zeros
#'
#' @param ts A `ts` object.
#' @param tstart A numeric. Start time of output.
#' @param tend A numeric. End time of output.
#' @param at A numeric. Time to inject original `ts`.
#'
#' @return A padded `ts` object.
#' @examples
#' # Original signal from t = 0 with 0.1 sec spacing
#' x <- ts(c(1, 2, 3, 4, 5), deltat = 0.1)
#'
#' # Pad x into a 1-second series starting at t = 1.3
#' padded <- pad(x, tstart = 1, tend = 2, at = 1.3)
#' # shows time indices from 1.0 to 2.0
#' time(padded)
#' # shows inserted values at 1.3, 1.4, ..., 1.7
#' padded
#'
#' @export
pad <- function(ts, tstart, tend, at = 0) {
    sampling_freq <- frequency(ts)

    # Time vector of full zero-base
    times <- seq(tstart, tend, by = 1 / sampling_freq)
    base.inds <- seq_along(times)

    # Shifted insertion index
    shift_samples <- round((at - tstart) * sampling_freq)
    ts_len <- length(ts)

    # Zero vector
    zerobase <- numeric(length(times))

    # Assign data (careful indexing)
    start_idx <- shift_samples + 1
    end_idx <- start_idx + ts_len - 1

    if (start_idx >= 1 && end_idx <= length(zerobase)) {
        zerobase[start_idx:end_idx] <- ts
    } else {
        stop("Padded series would go out of bounds. Check 'at', 'tstart', 'tend'.")
    }

    ts(zerobase, start = tstart, frequency = sampling_freq)
}

#' Ensure even-length `ts`
#'
#' @param ts A `ts` object.
#' @return A `ts` object with even length (truncates last sample if needed).
#' @export
evenify <- function(ts) {
    if (length(ts) %% 2 == 1) {
        window_to(ts, tr(ts) + c(0, -1 / frequency(ts)))
    } else {
        message("Given ts has already even number of length: ", length(ts))
        ts
    }
}

#' Normalize a `ts` to unit scale
#'
#' @param ts A `ts` object.
#' @return A normalized `ts` object. Scale factor is stored in \code{attr(..., "order")}.
#' @export
unit_normalize <- function(ts) {
    order.val <- get_order(ts)
    norm <- ts / order.val
    attr(norm, "order") <- order.val
    return(norm)
}

#' Denormalize a unit-normalized `ts` object
#'
#' Restores the original scale of a normalized time series. The scale factor is retrieved
#' from the \code{"order"} attribute of the input \code{ts}, or optionally provided directly.
#'
#' @param ts A normalized `ts` object.
#' @param order Optional numeric. If provided, used as the scale factor.
#'
#' @return A `ts` object restored to its original scale.
#' @export
unit_denormalize <- function(ts, order = NULL) {
    scale_factor <- if (!is.null(order)) {
        order
    } else {
        attr(ts, "order", exact = TRUE)
    }

    if (is.null(scale_factor)) {
        stop("No 'order' attribute found and no scale factor provided.")
    }

    ts * scale_factor
}

#' Apply median filter to a `ts`
#'
#' @param ts A `ts` object.
#' @param order An odd integer. Window size of the median filter.
#' @return A smoothed `ts` object.
#' @export
mmed <- function(ts, order) {
    meds <- runmed(ts, order)
    ts(meds, start = ti(ts), frequency = frequency(ts))
}

#' Apply band-pass filter to `ts`
#'
#' @param ts A `ts` object.
#' @param fl A numeric. Lower cutoff frequency.
#' @param fu A numeric. Upper cutoff frequency.
#' @param resp A character. Filter type ("FIR" or "IIR").
#' @param filt_order A numeric. Filter order (default: 512 for FIR, 8 for IIR).
#' @param verbose Logical. If \code{TRUE}, print filter info.
#'
#' @return A band-pass filtered `ts` object with attributes.
#' @export
BandPass <- function(
    ts,
    fl = NULL,
    fu = NULL,
    resp = "FIR",
    filt_order = NULL,
    verbose = T) {
    if (resp == "FIR") {
        filt.name <- "signal::fir1"
        n <- ifelse(is.null(filt_order), 512, filt_order)
    } else if (resp == "IIR") {
        filt.name <- "signal::butter"
        n <- ifelse(is.null(filt_order), 8, filt_order)
    }
    filt.func <- eval(parse(text = filt.name))
    window.func <- bspec::welchwindow

    sampling.freq <- frequency(ts)
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
        signal::filtfilt(filt = FiltFun, x = ts),
        ref = ts
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
#' @param ts A `ts` object.
#' @param sl A numeric. The segment length for estimating `psd`.
#' @param fl A numeric. The lower-frequency-cutoff for `bandpass`.
#' @param fu A numeric. The upper-frequency-cutoff for `bandpass`.
#' @param ... Additional arguments for `bandpass()`.
#' @return A `ts` object. Whitened time series.
#'
#' @export
whiten <- function(ts, sl, fl, fu, ...) {
    PSD <- psd(ts, sl, fl)
    BandPass(to_ts((to_fs(ts) / (PSD^0.5))), fl, fu, verbose = F, ...)
}

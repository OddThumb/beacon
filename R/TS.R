# Time Series Analysis ----

#' Simple initial time
#'
#' @param ts A time series (`ts`) object.
#' @param n  A numeric (default: 1). A number of head.
#' @return Numeric(s). n initial times.
#' @export
ti <- function(ts, n = 1) {
    head(c(time(ts)), n)
}

#' Simple final time
#'
#' @param ts A time series (`ts`) object.
#' @param n  A numeric (default: 1). A number of tail.
#' @return Numeric(s). n final times.
#' @export
tf <- function(ts, n = 1) {
    tail(c(time(ts)), n)
}

#' Simple time range
#'
#' @param ts A time series (`ts`) object.
#' @return A vector of length 2 which is equivalent to `c(ti(ts), tf(ts))`.
#' @export
tr <- function(ts) {
    c(ti(ts), tf(ts))
}

#' Simple time length
#'
#' @param ts A time series (`ts`) object.
#' @return A vector of length 2 which is equivalent to `c(ti(ts), tf(ts))`.
#' @export
tl <- function(ts) {
    diff(tr(ts)) + deltat(ts)
}

#' Convert GPS time to UTC time
#'
#' @param x          A numeric. A GPS time.
#' @param origin     A character (default: '1980-01-06'). A GPS time origin (Do not change for GW work).
#' @param formatting A logical (default: FALSE). Whether it formats with "%Y-%m-%d %H:%M:%OS", otherwise, "YYYYMMDD HH:MM:SS".
#' @return A character. An UTC time.
#' @export
gps2utc <- function(
    x,
    origin = as.Date('1980-01-06'),
    out_format = "%Y-%m-%d %H:%M:%OS"
) {
    y <- as.POSIXct(as.vector(x), origin = origin, tz = 'UTC')
    format.POSIXct(y, out_format)
}

#' Convert UTC time to GPS time
#'
#' @param y      A character. A UTC time.
#' @param origin A character (default: '1980-01-06'). A GPS time origin (Do not change for GW work).
#' @param format A character. A specific format of UTC time, e.g. `"%Y-%m-%d"`, `"%Y-%m-%d %H:%M:%OS"`.
#' @return A numeric. A GPS time.
#' @export
utc2gps <- function(
    y,
    origin = as.Date('1980-01-06'),
    in_format = "%Y-%m-%d %H:%M:%OS"
) {
    as.numeric(as.POSIXct(as.character(y), origin = origin, tz = "UTC")) -
        as.numeric(as.POSIXct(origin))
}


#' Print start time or update start time
#'
#' @param x     A time series (`ts`) object.
#' @param tstart  A numeric (default: NULL). A start time to be updated.
#' @return Nothing, but given `ts.`'s start time will be updated.
#' @export
start_time <- function(x, tstart = NULL) {
    if (is.null(tstart)) {
        ti(x)
    } else {
        assign(
            deparse(substitute(x)),
            ts(x, start = tstart, freq = frequency(x)),
            envir = .GlobalEnv
        )
    }
}

#' Transform to ts obj with reference ts
#'
#' @param obj A list or a vector.
#' @param ref A reference time series.
#' @param na.rm A logical (default: TRUE). Whether remove NA values or not.
#' @return A time series (`ts`) object with same time stamp of 'ref'.
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
        out <- na.omit(out) #tseries::na.remove()
    }

    return(out)
}

#' Transform to data frame with 'time' column
#'
#' @param ts A time series (`ts`) object.
#' @param tzero A numeric (default: 0). A zero time. Times will be shifted by '\code{tzero}'.
#' @return A data frame with 'time' column at first.
#' @export
ts_df <- function(ts, tzero = 0, val.name = "x") {
    if (!is.ts(ts)) {
        stop("Input object class is not 'ts'")
    }

    if (!is.null(dim(ts))) {
        ts |>
            as.data.frame() |>
            dplyr::mutate(time = c(time(ts) - tzero)) |>
            dplyr::relocate(time, .before = 1)
    } else {
        data.frame(time = c(time(ts) - tzero), c(ts)) |>
            `colnames<-`(c("time", val.name))
    }
}

#' Transform to 'tbl_time'
#'
#' @param ts A time series (`ts`) object.
#' @return A tbl_time.
#' @export
as_tbt <- function(ts, time_col = 'time', val_col = 'x') {
    timerange <- as.POSIXct(
        tr(ts),
        origin = as.Date("1980-01-06"),
        tz = "UTC",
        format = "%Y-%m-%d %H:%M:%OS"
    )
    tbt <- tibbletime::create_series(
        timerange[1] ~ timerange[2],
        period = paste(as.character(1 / frequency(ts)), "second")
    ) |>
        dplyr::mutate('{val_col}' := c(ts)) |>
        dplyr::rename('{time_col}' := date)
    attr(tbt, "index_quo") <- dplyr::quo(time)
    tbt
}

#' Transform `fs` class to `ts` class
#'
#' @param fs A `fs` object
#'
#' @return A `ts` object with respect to the given `fs` object.
#' @export
to_ts <- function(fs, start = 0, delta_t = NULL) {
    " Return the Fourier transform of this time series.

    Note that this assumes even length time series!

    Parameters
    ----------
    delta_t : {None, float}, optional
        The time resolution of the returned series. By default the
    resolution is determined by length and delta_f of this frequency
    series.

    Returns
    -------
    TimeSeries:
        The inverse fourier transform of this frequency series.
    "
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

    tmp <- fs(double(tlen), df = deltaf(fs))
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


#' Shift time series by time
#'
#' @param ts      A `ts` object.
#' @param t_shift A numeric. Time to be shifted.
#' @return A time-shifted `ts`.
#' @examples
#' # {not run}
#' # > xt <- ts(c(1,2,3), start=1, frequency=1)
#' # > ti(xt) # >>> 1
#' # > xt_shift <- shift(xt, 1)
#' # > ti(xt_shift) # >>> 2
#'
#' @export
shift <- function(ts, t_shift) {
    ts(data = c(ts), start = c(ti(ts)) + t_shift, deltat = deltat(ts))
}

#' Cyclic-shift a vector
#'
#' @param x A numeric vector.
#' @param n A numeric. length of shift.
#' @return A cyclic-shifted vector.
#' @examples
#' # {not run}
#' # > x <- c(1,2,3,4,5)
#' # > x_cyc <- cyclic(x, 2)
#' # > x_cyc # >>> 3 4 5 1 2
#'
#' @export
cyclic <- function(x, n) {
    if (n == 0) {
        x
    } else {
        c(tail(x, -n), head(x, n))
    }
}

#' Cyclic shift a `ts`
#'
#' @param ts       A `ts` object.
#' @param t_cyclic A numeric. Time to be cyclic-shifted
#' @return A cyclic-shifted `ts`.
#' @examples
#' # {not run}
#' # > xt <- ts(c(1,2,3,4,5), start=1, freqyency=1)
#' # > xt_cyc <- shift.cyclic(xt, t_cyclic=2)
#' # > xt_cyc
#' # Time Series:
#' # Start = 1
#' # End = 5
#' # Frequency = 1
#' # [1] 3 4 5 1 2
#'
#' @export
shift_cyclic <- function(ts, t_cyclic) {
    x <- c(ts)
    n <- trunc(t_cyclic * frequency(ts))
    ts(cyclic(x, n), start = ti(ts), frequency = frequency(ts))
}


#' Shift phase of ts
#'
#' @param ts    A ts object to be phase-shifted
#' @param ref   A ts object as a reference (default: NULL).
#' @param phase A numeric (default: NULL). A specific shifting phase.
#' @return A phase-shifted phase.
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
    attr(ts.shift, 'd_phi') <- d_phi
    attr(
        ts.shift,
        'corrected_by'
    ) <- "time(ts) + d_phi; d_phi = phi_ref - phi_ts"

    return(ts.shift)
}


#' Resize `ts` with given length
#'
#' @param ts   A `ts` object.
#' @param nlen A numeric. A length to be resized.
#' @return A resized `ts` with given `nlen`.
#' @examples
#' # {not run}
#' # > xt <- ts(c(1,2,3,4,5), start=1, freqyency=1)
#' # If nlen > length(ts)
#' # > xt_resize1 <- resize(xt, 10)
#' # > xt_resize1
#' # Time Series:
#' # Start = 1
#' # End = 10
#' # Frequency = 1
#' # [1] 1 2 3 4 5 0 0 0 0 0
#' # If nlen <= length(ts)
#' # > xt_resize2 <- resize(xt, 3)
#' # Time Series:
#' # Start = 1
#' # End = 3
#' # Frequency = 1
#' # [1] 1 2 3
#'
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


#' Padding short ts into longer zeros.
#'
#' @param ts     A time series (`ts`) object.
#' @param tstart A numeric. Start time of zeros.
#' @param tend   A numeric. End time of zeros.
#' @param at     A numeric (default: 0). Time that ts will be injected.
#' @return A ts object of given ts injected zeros.
#' @export
pad <- function(ts, tstart, tend, at = 0) {
    sampling_freq <- frequency(ts)

    # Span zerobase indicies
    base.inds <- trunc(
        seq(tstart, tend, by = 1 / sampling_freq) * sampling_freq
    )

    # Modify `tsp` by `at`
    tsp(ts) <- tsp(ts) + c(at, at, 0)

    # Get modified time range and its indicies
    # As `ts` has been modified by `at`, its index will be integer > 0
    dat.tr <- range(time(ts))
    dat.range <- trunc(dat.tr * sampling_freq)

    # Generate zero-base
    len <- length(base.inds)
    zerobase <- double(len)

    # Specify starting & ending padding index
    padstart <- if (vi(base.inds) < dat.range[1]) {
        which(base.inds == dat.range[1])
    } else {
        which(base.inds == vi(base.inds))
    }
    padend <- if (vf(base.inds) > dat.range[2]) {
        which(base.inds == dat.range[2])
    } else {
        which(base.inds == vf(base.inds))
    }

    # Inject into zero-base
    zerobase[padstart:padend] <- c(suppressWarnings(window_to(
        ts,
        c(tstart, tend)
    )))

    # Make it as `ts`
    ts(zerobase, start = tstart, frequency = sampling_freq)
}

#' If length(Data$data) is odd, cut the last element
#'
#' @param ts A time series (`ts`) object.
#' @return A time series (`ts`) object with even length.
#' @export
evenify <- function(ts) {
    if (length(ts) %% 2 == 1) {
        window_to(ts, tr(ts) + c(0, -1 / frequency(ts)))
    } else {
        message("Given ts has already even number of length: ", length(ts))
        ts
    }
}

#' Scale ts
#'
#' @param ts A time series (`ts`) object.
#' @return A scaled time series. A scale factor is calculated by '`get.order`'. The factor will be stored in '`$order`' attribute.
#' @export
one_ts <- function(ts) {
    order.val <- get_order(ts)
    norm <- ts / order.val
    attr(norm, 'order') <- order.val
    return(norm)
}

#' Simple Median filter wrapper
#'
#' @param ts A time series (`ts`) object.
#' @param order An integer. The order of median filter.
#' @return A median filtered time series (`ts`) object.
#' @export
mmed <- function(ts, order) {
    meds <- runmed(ts, order)
    tsfy(meds, ts)
}

#' Band-pass filter
#'
#' @param ts A time series (`ts`) object.
#' @param fl A numeric. The frequency lower bound for the band-pass filter.
#' @param fu A numeric. The frequency upper bound for the band-pass filter.
#' @param resp A character. "FIR (Finite Impulse Response)" or "IIR (Infinite Impulse Response)"
#' @param filt_order A numeric. The order of filter. Default value is 512 (FIR) or 8 (IIR).
#'
#' @return A band-pass filtered time series (`ts`) object.
#' @export
BandPass <- function(
    ts,
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
    out <- signal::filtfilt(filt = FiltFun, x = ts) |> tsfy(ref = ts)

    attr(out, "type") <- filter.type
    attr(out, "order") <- n
    attr(out, 'window.name') <- 'bspec::welchwindow'
    attr(out, 'filter.name') <- filt.name
    attr(out, 'cutoff') <- c(fl, fu)
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
    to_ts((to.fs(ts) / (PSD^0.5))) |>
        BandPass(fl, fu, verbose = F, ...)
}

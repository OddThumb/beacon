#' Correlate two complex vectors
#'
#' Computes the element-wise product of the conjugate of `x` and `y`, used in matched filtering.
#'
#' @references https://pycbc.org/pycbc/latest/html/_modules/pycbc/filter/matchedfilter_numpy.html#correlate
#'
#' @param x A complex vector.
#' @param y A complex vector.
#' @return A complex vector, result of conjugate(x) * y.
#' @export
correlate <- function(x, y) {
    Conj(x) * y
}


#' Get frequency cutoff indices for overlap calculation
#'
#' @references https://pycbc.org/pycbc/latest/html/_modules/pycbc/filter/matchedfilter.html#get_cutoff_indices
#'
#' @param flow Numeric. The lower frequency in Hz.
#' @param fupp Numeric. The upper frequency in Hz.
#' @param df Numeric. Frequency resolution (Hz).
#' @param N Integer. Number of time-domain samples.
#' @return A list with elements `min` and `max`, indicating cutoff indices.
#'
#' @export
get_cutoff_indices <- function(flow, fupp, df, N) {
    if (inherits(N, "numeric")) {
        N <- as.integer(N)
    }

    # For f_lower,
    if (flow) {
        kmin <- as.integer(flow / df)
        if (kmin < 0) {
            err_msg <- paste(
                "ValueError: Start frequency cannot be negative.",
                "Supplied value and kmin",
                flow,
                "and",
                kmin
            )
            stop(err_msg)
        } else {
            kmin <- 1
        }
    }

    # For f_upper,
    if (fupp) {
        kmax <- as.integer(fupp / df)
        if (kmax > as.integer((N + 1) / 2.)) {
            kmax <- as.integer((N + 1) / 2.)
        }
    } else {
        # int() truncates towards 0, so this is
        # equivalent to the floor of the float
        kmax <- as.integer((N + 1) / 2.)
    }

    # Is it possible that kmax is smaller than kmin?
    if (kmax <= kmin) {
        err_msg <-
            paste0(
                "ValueError: Kmax cannot be less than or equal to kmin. ",
                "Provided values of freqencies (min,max) were ",
                flow,
                " and ",
                fupp,
                " corresponding to (kmin, kmax) of ",
                kmin,
                " and ",
                kmax,
                "."
            )
        stop(err_msg)
    }

    return(c(kmin, kmax))
}


#' Compute inner product between two vectors
#'
#' @param x A complex or numeric vector.
#' @param y A complex or numeric vector.
#' @return Complex scalar. Inner product of `x` and `y`.
#' @export
inner <- function(x, y) {
    as.complex(Conj(x) %*% y)
}


#' Weighted inner product
#'
#' Computes the inner product between `x` and `y` with inverse-variance weighting.
#'
#' @param x A numeric or complex vector.
#' @param y A numeric or complex vector.
#' @param wt A numeric weight vector (same length as `x`, `y`).
#' @return Complex scalar. Weighted inner product.
#' @export
weighted_inner <- function(x, y, wt) {
    if (is.null(dim(x))) {
        if (length(wt) != length(x) || length(wt) != length(y)) {
            stop("Weight does not have same length as x or y.")
        }
    } else {
        if (all(dim(wt) != dim(x)) || all(dim(wt) != dim(y))) {
            stop("Weight does not have same dimension as x or y.")
        }
    }
    as.complex(Conj(x) %*% (y / wt))
}


#' Estimate signal loudness (sigma squared)
#'
#' Computes the matched-filter self-inner-product of a waveform in frequency domain.
#'
#' @references https://pycbc.org/pycbc/latest/html/_modules/pycbc/filter/matchedfilter.html#sigmasq
#'
#' @param htilde A `ts` or `fs` object. The waveform.
#' @param psd An optional `fs` PSD object.
#' @param low_frequency_cutoff Lower frequency bound (Hz).
#' @param high_frequency_cutoff Upper frequency bound (Hz).
#' @return Numeric. The unnormalized signal power.
#' @export
sigmasq <- function(
    htilde,
    psd = NULL,
    low_frequency_cutoff = NULL,
    high_frequency_cutoff = NULL) {
    # If htilde is ts, transform to fs.
    if (inherits(htilde, "ts")) {
        htilde <- to_fs(htilde)
    }

    # Output length
    N <- (length(htilde) - 1) * 2

    # Normalization factor
    norm <- 4.0 * deltaf(htilde)

    # Frequency range cutoff indices
    kidx <- get_cutoff_indices(
        flow = low_frequency_cutoff,
        fupp = high_frequency_cutoff,
        df = deltaf(htilde),
        N = N
    )

    # Cut htilde with kidx
    ht <- cutoff_to(htilde, kidx)

    # psd must have same deltaf as waveform (ht)
    if (!is.null(psd)) {
        tryCatch(
            expr = {
                testing.equal <- all.equal.numeric(
                    deltaf(ht),
                    deltaf(psd),
                    tol = 1e-5
                )
                if (testing.equal) {}
            },
            error = function(e) {
                stop(
                    paste(
                        "AssertionError:",
                        "Waveform does not have same deltaf as psd.",
                        testing.equal
                    )
                )
            }
        )
    }

    if (is.null(psd)) {
        # inner product without weight of psd.
        sq <- inner(ht, ht)
    } else {
        # inner product with psd weight.
        sq <- weighted_inner(ht, ht, wt = cutoff_to(psd, kidx))
    }

    return(Re(sq) * norm)
}


#' Matched filter (R ver.)
#'
#' I've digged out the source codes to translate from Python to R.
#'
#' @param template A `ts` object of waveform template.
#' @param data     A `ts` object of data.
#' @param psd      A `fs` object of PSD.
#' @param fl       A numeric. Lower frequency cutoff.
#' @param fu       A numeric. Upper frequency cutoff.
#' @param h.norm   A custom normalization factor.
#' @return A list containing: `snr.ts`, `snr.fs`, and `norm`, where SNR in `ts` and `fs` object and the normalization factor, respectively. Final SNR will be \code{norm*abs(snr.ts)}
#'
#'
#' @export
matched_filter <- function(
    template,
    data,
    psd = NULL,
    fl = NULL,
    fu = NULL,
    h.norm = NULL) {
    # FFT of given ts
    htilde <- to_fs(template)
    stilde <- to_fs(data)
    sampling.freq <- attr(stilde, "sampling.freq")

    # Check lengths
    if (length(htilde) != length(stilde)) {
        stop("ValueError: Length of template and data must match")
    }

    # Output ts length
    N <- (length(stilde) - 1) * 2

    # Indices in frequency series
    kidx <- get_cutoff_indices(flow = fl, fupp = fu, df = deltaf(stilde), N = N)

    # s_tilde x h*_tilde:
    #   the fft of s *times* the complex conjugate of the fft of h
    # ( correlate(x,y) = Conj(x) * y )
    qtilde.replace <- correlate(
        cutoff_to(htilde, kidx),
        cutoff_to(stilde, kidx)
    )

    # qtilde need to retain the length of N before ifft
    qtilde <- double(N)
    qtilde[kidx[1]:kidx[2]] <- qtilde.replace

    # Let qtilde behave fs
    qtilde <- copy_attr(
        qtilde,
        ref = stilde,
        which = c("sampling.freq", "delta_f", "class", "ti")
    )
    attr(qtilde, "flen") <- N

    # PSD needs to be given as fs
    if (!inherits(psd, "fs")) {
        stop("TypeError: PSD must be a fs class")
    }

    # Check lengths of psd and qtilde
    tryCatch(
        expr = {
            testing.equal <- all.equal.numeric(
                deltaf(qtilde),
                deltaf(psd),
                tol = 1e-5
            )
            if (testing.equal) {}
        },
        error = function(e) {
            stop(
                paste(
                    "AssertionError:",
                    "Data does not have same deltaf as psd.",
                    testing.equal
                )
            )
        }
    )

    # qtilde divided by psd
    qtilde[kidx[1]:kidx[2]] <- qtilde[kidx[1]:kidx[2]] /
        cutoff_to(psd, kidx)

    # Output SNR, need to be transformed into ts
    qs <- fft(qtilde, inverse = T) / sampling.freq
    class(qs) <- "complex"

    # Normalizing factor
    if (is.null(h.norm)) {
        h.norm <- sigmasq(htilde, psd, fl, fu)
    }

    # h.norm from `sigmasq` is actually normalized by 4.0 * deltaf(htilde)
    #     (4.0 * deltaf(stilde)) / (4.0 * deltaf(htilde) * Re(sq))
    #         = ( deltaf(stilde)/deltaf(htilde) ) / Re(sq)
    #
    # This is the reason why every expression of SNR by the matched filter in PyCBC
    # is having a factor of "4"!!!
    #
    # See note in function `pycbc.filter.matchedfilter.sigmasq`:
    #### Return the loudness of the waveform. This is defined (see Duncan Brown's
    #### thesis) as the unnormalized matched-filter of the input waveform, htilde,
    #### with itself. This quantity is usually referred to as (sigma)^2 and is then
    #### used to normalize matched-filters with the data.
    norm <- (4.0 * deltaf(stilde)) / sqrt(h.norm)

    # Final output
    # I skipped intermediate procedure from matched_filter_core() to matched_filter()
    snr.ts <- ts(data = qs, start = ti(data), deltat = deltat(data))
    snr.fs <- qtilde
    return(list("ts" = snr.ts, "fs" = snr.fs, "norm" = norm))
}

#' Calculate complex overlap
#'
#' @description
#' Return the complex overlap between the two TimeSeries or FrequencySeries.
#'
#' @param vec1 A `ts` or `fs`. The input vector containing a waveform.
#' @param vec2 A `ts` or `fs`. The input vector containing a waveform.
#' @param psd  A `fs` object (default: `NULL`). A power spectral density to weight the overlap.
#' @param low_frequency_cutoff  A numeric (default: `NULL`). The frequency to begin the overlap.
#' @param high_frequency_cutoff A numeric (default: `NULL`). The frequency to stop the overlap.
#' @param normalized A logical (default: `TRUE`). Set if the overlap is normalized. If true, it will range from 0 to 1.
#' @return overlap: complex
#'
#' @export
overlap_cplx <- function(
    vec1,
    vec2,
    psd = NULL,
    low_frequency_cutoff = NULL,
    high_frequency_cutoff = NULL,
    normalized = TRUE) {
    if (inherits(vec1, "ts")) {
        htilde <- to_fs(vec1)
    } else if (inherits(vec1, "fs")) {
        htilde <- vec1
    } else {
        stop("InputError: vec1 should be an object of ts or fs")
    }
    if (inherits(vec2, "ts")) {
        stilde <- to_fs(vec2)
    } else if (inherits(vec2, "fs")) {
        stilde <- vec2
    } else {
        stop("InputError: vec2 should be an object of ts or fs")
    }

    kidx <- get_cutoff_indices(
        flow = low_frequency_cutoff,
        fupp = high_frequency_cutoff,
        df = deltaf(stilde),
        N = (length(stilde) - 1) * 2
    )
    kmin <- kidx[1]
    kmax <- kidx[2]

    if (!is.null(psd)) {
        if (!inherits(psd, "fs")) {
            stop("InputError: psd should be an object of fs")
        }
        inner <- weighted_inner(
            x = htilde[kmin:kmax],
            y = stilde[kmin:kmax],
            wt = psd[kmin:kmax]
        )
    } else {
        inner <- inner(x = htilde[kmin:kmax], y = stilde[kmin:kmax])
    }

    if (normalized) {
        sig1 <- sqrt(sigmasq(
            htilde = vec1,
            psd = psd,
            low_frequency_cutoff = low_frequency_cutoff,
            high_frequency_cutoff = high_frequency_cutoff
        ))
        sig2 <- sqrt(sigmasq(
            htilde = vec2,
            psd = psd,
            low_frequency_cutoff = low_frequency_cutoff,
            high_frequency_cutoff = high_frequency_cutoff
        ))
        norm <- 1 / sig1 / sig2
    } else {
        norm <- 1
    }

    return(4 * deltaf(htilde) * inner * norm)
}

#' Calculate real overlap
#'
#' @description
#' Return the overlap between the two TimeSeries or FrequencySeries
#'
#' @param vec1 A `ts` or `fs`. The input vector containing a waveform.
#' @param vec2 A `ts` or `fs`. The input vector containing a waveform.
#' @param psd  A `fs` object (default: `NULL`). A power spectral density to weight the overlap.
#' @param low_frequency_cutoff  A numeric (default: `NULL`). The frequency to begin the overlap.
#' @param high_frequency_cutoff A numeric (default: `NULL`). The frequency to stop the overlap.
#' @param normalized A logical (default: `TRUE`). Set if the overlap is normalized. If true, it will range from 0 to 1.
#' @return overlap: numeric
#'
#' @export
overlap <- function(
    vec1,
    vec2,
    psd = NULL,
    low_frequency_cutoff = NULL,
    high_frequency_cutoff = NULL,
    normalized = TRUE) {
    Re(overlap_cplx(
        vec1,
        vec2,
        psd = psd,
        low_frequency_cutoff = low_frequency_cutoff,
        high_frequency_cutoff = high_frequency_cutoff,
        normalized = normalized
    ))
}


#' Interpolate PSD
#' Return a new PSD that has been interpolated to the desired delta_f
#'
#' @param fs      A `fs` object. Frequency series to be interpolated.
#' @param delta_f A numeric. The desired delta_f of the output
#'
#' @export
interp_psd <- function(fs, delta_f) {
    new_n <- (length(fs) - 1) * deltaf(fs) / delta_f + 1
    samples <- 0:(new_n - 1) * delta_f
    interp.fs <- approx(xout = samples, x = freqs(fs), y = fs)

    fs(interp.fs$y, df = delta_f, sampling.freq = attr(fs, "sampling.freq"))
}

#' Inverse Spectrum Truncation
#'
#' @param fs             A `fs` object of PSD.
#' @param max_filter_len A numeric.
#' @param fl             A numeric. Low-frequency-cutoff.
#'
#' @export
inv_spec_trunc_psd <- function(
    fs,
    max_filter_len,
    fl = NULL,
    trunc_method = NULL) {
    if (!is.null(fl) & (fl < 0 | fl > vf(freqs(fs)))) {
        stop(
            "ValueError: low_frequency_cutoff must be within the bandwidth of the PSD"
        )
    }

    max_filter_len <- trunc(max_filter_len)
    N <- (length(fs) - 1) * 2

    kmin <- ifelse(is.null(fl), 1, fl / deltaf(fs))
    inv_asd <- double(N)
    inv_asd[(kmin + 1):(N %/% 2)] <- (1.0 / fs[(kmin + 1):(N %/% 2)])^0.5
    q <- fftw::IFFT(inv_asd, plan = fftw::planFFT(length(inv_asd)))

    trunc_start <- max_filter_len %/% 2
    trunc_end <- N - max_filter_len %/% 2
    if (trunc_end < trunc_start) {
        stop("ValueError: Invalid value in inverse_spectrum_truncation")
    }

    if (!is.null(trunc_method)) {
        trunc_window <- trunc_method(max_filter_len)
        q[1:trunc_start] <- q[1:trunc_start] * tail(trunc_window, trunc_start)
        q[(trunc_end + 1):N] <- q[(trunc_end + 1):N] *
            head(trunc_window, max_filter_len %/% 2)
    }

    if (trunc_start < trunc_end) {
        q[trunc_start:trunc_end] <- 0
    }
    psd_trunc <- fftw::FFT(q, plan = fftw::planFFT(length(q)))
    psd_trunc <- psd_trunc[1:length(fs)]
    psd_trunc <- Conj(psd_trunc) * psd_trunc
    psd_out <- 1 / abs(psd_trunc)

    fs(psd_out, df = deltaf(fs), sampling.freq = attr(fs, "sampling.freq"))
}

#' Generate PSD
#'
#' @param ts A `ts` object.
#' @param sl A numeric. A segment length.
#' @param fl A numeric. Low-frequency-cutoff.
#' @return A `fs` object. PSD with interpolated and inverse-spectrum-truncated.
#'
#' @export
psd <- function(
    ts,
    sl = 4,
    fl = 15,
    delf = NULL,
    window_func = bspec::hannwindow) {
    if (is.null(delf)) {
        delf <- frequency(ts) / length(ts)
    }

    welch_psd <- bspec::welchPSD(
        ts,
        seglength = sl,
        method = "median",
        windowfun = window_func
    )
    first_psd <- fs(welch_psd$power,
        df = uniqdif(welch_psd$frequency),
        sampling.freq = frequency(ts)
    )
    secon_psd <- interp_psd(first_psd, delf)
    third_psd <- inv_spec_trunc_psd(
        secon_psd,
        max_filter_len = sl * frequency(ts),
        fl = fl,
        trunc_method = window_func
    )
    return(third_psd)
}

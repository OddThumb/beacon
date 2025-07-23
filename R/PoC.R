#' Loading waveforms from PyCBC
#'
#' @export
get_wave <- function(
    model.name,
    sampling.freq,
    fl = NULL,
    fu = NULL,
    m1 = NULL,
    m2 = NULL,
    d_L = NULL,
    inc = 0,
    ...,
    R.ts = TRUE,
    det,
    ra,
    dec,
    pol,
    t_gps,
    proj = FALSE
) {
    # Input:
    # ├─ model.name    : A character. Waveform model name.
    # ├─ sampling.freq : A numeric. Sampling frequency (Hz).
    # ├─ fl    : A numeric (default: NULL). Lower frequency bound (Hz).
    # ├─ fu    : A numeric (default: NULL). Upper frequency bound (Hz).
    # ├─ m1    : A numeric (default: NULL). Mass 1 (solar mass).
    # ├─ m2    : A numeric (default: NULL). Mass 2 (solar mass).
    # ├─ d_L   : A numeric (default: NULL). Luminosity distance (Mpc).
    # ├─ inc   : A numeric (default: 0). Inclination angle (radian).
    # ├─ ...   : Other arguments for waveform parameters.
    # ├─ R.ts  : A logical (default: TRUE). Whether returns in R ts object or
    # │          pycbc object.
    # │
    # │  (For antenna patterns)
    # ├─ dets  : A vector. Characters of detector abbreviations.
    # ├─ ra    : A numeric. RA of the GW source (radian).
    # ├─ dec   : A numeric. Dec of the GW source (radian).
    # ├─ pol   : A numeric. Polarization angle of the GW source (radian).
    # ├─ t_gps : A numeric. Passing time of the GW source to Earth (second).
    # └─ proj  : A logical (default: FALSE). Whether takes Earth rotation into
    #            account.
    #Output:
    # A list.
    # It has two elements of factors for the plus polarization and the cross
    # polarization.
    # If R.ts=TRUE, return list will have attributes

    # Generate waveforms
    get_td_waveform <- reticulate::import(
        'pycbc.waveform',
        convert = FALSE
    )$get_td_waveform
    waveforms <- get_td_waveform(
        approximant = model.name,
        mass1 = m1,
        mass2 = m2,
        distance = d_L,
        inclination = inc,
        ...,
        f_lower = fl,
        f_final = fu,
        delta_t = 1 / sampling.freq
    )
    waveforms <- list('hp' = waveforms[[0]], 'hc' = waveforms[[1]])

    # Antenna Pattern
    if (
        all(
            !hasArg(det),
            !hasArg(ra),
            !hasArg(dec),
            !hasArg(pol),
            !hasArg(t_gps)
        )
    ) {
        message("> Waveform WITHOUT the antenna pattern is calculated")
        AntPat <- F
    } else {
        message("> Antenna pattern is applied")

        if (proj) {
            message("> and Earth rotation is taken into account")
            waveforms$ht <- proj_wave(
                waveforms$hp,
                waveforms$hc,
                det,
                ra,
                dec,
                pol,
                t_gps
            )

            AntPat <- list(
                "proj" = TRUE,
                "det" = det,
                "ra" = ra,
                "dec" = dec,
                "pol" = pol,
                "t_gps" = t_gps
            )
        } else {
            add <- reticulate::import("operator")$add
            mul <- reticulate::import("operator")$mul
            ap <- get_antpatt(det, ra, dec, pol, t_gps)
            ht <- add(mul(ap$fp, waveforms$hp), mul(ap$fc, waveforms$hc))
            waveforms$ht <- ht

            AntPat <- list(
                "ap" = ap,
                "det" = det,
                "ra" = ra,
                "dec" = dec,
                "pol" = pol,
                "t_gps" = t_gps
            )
        }
    }
    Params <- list(
        "model" = model.name,
        "m1" = m1,
        "m2" = m2,
        "d_L" = d_L,
        "inc" = inc,
        "fl" = fl,
        "fu" = fu,
        "sampling.freq" = sampling.freq
    )
    # Output format
    if (R.ts) {
        waveforms <-
            lapply(waveforms, function(x) {
                ts(
                    reticulate::py_to_r(x$data),
                    start = reticulate::py_to_r(x$sample_times[0]),
                    frequency = reticulate::py_to_r(x$sample_rate)
                )
            })
    }
    attr(waveforms, "Params") <- Params
    attr(waveforms, "AntPat") <- AntPat
    return(waveforms)
}

#' Generate noise sample
#'
#' @export
gen_noise <- function(
    tlen,
    sampling.freq,
    fl = 5,
    delta_f = 1.0 / 16,
    noise_type = "aLIGOZeroDetHighPower",
    txt_path = NULL,
    noise.mean = 0,
    noise.sd = 1,
    seed = NULL
) {
    if (noise_type == "aLIGOZeroDetHighPower") {
        if (!is.null(seed)) {
            seed <- as.integer(seed)
        }

        # Load PyCBC modules
        pycbc.psd <- reticulate::import('pycbc.psd')
        pycbc.noise <- reticulate::import('pycbc.noise')

        # The color of the noise matches a PSD which you provide
        Nyq.freq <- sampling.freq / 2
        flen <- (Nyq.freq / delta_f) + 1
        colored.psd <- pycbc.psd$aLIGOZeroDetHighPower(
            as.integer(flen),
            delta_f,
            fl
        )

        # Generate 32 seconds of noise at 4096 Hz
        delta_t <- 1.0 / sampling.freq
        tsamples <- tlen / delta_t
        NOISE.from_psd <- pycbc.noise$noise_from_psd(
            as.integer(tsamples),
            delta_t,
            colored.psd,
            seed = seed
        )

        # Convert to ts object
        noise.ts <- ts(
            NOISE.from_psd$data,
            start = NOISE.from_psd$sample_times[0],
            frequency = sampling.freq
        )
    } else if (noise_type == "Gaussian") {
        set.seed(seed)
        noise.ts <- ts(
            rnorm(tlen * sampling.freq, mean = 0, sd = noise.sd),
            start = 0,
            frequency = sampling.freq
        )
    } else if (noise_type == "from_txt") {
        pycbc.psd <- reticulate::import("pycbc.psd", convert = F)
        pycbc.noise <- reticulate::import('pycbc.noise')

        Nyq.freq <- sampling.freq / 2
        flen <- (Nyq.freq / delta_f) + 1

        PSD.from_txt <- pycbc.psd$from_txt(
            filename = txt_path,
            length = as.integer(flen),
            delta_f = delta_f,
            low_freq_cutoff = fl
        )

        NOISE.from_txt <- pycbc.noise$noise_from_psd(
            length = as.integer(tlen * sampling.freq),
            delta_t = 1 / sampling.freq,
            psd = PSD.from_txt
        )

        # Convert to ts object
        noise.ts <- ts(
            NOISE.from_txt$data,
            start = NOISE.from_txt$sample_times[0],
            frequency = sampling.freq
        )
    } else {
        stop("TypeError: Wrong noise type")
    }

    return(noise.ts)
}

#' Generate colored noise sample
#'
#' @export
gen_colornoise <- function(tlen, fs, type, seed) {
    set.seed(seed)
    if (type == "mix") {
        noise.brown <- tuneR::noise(
            kind = "red",
            duration = tlen,
            samp.rate = fs,
            xunit = "time"
        )
        noise.white <- tuneR::noise(
            kind = "white",
            duration = tlen,
            samp.rate = fs,
            xunit = "time"
        )
        noise.pink <- tuneR::noise(
            kind = "pink",
            duration = tlen,
            samp.rate = fs,
            xunit = "time"
        )
        noise.power <- tuneR::noise(
            kind = "power",
            duration = tlen,
            samp.rate = fs,
            xunit = "time"
        )
        noise <- noise.brown@left +
            noise.white@left +
            noise.pink@left +
            noise.power@left
    } else {
        noise <- tuneR::noise(
            kind = type,
            duration = tlen,
            samp.rate = fs,
            xunit = "time"
        )@left
    }

    ts(noise, start = 0, frequency = 4096)
}

#' Generating random noise and inserting signal into the noise
#'
#' @export
gen_data <- function(
    signal,
    sampling_freq,
    beta = 1,
    tlen = 32,
    pre.sec = 2,
    fl = 5,
    delta_f = 1 / 32,
    noise_type = "aLIGOZeroDetHighPower",
    txt_path = NULL,
    t0.signal = F,
    noise.seed = NULL
) {
    # To generate same noise
    if (!is.null(noise.seed)) {
        set.seed(noise.seed)
    }

    # Checking appropriate length of noise
    N <- (tlen - pre.sec) * sampling_freq
    if (N < length(signal)) {
        stop('Error: given N < length(signal)!')
    }

    # Noise with the scale of beta
    noise <- beta *
        gen_noise(
            tlen,
            sampling_freq,
            fl = fl,
            delta_f = delta_f,
            noise_type = noise_type,
            txt_path = txt_path,
            noise.mean = 0,
            noise.sd = sd(signal),
            seed = noise.seed
        )

    # Matching phase
    signal <- shift_phase(signal, noise)

    # Random signal starting index
    signal.ind <- runif(1, min = 0, max = N - length(signal)) %>% trunc()

    # Padding signal with noise length
    pre.ind <- pre.sec * sampling_freq
    signal.pad <- c(
        rep(0, signal.ind + pre.ind),
        signal,
        rep(0, N - length(signal) - signal.ind)
    )

    # Start time and event time of simulation data w.r.t. signal time
    start_time <- rev(
        seq(
            from = beacon::ti(signal),
            to = beacon::ti(signal) - (signal.ind / sampling_freq + pre.sec),
            by = -(1 / sampling_freq)
        )
    )[1]
    tevent <- beacon::ti(signal)

    # If start_time is shifted to 0,
    if (!t0.signal) {
        tevent <- tevent - start_time
        start_time <- 0
    }

    # Padded signal w.r.t. noise time length
    signal.pad <- ts(signal.pad, start = start_time, frequency = sampling_freq)

    # Injecting signal by add
    signal.noise <- ts(
        c(noise) + c(signal.pad),
        start = start_time,
        frequency = sampling_freq
    )

    # signal
    signal <- ts(c(signal), start = tevent, frequency = sampling_freq)

    return(list(
        'data' = signal.noise,
        'signal' = signal,
        'padded' = signal.pad,
        'tevent' = tevent
    ))
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

#' Get available GW detectors
#'
#' Queries the list of available detectors from PyCBC and returns location info.
#' @details
#' This function interfaces with Python's \pkg{pycbc} library using \pkg{reticulate}.
#' Please ensure that the required Python packages are available in your environment.
#'
#' @return A data frame with columns: \code{Detector Name}, \code{Abbreviation}, \code{latitude}, \code{longitude}.
#'
#' @importFrom reticulate import
#' @export
get_available_detectors <- function() {
    # functions from external libraries
    bind_rows <- dplyr::bind_rows
    Detector <- reticulate::import("pycbc.detector")$Detector
    get_available_detectors <- reticulate::import(
        "pycbc.detector"
    )$get_available_detectors

    detector.list <- get_available_detectors()
    det.df <- lapply(detector.list, function(idx) {
        abv <- idx[[1]]
        name <- idx[[2]]
        d <- Detector(abv)
        data.frame(
            "Detector Name" = name,
            "Abbreviation" = abv,
            "latitude" = d$latitude,
            "longitude" = d$longitude
        )
    })
    dplyr::bind_rows(det.df)
}

#' Light travel time between detectors
#'
#' Computes the pairwise light-travel times between gravitational-wave detectors.
#' @details
#' This function interfaces with Python's \pkg{pycbc} library using \pkg{reticulate}.
#' Please ensure that the required Python packages are available in your environment.
#'
#' @param dets A character vector of detector abbreviations (e.g., \code{c("H1", "L1")}).
#' @return A data frame of light-travel times (upper triangular only).
#'
#' @importFrom reticulate import
#' @export
travel_times <- function(dets = c("H1", "L1", "V1", "K1")) {
    Detector <- reticulate::import("pycbc.detector")$Detector
    dets.grid <- expand.grid(dets, dets)

    ifo.mat <- matrix(nrow = length(dets), ncol = length(dets))
    rownames(ifo.mat) <- dets
    colnames(ifo.mat) <- dets
    for (i in 1:nrow(dets.grid)) {
        ifo1 <- dets.grid[i, 1]
        ifo2 <- dets.grid[i, 2]
        dt <- Detector(ifo1)$light_travel_time_to_detector(Detector(ifo2))
        ifo.mat[ifo1, ifo2] <- dt
    }
    ifo.mat[lower.tri(ifo.mat, diag = T)] <- NA
    as.data.frame(ifo.mat)
}

#' Compute maximum light travel time between detectors
#'
#' Given a set of detector names, compute the light travel time between all unique pairs
#' using PyCBC's Detector geometry.
#'
#' @param dets A character vector of detector names (e.g., \code{c("H1", "L1", "V1")}).
#'
#' @return A named numeric vector of light travel times (in seconds) between each detector pair.
#'         The names are formatted as \code{"H1-L1"}, \code{"H1-V1"}, etc.
#'
#' @examples
#' \dontrun{
#' get_light_travel_time_among_detectors(c("H1", "L1", "V1"))
#' }
#'
#' @importFrom reticulate import
#' @export
get_light_travel_time_among_detectors <- function(dets) {
    Detector <- reticulate::import("pycbc.detector")$Detector
    det_combn <- t(combn(dets, 2))
    dt_combn <- c()
    dt_names <- c()
    for (ii in 1:nrow(det_combn)) {
        det1 <- det_combn[ii, 1]
        det2 <- det_combn[ii, 2]
        dt_crit <- Detector(det1)$light_travel_time_to_detector(Detector(det2))
        dt_combn[ii] <- dt_crit
        dt_names[ii] <- paste0(det1, '-', det2)
    }
    names(dt_combn) <- dt_names
    dt_combn
}


#' Relative arrival times of signal at detectors
#'
#' Compute arrival time delays among detectors for a given sky position.
#' @details
#' This function interfaces with Python's \pkg{pycbc} library using \pkg{reticulate}.
#' Please ensure that the required Python packages are available in your environment.
#'
#' @param deltat A numeric. Time resolution to round delays (in seconds).
#' @param dets A character vector of detector abbreviations.
#' @param ra A numeric. Right Ascension of the source (in radians).
#' @param dec A numeric. Declination of the source (in radians).
#' @param t_gps A numeric. GPS time of signal arrival at Earth's center.
#' @param verbose Logical. If \code{TRUE}, print intermediate results.
#'
#' @return If single detector, a list with \code{time_rel} and \code{remainder}. If multiple detectors, a list with matrix \code{dt_rel}, absolute delays \code{dt_earth}, and \code{remainder}.
#'
#' @importFrom reticulate import
#' @export
relpass_time <- function(deltat, dets, ra, dec, t_gps, verbose = T) {
    Detector <- reticulate::import("pycbc.detector")$Detector

    if (length(dets) == 1) {
        dt <- Detector(dets)$time_delay_from_earth_center(ra, dec, t_gps)
        dt_q <- (dt %/% deltat) * deltat
        dt_r <- dt %% deltat
        if (verbose) {
            message("> Time delay of a detector '", dets, "' from Earth center")
        }
        return(list("time_rel" = dt_q, "remainder" = dt_r))
    } else {
        dt_refs <- c()
        for (det in dets) {
            dt_refs[det] <- Detector(det)$time_delay_from_earth_center(
                ra,
                dec,
                t_gps
            )
        }

        det.mat <- matrix(nrow = length(dets), ncol = length(dets))
        rownames(det.mat) <- paste("Ref:", dets)
        colnames(det.mat) <- paste("Pass:", dets)

        dt_rs <- c()
        dets.grid <- expand.grid(dets, dets)
        for (i in 1:nrow(dets.grid)) {
            dref <- dets.grid[i, 1]
            dpass <- dets.grid[i, 2]
            dt <- Detector(dpass)$time_delay_from_detector(
                Detector(dref),
                ra,
                dec,
                t_gps
            )
            quota <- dt %/% deltat
            quota <- ifelse(quota < 0, quota + 1, quota)
            dt_q <- quota * deltat
            dt_r <- dt - dt_q
            names(dt_r) <- paste0("Ref: ", dref, " / Pass: ", dpass)
            dt_rs <- append(dt_rs, dt_r)
            det.mat[paste("Ref:", dref), paste("Pass:", dpass)] <- dt_q
        }
        det.df <- as.data.frame(det.mat)
        remainders <- data.frame("remainder" = dt_rs[dt_rs != 0])

        if (verbose) {
            message(
                "> Time delays among detectors '",
                paste(dets, collapse = ', '),
                "'"
            )
            print(det.df)
            message(
                "> Because the time delay is not multiple of time resolution,"
            )
            message(
                "  following remainders are subtracted from preliminary time delay."
            )
            print(remainders)
        }

        return(list(
            "dt_rel" = det.df,
            "dt_earth" = dt_refs,
            "remainder" = remainders
        ))
    }
}

#' Compute antenna pattern for a detector
#'
#' @details
#' This function interfaces with Python's \pkg{pycbc} library using \pkg{reticulate}.
#' Please ensure that the required Python packages are available in your environment.
#'
#' @param det A character. Detector abbreviation (e.g., "H1").
#' @param ra A numeric. Right Ascension (radians).
#' @param dec A numeric. Declination (radians).
#' @param pol A numeric. Polarization angle (radians).
#' @param t_gps A numeric. GPS time of signal arrival at Earth's center.
#' @param online Logical. Whether to use online IERS data (default: FALSE).
#'
#' @return A named list with \code{fp} and \code{fc} antenna pattern coefficients.
#'
#' @importFrom reticulate import
#' @export
get_antpatt <- function(det, ra, dec, pol, t_gps, online = F) {
    # Set astropy IERS (Earth rotation) mode to online or offline
    # Controls whether PyCBC's astropy backend is allowed to auto-download IERS tables.
    set_iers <- function(online = F) {
        iers <- reticulate::import("astropy.utils")$iers
        iers$conf$auto_download <- online
    }

    # If online=T, it takes quite long
    set_iers(online)
    if (
        any(
            !hasArg(det),
            !hasArg(ra),
            !hasArg(dec),
            !hasArg(pol),
            !hasArg(t_gps)
        )
    ) {
        stop(
            "All 4 arguments of 'ra', 'dec', 'pol', 't_gps' needs to be given for applying the antenna pattern. "
        )
    }

    Detector <- reticulate::import("pycbc.detector")$Detector
    d <- Detector(det)
    ap <- d$antenna_pattern(
        right_ascension = ra,
        declination = dec,
        polarization = pol,
        t_gps = t_gps
    )
    names(ap) <- c("fp", "fc")

    return(ap)
}

#' Project GW polarizations onto detector
#'
#' Computes detector strain by projecting plus and cross polarizations using Earth rotation and antenna pattern.
#'
#' @details
#' This function interfaces with Python's \pkg{pycbc} library using \pkg{reticulate}.
#' Please ensure that the required Python packages are available in your environment.
#'
#' @param hp A \code{ts} object. Plus polarization waveform.
#' @param hc A \code{ts} object. Cross polarization waveform.
#' @param det A character. Detector abbreviation.
#' @param ra A numeric. Right Ascension (radians).
#' @param dec A numeric. Declination (radians).
#' @param pol A numeric. Polarization angle (radians).
#' @param t_gps A numeric. GPS time of source arrival at Earth's center.
#'
#' @return A projected waveform as a \code{ts} object.
#'
#' @importFrom reticulate import
#' @export
proj_wave <- function(hp, hc, det, ra, dec, pol, t_gps) {
    TimeSeries <- reticulate::import("pycbc.types", convert = F)$TimeSeries
    add <- reticulate::import("operator", convert = F)$add
    Detector <- reticulate::import("pycbc.detector", convert = F)$Detector

    hp.TS <- TimeSeries(hp, epoch = ti(hp) + t_gps, delta_t = deltat(hp))
    hc.TS <- TimeSeries(hc, epoch = ti(hc) + t_gps, delta_t = deltat(hc))

    d <- Detector(det)
    ht <- d$project_wave(
        hp = hp.TS,
        hc = hc.TS,
        ra = ra,
        dec = dec,
        polarization = pol,
        method = "lal"
    )
    ts(
        reticulate::py_to_r(ht$data),
        start = reticulate::py_to_r(ht$sample_times$data[0]),
        deltat = reticulate::py_to_r(ht$delta_t)
    )
}

#' Project GW polarizations onto multiple detectors
#'
#' Applies antenna pattern projection to gravitational-wave polarizations (\code{hp}, \code{hc})
#' for a network of detectors, taking into account relative arrival times due to Earth's rotation.
#'
#' @param hp A \code{ts} object. Plus polarization waveform.
#' @param hc A \code{ts} object. Cross polarization waveform.
#' @param dets A character vector of detector abbreviations (e.g., \code{c("H1", "L1")}).
#' @param ra A numeric. Right Ascension of the source (in radians).
#' @param dec A numeric. Declination of the source (in radians).
#' @param pol A numeric. Polarization angle (in radians).
#' @param t_gps A numeric. GPS time of signal arrival at Earth's center.
#' @param t_ref A character. Reference detector for computing relative arrival times (default: \code{"H1"}). [Currently unused]
#'
#' @details
#' For each detector in \code{dets}, the function computes the relative arrival time of the signal
#' based on its sky location. It then applies \code{\link{proj_wave}} to project the polarizations
#' onto each detector using the corresponding time and antenna pattern.
#'
#' @return A named list of projected \code{ts} objects for each detector, with the following attributes:
#' \describe{
#'   \item{\code{"tgps.net"}}{Vector of GPS arrival times per detector.}
#'   \item{\code{"dt_rel"}}{Matrix of pairwise relative time delays between detectors.}
#'   \item{\code{"tgps_remainder"}}{Rounding residuals for each detector’s delay.}
#' }
#'
#' @note Requires Python environment with \pkg{pycbc} available via \pkg{reticulate}.
#'
#' @examples
#' \dontrun{
#' hp <- ts(sin(seq(0, 1, length.out = 4096)), start = 1000000000, fs = 4096)
#' hc <- ts(cos(seq(0, 1, length.out = 4096)), start = 1000000000, fs = 4096)
#' proj <- proj_network(hp, hc, dets = c("H1", "L1"), ra = 1.0, dec = -0.5, pol = 0.0, t_gps = 1126259462)
#' }
#'
#' @export
proj_network <- function(hp, hc, dets, ra, dec, pol, t_gps, t_ref = "H1") {
    if (length(dets) == 1L) {
        stop("InputError: `dets` must have length larger than 1")
    }

    # Relative passing time on each detector (reference="H1")
    reltime.net <- relpass_time(
        deltat = deltat(hp),
        dets = dets,
        ra = ra,
        dec = dec,
        t_gps = t_gps,
        verbose = F
    )
    tgps.net <- t_gps + reltime.net$dt_earth

    patterned.lis <- mapply(
        function(det, tgps) {
            proj_wave(
                hp = hp,
                hc = hc,
                det = det,
                ra = ra,
                dec = dec,
                pol = pol,
                t_gps = t_gps
            )
        },
        det = dets,
        tgps = tgps.net,
        SIMPLIFY = F
    )

    attr(patterned.lis, "tgps.net") <- tgps.net
    attr(patterned.lis, "dt_rel") <- reltime.net$dt_rel
    attr(patterned.lis, "tgps_remainder") <- reltime.net$remainder
    patterned.lis
}

#' Simulate Gaussian noise from detector PSD
#'
#' Generate colored Gaussian noise with power spectral density of a given detector.
#'
#' @details
#' This function interfaces with Python's \pkg{pycbc} library using \pkg{reticulate}.
#' Please ensure that the required Python packages are available in your environment.
#'
#' @param det A character. Detector abbreviation ("H1", "L1", "V1", "K1", or "E1").
#' @param duration A numeric. Noise duration (in seconds).
#' @param tstart A numeric. Start GPS time of the noise.
#' @param sampling.freq A numeric. Sampling frequency (Hz). Default: 4096.
#' @param fl A numeric. Lower frequency cutoff. Default: 15 Hz.
#' @param delta_f A numeric. Frequency resolution. Default: 1/32.
#' @param seed Optional numeric. Random seed for reproducibility.
#'
#' @return A \code{ts} object of simulated Gaussian noise.
#'
#' @importFrom reticulate import
#' @export
psd_noise <- function(
    det,
    duration,
    tstart,
    sampling.freq = 4096,
    fl = 15,
    delta_f = 1 / 32,
    seed = NULL
) {
    if (det == "H1" | det == "L1") {
        psd.name <- "aLIGODesignSensitivityT1800044"
    } else if (det == "V1") {
        psd.name <- "AdvVirgo"
    } else if (det == "K1") {
        psd.name <- "KAGRADesignSensitivityT1600593"
    } else if (det == "E1") {
        psd.name <- "EinsteinTelescopeP1600143"
    } else {
        stop('For now, available `det` are "H1", "L1", "V1", "K1", and "E1"')
    }
    pycbc.psd <- reticulate::import("pycbc.psd")
    pycbc.noise <- reticulate::import("pycbc.noise")

    # Simulating PSD
    Nyq.freq <- sampling.freq / 2
    flen <- (Nyq.freq / delta_f) + 1
    PSD <- pycbc.psd[[psd.name]](as.integer(flen), delta_f, fl)

    # Generating noise from PSD
    if (!is.null(seed)) {
        seed <- as.integer(seed)
    }
    tlen <- duration * sampling.freq + 1 # time stamp must be 1 index larger than dur*fs
    NOISE.from_psd <- pycbc.noise$noise_from_psd(
        as.integer(tlen),
        1 / sampling.freq,
        PSD,
        seed
    )

    # Convert to ts
    noise.ts <- ts(
        NOISE.from_psd$data,
        start = tstart,
        frequency = sampling.freq
    )

    attr(noise.ts, "det") <- det
    attr(noise.ts, "psd.name") <- psd.name
    attr(noise.ts, "fl") <- fl
    attr(noise.ts, "deltaf") <- delta_f

    return(noise.ts)
}


#' Generate gravitational-wave signal from PyCBC
#'
#' Calls PyCBC waveform generator and optionally applies antenna projection and Earth rotation.
#'
#' @details
#' This function interfaces with Python's \pkg{pycbc} library using \pkg{reticulate}.
#' Please ensure that the required Python packages are available in your environment.
#'
#' @param model.name A character. Waveform approximant name (e.g., "IMRPhenomPv2").
#' @param sampling.freq A numeric. Sampling rate in Hz.
#' @param fl A numeric. Lower frequency bound (Hz).
#' @param fu A numeric. Upper frequency bound (Hz).
#' @param m1 A numeric. Mass 1 (solar masses).
#' @param m2 A numeric. Mass 2 (solar masses).
#' @param d_L A numeric. Luminosity distance (Mpc).
#' @param inc A numeric. Inclination angle (radians). Default: 0.
#' @param ... Additional waveform parameters.
#' @param R.ts Logical. If \code{TRUE}, return result as R \code{ts} objects.
#' @param det A character. Detector abbreviation.
#' @param ra A numeric. Right Ascension (radians).
#' @param dec A numeric. Declination (radians).
#' @param pol A numeric. Polarization angle (radians).
#' @param t_gps A numeric. GPS time of signal arrival.
#' @param proj Logical. If \code{TRUE}, apply full Earth-rotation-based projection.
#'
#' @return A list with waveform components (\code{hp}, \code{hc}, \code{ht}) and attributes:
#' \itemize{
#'   \item \code{Params} – generation parameters
#'   \item \code{AntPat} – antenna pattern info
#' }
#'
#' @importFrom reticulate import
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

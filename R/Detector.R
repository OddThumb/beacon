# Detector ----

#' Set astropy.utils.iers runs in online or offline
#'
#' @export
set_iers <- function(online = F) {
    iers <- reticulate::import("astropy.utils")$iers
    iers$conf$auto_download <- online
}

#' Get available GW detectors
#'
#' @return A data frame. Output includes columns of "Detector name", "Abbreviation" of detector name, latitude and longitude of the detector.
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
    bind_rows(det.df)
}

#' Light travel time among detectors
#'
#' @param dets  A vector. Charcters of detector abbreviations.
#' @return A data frame. Light travel time values among detectors are in only upper triangular matrix.
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

#' Time source gravitational-wave passes through detector
#'
#' @param dets  A vector. Characters of detector abbreviations.
#' @param ra    A numeric. RA of the GW source in radian.
#' @param dec   A numeric. Dec of the GW source in radian.
#' @param t_gps A numeric. Passing time of the GW source to Earth in gps second.
#' @return A numeric. (If 'dets' has one argument)
#'         A data frame. (If 'dets' is multiple)
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
            dt_r <- (dt - dt_q) |>
                `names<-`(paste0("Ref: ", dref, " / Pass: ", dpass))
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

#' Calculate Antenna Pattern
#'
#' @param dets  A vector. Characters of detector abbreviations.
#' @param ra    A numeric. RA of the GW source in radian.
#' @param dec   A numeric. Dec of the GW source in radian.
#' @param pol   A numeric. Polarization angle of the GW source in radian.
#' @param t_gps A numeric. Passing time of the GW source to Earth in gps second.
#' @return A list. It has two elements of factors for the plus (+) polarization and the cross (x) polarization.
#' @export
get_antpatt <- function(det, ra, dec, pol, t_gps, online = F) {
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

#' The projection process can also take into account the rotation of the Earth using the project wave function.
#'
#' @param hp    A ts object. A plus polarization signal template.
#' @param hc    A ts object. A cross polarization signal template.
#' @param dets  A vector. Characters of detector abbreviations.
#' @param ra    A numeric. RA of the GW source in radian.
#' @param dec   A numeric. Dec of the GW source in radian.
#' @param pol   A numeric. Polarization angle of the GW source in radian.
#' @param t_gps A numeric. Passing time of the GW source to Earth in gps second.
#' @return A Python pycbc object. A projected signal template.
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

#' Gaussian noise from detetctor PSD
#'
#' @param det           A character. An abbreviation of detector name.
#'                      (this ver., H1, L1, V1, K1, or E1)
#' @param duration      A numeric. A length of noise in second.
#' @param tstart        A numeric. Starting time of the noise.
#' @param sampling.freq A numeric (default: 4096). A sampling rate in Hz.
#' @param fl            A numeric (default: 32). A lower frequency cutoff.
#' @param delta_f       A numeric (default: 1/32). A frequency resolution.
#' @param seed          A numeric (default: NULL). Random seed.
#' @return ts object of Gaussian noise.
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

#' Iterate over the Q values
#'
#' @param qrange A numeric vector. upper and lower bounds of q range.
#' @param deltam A numeric. Fractional mismatch between neighboring tiles.
#' @return A numeric. Q value for Q-tile
.iter_qs <- function(qrange, deltam) {
    # work out how many Qs we need
    cumum <- log(qrange[2] / qrange[1]) / 2^(1 / 2)
    nplanes <- max(ceiling(cumum / deltam), 1)
    dq <- cumum / nplanes

    qs <- c()
    for (i in 1:nplanes) {
        qs[i] <- qrange[1] * exp(2^(1 / 2) * dq * ((i - 1) + 0.5)) # ((i-1)+0.5)
    }
    return(qs)
}

#' Fractional mismatch between neighbouring tiles
#'
#' @param mismatch A numeric. percentage of desired fractional mismatch.
#' @return A numeric.
#' @export
deltam_f <- function(mismatch) {
    2 * (mismatch / 3.)^(1 / 2.)
}

#' Iterate over the frequencies of this 'Qplane'
#'
#' @param q        A numeric. q value.
#' @param frange   A numeric vector. upper and lower bounds of frequency range.
#' @param mismatch A numeric. percentage of desired fractional mismatch.
#' @param dur      A numeric. duration of timeseries in seconds.
#' @return A numeric vector. Q-Tile frequency.
.iter_freqs <- function(q, frange, mismatch, duration) {
    minf <- frange[1]
    maxf <- frange[2]
    fcum_mismatch <- log(maxf / minf) * (2 + q^2)^(1 / 2) / 2
    nfreq <- as.integer(max(1, ceiling(fcum_mismatch / deltam_f(mismatch))))
    fstep <- fcum_mismatch / nfreq
    fstepmin <- 1 / duration
    qfrq <- c()
    for (i in 1:nfreq) {
        qfrq[i] <- (minf *
            exp(2 / (2 + q^2)^(1 / 2) * ((i - 1) + 0.5) * fstep)) %/%
            fstepmin *
            fstepmin # ((i-1)+0.5)
    }
    return(qfrq)
}

#' Q-tiling
#'
#' @param fseries  A `fs` object. frequency-series data set.
#' @param qrange   A numeric vector. upper and lower bounds of q range.
#' @param frange   A numeric vector. upper and lower bounds of frequency range.
#' @param mismatch A numeric. percentage of desired fractional mismatch.
#' @return A list containing Q-tile vectors for a set of Q-planes.
#' @export
qtiling <- function(fseries, qrange, frange, mismatch = 0.2) {
    qplane.tile.list <- list()
    qs <- .iter_qs(qrange, deltam_f(mismatch))
    for (i in 1:length(qs)) {
        q <- qs[i]
        qplane.tile.list[[i]] <- list(
            "q" = q,
            "qfrq" = .iter_freqs(q, frange, mismatch, dur(fseries))
        )
    }
    return(qplane.tile.list)
}

#' Q-series
#'
#' @param fseries        A `fs` object. frequency-series data set.
#' @param Q              A numeric. q value.
#' @param f0             A numeric. central frequency.
#' @param return_complex A logical (default: FALSE). Return the raw complex series instead of the normalized power.
#' @return A `ts` of the normalized energy from the Q-transform of this tile against the data.
#' @export
qseries <- function(fseries, Q, f0, return_complex = FALSE) {
    # normalize and generate bi-square window
    qprime <- Q / 11^(1 / 2)
    norm <- sqrt(315 * qprime / (128 * f0))
    window_size <- 2 * as.integer(f0 / qprime * dur(fseries)) + 1
    xfreqs <- seq(-1, 1, length.out = window_size)

    fstart <- as.integer((f0 - (f0 / qprime)) * dur(fseries))
    fend <- as.integer(fstart + window_size)
    center <- (fstart + fend) %/% 2
    # center should be a center value? or index?
    # > c(3,4,5,6,7)
    # (1) value: center=5
    # (2) index: center=3
    # At the line with the function 'cyclic' which will use this 'center' later, the shift by this center results in
    # (1) value > asymmetric vector
    # (2) index > the symmetric side-by-side (head and tail concentrated) vector.
    # Let's assume "(2) index".

    tlen <- attr(fseries, "tlen")
    # windowed <- fseries[fstart:(fend-1)] * (1-xfreqs^2)^2 * norm # This is Python-based
    windowed <- fseries[(fstart + 1):fend] * (1 - xfreqs^2)^2 * norm
    windowed <- c(windowed, rep(0, tlen - window_size))
    windowed <- cyclic(windowed, center)

    # calculate the time series for this q -value
    windowed <- fs(windowed, df = deltaf(fseries))
    ifft.res <- fftw::IFFT(windowed, plan = fftw::planFFT(tlen))

    if (return_complex) {
        ctseries <- ts(
            ifft.res,
            start = attr(fseries, "ti"),
            frequency = attr(fseries, "sampling.freq")
        )
        return(ctseries)
    } else {
        energy <- sapply(ifft.res, pracma::Norm)^2
        medianenergy <- median(energy)
        energy_med <- ts(
            energy / medianenergy,
            start = attr(fseries, "ti"),
            frequency = attr(fseries, "sampling.freq")
        )
        return(energy_med)
    }
}

#' Q-plane
#'
#' @param qplane.tile.list A list containing a vector of q-tile for each q-plane.
#' @param fseries          A `fs` object. frequency-series data set.
#' @param return_complex   A logical. Return the raw complex series instead of the normalized power.
#' @return A list containing q: The q of the maximum q plane, times: The time that the qtransform is sampled, freqs: The frequencies that the qtransform is sampled, and qplane (2d): The two dimensional interpolated qtransform of this time series.
#' @export
qplane <- function(qplane.tile.list, fseries, return_complex = FALSE) {
    # store q-transforms for each q in a list
    max_energy <- NA
    max_key <- NA

    qplanes <- list()
    for (i in 1:length(qplane.tile.list)) {
        q <- qplane.tile.list[[i]]$q
        qfrq <- qplane.tile.list[[i]]$qfrq

        energies <- matrix(nrow = attr(fseries, "tlen"), ncol = length(qfrq))
        for (j in 1:length(qfrq)) {
            f0 <- qfrq[j]
            energy <- qseries(fseries, q, f0, return_complex = return_complex)
            menergy <- amax(energy)
            energies[, j] <- energy

            if (i == 1 | menergy > max_energy) {
                max_energy <- menergy
                max_key <- i
                max_q <- q
            }
        }
        qplanes[[i]] <- list("q" = q, "energies" = energies)
    }

    # record q-transform output for peak q
    plane <- qplanes[[max_key]]$energies
    freqs <- qplane.tile.list[[max_key]]$qfrq
    times <- time(energy)
    ret.list <- list("times" = times, "freqs" = freqs, "plane" = plane)
    attr(ret.list, "max_key") <- max_q
    return(ret.list)
}

#' Surface Interpolation
#'
#' @description
#' A mutated function of `fields::interp.surface` and `fields::interp.surface.grid`. This function is boosted up by `sapply`.
#'
#' @param x A numeric vector to be interpolated.
#' @param y A numeric vector to be interpolated.
#' @param z A numeric matrix to be interpolated. `dim(z) == c(length(x), length(y))`.
#' @param xout A numeric vector. The output grid.
#' @param yout A numeric vector. The output grid.
#' @param method A character. Available methods is `linear` or `spline`.
#' @return `zout`, the interpolated 2d matrix.
#' @export
interp2d <- function(x, y, z, xout, yout, method = "linear") {
    interp.surface <- function(x, y, z, loc, method = "linear") {
        # Inspired by fields::interp.surface
        nx <- length(x)
        ny <- length(y)
        ll <- switch(
            method,
            "linear" = list(
                lx = approx(x = x, y = 1:nx, xout = loc[, 1], rule = 2)$y,
                ly = approx(x = y, y = 1:ny, xout = loc[, 2], rule = 2)$y
            ),
            "spline" = list(
                lx = spline(x = x, y = 1:nx, xout = loc[, 1])$y,
                ly = spline(x = y, y = 1:ny, xout = loc[, 2])$y
            )
        )
        lx <- ll$lx
        ly <- ll$ly
        lx1 <- floor(lx)
        ly1 <- floor(ly)
        ex <- lx - lx1
        ey <- ly - ly1
        ex[lx1 == nx] <- 1
        ey[ly1 == ny] <- 1
        lx1[lx1 == nx] <- nx - 1
        ly1[ly1 == ny] <- ny - 1
        ret <- z[cbind(lx1, ly1)] *
            (1 - ex) *
            (1 - ey) +
            z[cbind(lx1 + 1, ly1)] * ex * (1 - ey) +
            z[cbind(lx1, ly1 + 1)] * (1 - ex) * ey +
            z[cbind(lx1 + 1, ly1 + 1)] * ex * ey
        return(ret)
    }

    N <- length(yout)
    zout <- sapply(xout, function(xi) {
        interp.surface(x, y, z, cbind(rep(xi, N), yout), method = method)
    })
    list(x = xout, y = yout, z = zout)
}

#' Q-transform (R ver.)
#'
#' @param ts        A `ts` object.
#' @param delta_t   A numeric. The time resolution to interpolate to.
#' @param delta_f   A numeric. The frequency resolution to interpolate to.
#' @param logfsteps A numeric. Do a log interpolation (incompatible with delta_f option) and set the number of steps to take.
#' @param frange    A numeric vector. frequency range.
#' @param qrange    A numeric vector. q range.
#' @param mismatch  A numeric (default: 0.2). Mismatch between frequency tiles.
#' @param return_complex A logical (default: FALSE). return the raw complex series instead of the normalized power.
#' @return A list containing times: The time that the qtransform is sampled, freqs: The frequencies that the qtransform is sampled, and qplane (2d): The two dimensional interpolated qtransform of this time series.
#' @export
qtransform <- function(
    ts,
    delta_t = NULL,
    delta_f = NULL,
    logfsteps = NULL,
    frange = NULL,
    qrange = c(4, 64),
    mismatch = 0.2,
    return_complex = FALSE
) {
    if (is.null(frange)) {
        frange <- c(30, as.integer(frequency(ts) / 2 * 8))
    }

    fseries <- to.fs(ts)
    q_base <- qtiling(fseries, qrange, frange, mismatch)
    qpl.res <- qplane(q_base, fseries, return_complex = return_complex)
    times <- qpl.res$times
    freqs <- qpl.res$freqs
    q_plane <- qpl.res$plane

    if (!is.null(logfsteps) & !is.null(delta_f)) {
        stop("ValueError: Provide only one (or NULL) of delta_f and logfsteps")
    }

    # Interpolate if requested
    if (!is.null(delta_t)) {
        interp_times <- seq(ti(ts), tf(ts), delta_t)
    } else {
        interp_times <- times
    }

    if (!is.null(delta_f)) {
        interp_freqs <- seq(frange[1], frange[2], delta_f)
    } else {
        if (!is.null(logfsteps)) {
            interp_freqs <- pracma::logspace(
                log10(frange[1]),
                log10(frange[2]),
                logfsteps
            )
        } else {
            interp_freqs <- freqs
        }
    }

    if (!is.null(delta_f) | !is.null(delta_t) | !is.null(logfsteps)) {
        if (return_complex) {
            interp_amp <- interp2d(
                x = times,
                y = freqs,
                z = abs(q_plane),
                xout = interp_times,
                yout = interp_freqs
            )
            interp_phase <- interp2d(
                x = times,
                y = freqs,
                z = Arg(q_plane),
                xout = interp_times,
                yout = interp_freqs
            )
            q_plane <- exp(1.0i * interp_phase) * interp_amp
        } else {
            #
            interp <- interp2d(
                x = times,
                y = freqs,
                z = q_plane,
                xout = interp_times,
                yout = interp_freqs
            )
            q_plane <- interp
        }
    } else {
        q_plane <- qpl.res
    }
    names(q_plane) <- c("times", "freqs", "q_plane")
    return(q_plane)
}

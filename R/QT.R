#' Iterate over the Q values
#'
#' Compute Q values to use for Q-tiling, given a range and mismatch tolerance.
#'
#' @param qrange A numeric vector of length 2. Lower and upper bounds of Q range.
#' @param deltam A numeric. Fractional mismatch between neighboring Q tiles.
#'
#' @return A numeric vector of Q values for Q-planes.
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

#' Convert mismatch to fractional spacing
#'
#' Compute the fractional spacing (delta m) between neighboring tiles from mismatch.
#'
#' @param mismatch A numeric. Desired fractional mismatch (e.g., 0.2).
#'
#' @return A numeric. Fractional spacing.
#' @export
deltam_f <- function(mismatch) {
    2 * (mismatch / 3.)^(1 / 2.)
}

#' Iterate over tile center frequencies for a given Q
#'
#' Determine the central frequencies at which to place tiles, for a fixed Q.
#'
#' @param q A numeric. The Q value.
#' @param frange A numeric vector of length 2. Frequency range (Hz).
#' @param mismatch A numeric. Desired mismatch (e.g., 0.2).
#' @param duration A numeric. Duration of the input time series in seconds.
#'
#' @return A numeric vector of center frequencies (Hz).
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

#' Generate Q-tiling scheme
#'
#' Generate Q-tiles (Q, f0 pairs) for all Q-planes across a frequency range.
#'
#' @param fseries An \code{fs} object. Frequency-series representation of the data.
#' @param qrange A numeric vector. Range of Q values.
#' @param frange A numeric vector. Frequency range (Hz).
#' @param mismatch A numeric. Mismatch tolerance (default: 0.2).
#'
#' @return A list of Q-planes, each containing a Q value and associated frequency tiles.
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

#' Perform Q-transform on a single tile
#'
#' Compute Q-transform for a single Q and center frequency tile.
#'
#' @param fseries An \code{fs} object. Frequency-series representation of the data.
#' @param Q A numeric. Q value of the tile.
#' @param f0 A numeric. Center frequency of the tile.
#' @param return_complex Logical (default: FALSE). If \code{TRUE}, return complex time series instead of normalized energy.
#'
#' @return A \code{ts} object representing either normalized energy or complex waveform.
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
    windowed <- fs(windowed, df = deltaf(fseries), sampling.freq = attr(fseries, "sampling.freq"))
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

#' Compute Q-plane across all Q and f0 tiles
#'
#' Evaluate all Q-tiles and construct the Q-transform energy (or complex) plane.
#'
#' @param qplane.tile.list A list of Q-tiles (from \code{qtiling}).
#' @param fseries An \code{fs} object. Frequency-series representation of the data.
#' @param return_complex Logical. If \code{TRUE}, return complex-valued transform.
#'
#' @return A list with:
#' \describe{
#'   \item{times}{Time axis (s)}
#'   \item{freqs}{Frequency axis (Hz)}
#'   \item{plane}{Q-transform output (matrix)}
#' }
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

#' 2D Interpolation for Surface Data
#'
#' Interpolate a 2D surface (z-values) across a new grid using bilinear or spline methods.
#'
#' @param x A numeric vector. Original x-axis grid.
#' @param y A numeric vector. Original y-axis grid.
#' @param z A numeric matrix. Original z-values (dim: \code{length(x)} × \code{length(y)}).
#' @param xout A numeric vector. Output x grid.
#' @param yout A numeric vector. Output y grid.
#' @param method A character. Interpolation method: \code{"linear"} (default) or \code{"spline"}.
#'
#' @return A list with \code{x}, \code{y}, and interpolated \code{z} matrix.
#' @export
interp2d <- function(x, y, z, xout, yout, method = "linear") {
    interp.surface <- function(x, y, z, loc, method = "linear") {
        # Inspired by fields::interp.surface
        nx <- length(x)
        ny <- length(y)
        ll <- switch(method,
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

#' Q-transform of time series
#'
#' Perform a Q-transform over a time series and interpolate to desired resolution.
#'
#' @details
#' This is a ported implementation based on the original
#' \code{pycbc.filter.qtransform} function from the PyCBC library.
#'
#' @references
#' PyCBC source:
#' \url{https://pycbc.org/pycbc/latest/html/_modules/pycbc/filter/qtransform.html}
#'
#' @param ts A \code{ts} object. Input time series.
#' @param delta_t A numeric. Time resolution (optional).
#' @param delta_f A numeric. Frequency resolution (optional, mutually exclusive with \code{logfsteps}).
#' @param logfsteps A numeric. Number of log-spaced frequency bins (mutually exclusive with \code{delta_f}).
#' @param frange A numeric vector. Frequency range (Hz). Default: \code{c(30, Nyquist × 8)}.
#' @param qrange A numeric vector. Q value range (default: \code{c(4, 64)}).
#' @param mismatch A numeric. Mismatch between tiles (default: 0.2).
#' @param return_complex Logical. Whether to return complex data instead of power (default: FALSE).
#'
#' @return A list with:
#' \describe{
#'   \item{times}{Time axis (s)}
#'   \item{freqs}{Frequency axis (Hz)}
#'   \item{q_plane}{2D matrix of interpolated Q-transform (power or complex)}
#' }
#' @export
qtransform <- function(
    ts,
    delta_t = NULL,
    delta_f = NULL,
    logfsteps = NULL,
    frange = NULL,
    qrange = c(4, 64),
    mismatch = 0.2,
    return_complex = FALSE) {
    if (is.null(frange)) {
        frange <- c(30, as.integer(frequency(ts) / 2 * 8))
    }

    fseries <- to_fs(ts)
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

#' Create a Frequency Series Object (`fs`)
#'
#' Wraps a numeric vector into an `fs` class, assigning appropriate attributes
#' for frequency-domain analysis.
#'
#' @param x A numeric vector representing frequency-domain data.
#' @param df A numeric scalar. Frequency resolution (`delta_f`).
#' @param sampling.freq A numeric. Sampling frequency.
#'
#' @return An object of class `fs`, with additional attributes such as `delta_f` and `flen`.
#' @examples
#' fs(c(1, 2, 3), df = 0.5)
#' @export
fs <- function(x, df, sampling.freq) {
    attr(x, "assoc.ts") <- NULL
    attr(x, "ti") <- NULL
    attr(x, "sampling.freq") <- sampling.freq
    attr(x, "delta_f") <- df
    attr(x, "flen") <- length(x)
    attr(x, "frange") <- c(0, sampling.freq / 2)
    structure(x, class = c("fs", "complex"))
}

#' Convert Time Series (`ts`) to Frequency Series (`fs`)
#'
#' Transforms a time series object to a frequency series by computing FFT.
#'

#' @param ts A `ts` object.
#' @param delta_f Optional. A numeric value indicating desired frequency resolution.
#'                If NULL, defaults to `sampling.freq / length(ts)`.
#'
#' @return An `fs` object containing frequency-domain representation and metadata.
#'
#' @export
to_fs <- function(ts, delta_f = NULL) {
    # Check deltaf is given
    if (is.null(delta_f)) {
        delta_f <- frequency(ts) / length(ts)
    }

    # Add 0.5 to round integer
    sampling.freq <- frequency(ts)
    tlen <- floor(1 / delta_f / (1 / sampling.freq) + 0.5)
    flen <- (tlen %/% 2) + 1 # flen <- round(tlen/2 + 1)

    # Check tlen
    if (tlen < length(ts)) {
        stop(
            "ValueError: The value of delta_f (",
            delta_f,
            ") would be ",
            "undersampled. Maximum delta.f ",
            "is ",
            (1 / tl(ts))
        )
    }

    # Prepare temporary data for FFT
    tmp <- ts(rep_len(0, tlen), start = ti(ts), frequency = sampling.freq)
    tmp[1:length(ts)] <- ts
    fft.res <- fftw::FFT(ts, plan = fftw::planFFT(length(ts)))
    fs.out <- fft.res[1:flen]

    # Add attributes
    fs(fs.out, df = delta_f, sampling.freq = sampling.freq)
}

#' Extract Frequency Resolution from `fs` Object
#'
#' Returns the `delta_f` attribute from an `fs` object.
#'
#' @param x An `fs` object.
#'
#' @return A numeric scalar representing frequency resolution.
#' @export
deltaf <- function(x) {
    if ("fs" %in% class(x)) {
        attr(x, "delta_f")
    } else {
        stop("TypeError: 'delta_f' is available on 'fs' class.")
    }
}

#' Custom Print Method for `fs` Class
#'
#' Prints summary information for an `fs` object, including delta_f, flen,
#' and associated time-domain metadata if available.
#'
#' @param x An `fs` object.
#' @param ... Additional arguments (ignored).
#' @param ts.info Logical. Whether to show associated time-series info (default: TRUE).
#'
#' @return None. Used for side effects.
#' @export
print.fs <- function(x, ..., ts.info = TRUE) {
    # Title of printing
    cat("Frequency Series:\n")
    cat("├─ delta_f = ", attr(x, "delta_f"), "\n", sep = "")
    # If ts.info=TRUE, print "Associated ts info", too.
    if (
        !ts.info |
            (is.null(attr(x, "assoc.ts")) &
                is.null(attr(x, "ti")) &
                is.null(attr(x, "sampling.freq")))
    ) {
        cat("└─ flen    = ", attr(x, "flen"), "\n", sep = "")
    } else {
        cat("├─ flen    = ", attr(x, "flen"), "\n", sep = "")
        cat("└─ Associated ts info:\n")
        # Associated ts object name
        cat('   ├─ ts name       = "', attr(x, "assoc.ts"), '"\n', sep = "")
        # Associated ts object start time
        cat("   ├─ Start time    = ", attr(x, "ti"), "\n", sep = "")
        # Associated ts object sampling frequency
        cat("   └─ sampling.freq = ", attr(x, "sampling.freq"), "\n", sep = "")
    }

    # Default printing
    attributes(x) <- NULL
    print.default(x, digits = 4)
}


#' Custom Structure Summary for `fs` Object
#'
#' Displays a concise structural summary of an `fs` object, highlighting class and delta_f.
#'
#' @param x An `fs` object.
#'
#' @return None. Prints structure to console.
#' @export
str.fs <- function(x) {
    class.cat <- "Frequency-Series"
    df.cat <- paste0(
        "df=",
        format(signif(attr(x, "delta_f"), 3), scientific = T)
    )
    str.default <- capture.output(str(`class<-`(x, "complex")))

    cat(paste0(class.cat, " (", df.cat, ")", str.default[1], "\n"))
}

#' Plot a Frequency Series (`fs`)
#'
#' Visualizes the amplitude (or PSD) of an `fs` object using ggplot2.
#'
#' @param fs An `fs` object.
#' @param log A character ("x", "y", or "xy") to control log scaling.
#' @param xlab A character. X-axis label (default: "Frequency (Hz)").
#' @param ylab A character. Y-axis label (default: "PSD").
#' @param xlim A numeric vector of length 2 specifying x-axis limits.
#' @param ylim A numeric vector of length 2 specifying y-axis limits.
#'
#' @return A ggplot object.
#'
#' @export
plot.fs <- function(
    fs,
    log = "xy",
    xlab = "Frequency (Hz)",
    ylab = "PSD",
    xlim = NULL,
    ylim = NULL) {
    base_breaks <- function(n = 10) {
        function(x) {
            axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
        }
    }

    fs_df <- data.frame("freqs" = freqs(fs), "PSD" = abs(fs))

    # plot
    pl <- ggplot2::ggplot(fs_df, ggplot2::aes(x = freqs, y = PSD)) +
        ggplot2::geom_line()

    # log axis vs lin axis
    if ("x" %in% strsplit(log, split = "")[[1]]) {
        pl <- pl +
            ggplot2::scale_x_log10(breaks = base_breaks(), labels = prettyNum)
    } else {
        pl <- pl + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())
    }

    if ("y" %in% strsplit(log, split = "")[[1]]) {
        pl <- pl +
            ggplot2::scale_y_log10(
                breaks = scales::breaks_log(),
                labels = prettyNum
            )
    } else {
        pl <- pl + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks())
    }

    # coord_cartesian
    pl <- pl + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)

    # labels
    pl <- pl + ggplot2::labs(x = xlab, y = ylab)

    # theme
    pl + ggplot2::theme_bw(base_size = 15) + ggplot2::annotation_logticks()
}

#' Get Frequency Samples from `fs`
#'
#' Computes the discrete frequency values corresponding to the bins of an `fs` object.
#'
#' @param fs An `fs` object.
#'
#' @return A numeric vector of frequency values (in Hz).
#' @export
freqs <- function(fs) {
    delta_f <- attr(fs, "delta_f")
    flen <- attr(fs, "flen")
    frange <- attr(fs, "frange")
    # seq(0, flen - 1) * delta_f
    seq(frange[1], frange[2], by = delta_f)
}

#' Get Duration of the Original Time Series
#'
#' Computes the total duration in seconds of the time-domain signal that
#' generated the `fs` object.
#'
#' @param fs An `fs` object derived from a `ts` object.
#'
#' @return A numeric scalar. Duration in seconds.
#' @export
dur <- function(fs) {
    if (is.null(attr(fs, "tlen")) | is.null(attr(fs, "sampling.freq"))) {
        stop("This fs is not coming from ts")
    }
    attr(fs, "tlen") / attr(fs, "sampling.freq")
}

#' Convert `fs` to Data Frame
#'
#' Converts an `fs` object to a data frame with columns `freqs` and `PSD`.
#'
#' @param fs An `fs` object.
#'
#' @return A data frame with two columns: `freqs` and `PSD`.
#' @export
fs_df <- function(fs) {
    data.frame("freqs" = freqs(fs), "PSD" = fs)
}

#' Crop frequency series to a given range
#'
#' @param fs A `fs` object (frequency series).
#' @param ref A vector with elements: `c(min, max)`, indicating index range.
#' @return A cropped `fs` object with preserved attributes.
#'
#' @export
cutoff_to <- function(fs, ref, frange = NULL) {
    if (!inherits(fs, "fs")) {
        stop("TypeError: Input must be a fs class")
    }
    if (!is.null(frange)) {
        kmin <- which.min(abs(freqs(fs) - frange[1]))
        kmax <- which.min(abs(freqs(fs) - frange[2]))
    } else {
        kmin <- ref[1]
        kmax <- ref[2]
        frange <- freqs(fs)[ref]
    }

    out <- fs[kmin:kmax]
    out <- copy_attr(
        out,
        ref = fs,
        which = c("sampling.freq", "delta_f", "class", "ti")
    )
    attr(out, "flen") <- kmax - kmin + 1
    attr(out, "frange") <- frange
    out
}

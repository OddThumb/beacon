# FrequencySeries ----

#' fs: FrequencySeries class
#'
#' @param x  A numeric vector.
#' @param df A numeric. A delta frequency.
#'
#' @examples
#' # fs(c(1,2,3), df=0.5)
#'
#' @export
fs <- function(x, df) {
    attr(x, "assoc.ts") <- NULL
    attr(x, "ti") <- NULL
    attr(x, "sampling.freq") <- NULL
    attr(x, "delta_f") <- df
    attr(x, "flen") <- length(x)
    structure(x, class = c("fs", "complex"))
}

#' Transform `ts` class to `fs` class
#'
#' @param ts     A `ts` object to be transformed to `fs`.
#' @param deltaf A numeric. A delta frequency.
#' @return A `fs` object with respect to the given `ts`.
#' @export
to_fs <- function(ts, delta_f = NULL) {
    # Check deltaf is given
    if (is.null(delta_f)) {
        delta_f <- frequency(ts) / length(ts)
    }

    # Add 0.5 to round integer
    sampling.freq <- frequency(ts)
    tlen <- floor(1 / delta_f / (1 / sampling.freq) + 0.5)
    flen <- (tlen %/% 2) + 1 #flen <- round(tlen/2 + 1)

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
    attr(fs.out, "assoc.ts") <- deparse(substitute(ts))
    attr(fs.out, "ti") <- ti(ts)
    attr(fs.out, "sampling.freq") <- sampling.freq
    attr(fs.out, "delta_f") <- delta_f
    attr(fs.out, "flen") <- flen
    attr(fs.out, "tlen") <- tlen
    structure(fs.out, class = c("fs", "complex"))
}

#' Extract delta_f of fs
#'
#' @export
deltaf <- function(x) {
    if ("fs" %in% class(x)) {
        attr(x, "delta_f")
    } else {
        stop("TypeError: 'delta_f' is available on 'fs' class.")
    }
}

#' Printing fs class differently
#'
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


#' Structure function for fs class
#'
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

#' Plot function for fs class
#'
#' @export
plot.fs <- function(
    fs,
    log = "xy",
    xlab = "Frequency (Hz)",
    ylab = "PSD",
    xlim = NULL,
    ylim = NULL
) {
    base_breaks <- function(n = 10) {
        function(x) {
            axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
        }
    }

    fs_df <- data.frame('freqs' = freqs(fs), 'PSD' = abs(fs))
    devtools
    # plot
    pl <- ggplot2::ggplot(fs_df, ggplot2::aes(x = freqs, y = PSD)) +
        ggplot2::geom_line()

    # log axis vs lin axis
    if ("x" %in% strsplit(log, split = '')[[1]]) {
        pl <- pl +
            ggplot2::scale_x_log10(breaks = base_breaks(), labels = prettyNum)
    } else {
        pl <- pl + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())
    }

    if ("y" %in% strsplit(log, split = '')[[1]]) {
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

#' Get frequency samples
#'
#' @export
freqs <- function(fs) {
    delta_f <- attr(fs, "delta_f")
    flen <- attr(fs, "flen")
    seq(0, flen - 1) * delta_f
}

#' Get duration
#'
#' @export
dur <- function(fs) {
    if (is.null(attr(fs, "tlen")) | is.null(attr(fs, "sampling.freq"))) {
        stop("This fs is not coming from ts")
    }
    attr(fs, "tlen") / attr(fs, "sampling.freq")
}

#' Transform to the data frame
#'
#' @export
fs_df <- function(fs) {
    data.frame("freqs" = freqs(fs), "PSD" = fs)
}

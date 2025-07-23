#' Plot Auto Correlation Function (ACF)
#'
#' Compute and visualize the autocorrelation function of a time series.
#'
#' @param ts A numeric vector or time-series object.
#' @param lag.max An integer specifying the maximum lag to compute ACF. Default is the length of `ts`.
#' @param plot A logical. If TRUE (default), return a ggplot2 object for visualization.
#' @param title An optional string to set as plot title.
#'
#' @return If `plot = TRUE`, a list with:
#' \describe{
#'   \item{acf}{An object of class "acf" with added element `white95ci` (significance threshold).}
#'   \item{plot}{A ggplot2 object showing the autocorrelation function.}
#' }
#' Otherwise, returns the "acf" object only.
#'
#' @export
ACF <- function(ts, lag.max = length(ts), plot = TRUE, title = NULL) {
    ret <- stats::acf(x = ts, lag.max = lag.max, type = 'correlation', plot = F)
    sigs <- 1.96 / sqrt(length(ts))
    ret[['white95ci']] <- sigs

    ret.df <- data.frame('lag' = c(ret$lag), 'acf' = c(ret$acf))
    if (plot) {
        p <- ggplot2::ggplot(ret.df, ggplot2::aes(x = lag, y = acf)) +
            ggplot2::geom_segment(
                ggplot2::aes(xend = lag, yend = 0),
                alpha = 0.6
            ) +
            ggplot2::geom_point(size = 1, alpha = 0.5) +
            ggplot2::geom_hline(
                ggplot2::aes(
                    linetype = '95% C.I. for white noise',
                    yintercept = sigs
                ),
                color = 'blue'
            ) +
            ggplot2::geom_hline(
                ggplot2::aes(
                    linetype = '95% C.I. for white noise',
                    yintercept = -sigs
                ),
                color = 'blue'
            ) +
            ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
            ggplot2::scale_linetype_manual(
                name = NULL,
                values = 'dashed',
                guide = ggplot2::guide_legend(
                    override.aes = list(color = c("blue"))
                )
            ) +
            ggplot2::labs(x = 'lag (s)', y = 'ACF', title = title) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.justification = c(1, 1),
                legend.position = c(1, 1),
                legend.background = ggplot2::element_rect(
                    fill = ggplot2::alpha('white', 0)
                )
            )

        return(list('acf' = ret, 'plot' = p))
    } else {
        return(ret)
    }
}

#' Plot Partial Auto Correlation Function (PACF)
#'
#' Compute and visualize the partial autocorrelation function of a time series.
#'

#' @param ts A numeric vector or time-series object.
#' @param lag.max An integer specifying the maximum lag to compute PACF. Default is the length of `ts`.
#' @param plot A logical. If TRUE (default), return a ggplot2 object for visualization.
#' @param title An optional string to set as plot title.
#'
#' @return If `plot = TRUE`, a list with:
#' \describe{
#'   \item{pacf}{An object of class "acf" with type "partial", including `white95ci`.}
#'   \item{plot}{A ggplot2 object showing the partial autocorrelation function.}
#' }
#' Otherwise, returns the PACF object only.
#'
#' @export
PACF <- function(ts, lag.max = length(ts), plot = TRUE, title = NULL) {
    ret <- stats::acf(x = ts, lag.max = lag.max, type = 'partial', plot = F)
    sigs <- 1.96 / sqrt(length(ts))
    ret[['white95ci']] <- sigs

    ret.df <- data.frame('lag' = c(ret$lag), 'acf' = c(ret$acf))

    if (plot) {
        p <- ggplot2::ggplot(ret.df, aes(x = lag, y = acf)) +
            ggplot2::geom_segment(
                ggplot2::aes(xend = lag, yend = 0),
                alpha = 0.6
            ) +
            ggplot2::geom_point(size = 1, alpha = 0.5) +
            ggplot2::geom_hline(
                ggplot2::aes(
                    linetype = '95% C.I. for white noise',
                    yintercept = sigs
                ),
                color = 'blue'
            ) +
            ggplot2::geom_hline(
                ggplot2::aes(
                    linetype = '95% C.I. for white noise',
                    yintercept = -sigs
                ),
                color = 'blue'
            ) +
            ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
            ggplot2::scale_linetype_manual(
                name = NULL,
                values = 'dashed',
                guide = ggplot2::guide_legend(
                    override.aes = list(color = c("blue"))
                )
            ) +
            ggplot2::labs(x = 'lag (s)', y = 'PACF', title = title) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.justification = c(1, 1),
                legend.position = c(1, 1),
                legend.background = ggplot2::element_rect(
                    fill = ggplot2::alpha('white', 0)
                )
            )

        return(list('pacf' = ret, 'plot' = p))
    } else {
        return(ret)
    }
}

#' Plot Cross Correlation Function (CCF)
#'
#' Compute and optionally visualize the cross-correlation function between two time series.
#'
#' @param ts1 First numeric vector or time-series object.
#' @param ts2 Second numeric vector or time-series object.
#' @param lag.max Maximum lag to compute. Default is length of `ts1`.
#' @param na.action NA handling function. Default is `na.fail`.
#' @param title Optional plot title.
#' @param plot Logical. If TRUE (default), return a ggplot2 object.
#' @param save Logical. If TRUE, save plot as 'CCF.pdf'. Default is FALSE.
#' @param dir Optional directory to save plot (currently ignored unless customized in `savefig()`).
#' @param prefix Optional prefix for filename (currently ignored unless used in `savefig()`).
#' @param times Optional numeric vector used to convert inputs into ts objects if not already.
#'
#' @return A list containing:
#' \describe{
#'   \item{ccf_res}{The CCF computation result with added `white95ci`.}
#'   \item{ind_max}{Index of lag with highest absolute cross-correlation.}
#'   \item{ccf_max}{Value of maximum absolute cross-correlation.}
#'   \item{lag_max}{Lag at which maximum absolute cross-correlation occurs.}
#'   \item{plot}{ggplot2 object, only if `plot = TRUE`.}
#' }
#'
#' @export
CCF <- function(
    ts1,
    ts2,
    lag.max = length(ts1),
    na.action = na.fail,
    title = NULL,
    plot = TRUE,
    save = FALSE,
    dir = NULL,
    prefix = NULL,
    times = NULL
) {
    if (!is.ts(ts1) | !is.ts(ts2)) {
        if (is.null(times)) {
            stop("To convert given data to ts, 'times' needs to be provided")
        }
        ts1 <- ts(ts1, start = times[1], frequency = frequency(times))
        ts2 <- ts(ts2, start = times[1], frequency = frequency(times))
    }
    sampling.freq <- frequency(ts1)

    ret <- stats::ccf(
        ts1,
        ts2,
        lag.max = lag.max,
        plot = FALSE,
        na.action = na.action
    )
    ind_max <- which.max(abs(ret$acf))
    ccf_max <- ret$acf[ind_max]
    lag_max <- ret$lag[ind_max]
    sigs <- 1.96 / sqrt(length(ts1))
    ret[['white95ci']] <- sigs

    ret.df <- data.frame('lag' = c(ret$lag), 'acf' = c(ret$acf))

    if (plot) {
        plot <- ggplot2::ggplot(ret.df, aes(x = lag, y = acf)) +
            ggplot2::geom_line(show.legend = F) +
            ggplot2::geom_hline(
                ggplot2::aes(
                    linetype = '95% C.I. for white noise',
                    yintercept = sigs
                ),
                color = 'blue'
            ) +
            ggplot2::geom_hline(
                aes(linetype = '95% C.I. for white noise', yintercept = -sigs),
                color = 'blue'
            ) +
            ggplot2::cale_x_continuous(
                breaks = scales::breaks_pretty(ifelse(
                    nrow(ret.df > 10 * sampling.freq),
                    10,
                    trunc(length(ts) / (sampling.freq / 2))
                ))
            ) +
            ggplot2::scale_linetype_manual(
                name = NULL,
                values = 'dashed',
                guide = ggplot2::guide_legend(
                    override.aes = list(color = c("blue"))
                )
            ) +
            ggplot2::labs(x = 'lag (s)', y = 'CCF', title = title) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                legend.justification = c(1, 1),
                legend.position = c(1, 1),
                legend.background = ggplot2::element_rect(
                    fill = ggplot2::alpha('white', 0)
                )
            )

        if (save) savefig(plot, 'CCF.pdf')
    }

    return(list(
        'ccf_res' = ret,
        'ind_max' = ind_max,
        'ccf_max' = ccf_max,
        'lag_max' = lag_max,
        'plot' = plot
    ))
}

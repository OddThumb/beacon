# Messaging ----

#' Save a Message to a Text File
#'
#' Append a message (or multiple messages) to a text file.
#'
#' @param msg A character string representing the message to save.
#' @param ... Additional character strings to be saved.
#' @param file A character string specifying the file path.
#' @param show Logical; if \code{TRUE}, print the message on console.
#'
#' @return None.
#' @export
savemsg <- function(msg, ..., file, show = T) {
    write.table(
        rbind(msg, ...),
        file = paste_wd(file),
        quote = F,
        row.names = F,
        col.names = F,
        append = T
    )
    if (show) {
        message(cat(paste(msg, ..., sep = '\n')))
    }
}

#' Print Message Conditionally by Iteration
#'
#' Print a message every \code{verbose}-th iteration in a loop.
#'
#' @param i Integer indicating current iteration index.
#' @param verbose Logical or integer; if \code{TRUE}, print every iteration; if integer, print every \code{verbose}-th iteration.
#' @param msg A character string to be printed.
#'
#' @return None.
#' @export
message_by <- function(i = i, verbose = T, msg) {
    if (verbose) {
        verbby <- 1
        if (i %% verbby == 0) {
            message(msg)
        }
    } else if (is.numeric(verbose)) {
        verbby <- verbose
        if (i %% verbby == 0) {
            message(msg)
        }
    }
}

#' Conditionally Print Message
#'
#' Print message only when \code{v = TRUE}.
#'
#' @param ... Character strings to print.
#' @param v Logical; whether to print the message.
#'
#' @return None.
#' @export
message_verb <- function(..., v = TRUE) {
    if (v) {
        message(...)
    } else {
        NULL
    }
}

#' Print Contents of a File to Console
#'
#' Print the contents of a file using system \code{cat}.
#'
#' @param file A character string specifying the file path.
#'
#' @return None.
#' @export
sys_cat <- function(file) {
    system(paste("cat", paste_wd(file)))
}

#' Print Formatted String Using cat and sprintf
#'
#' Print a string by combining \code{sprintf()} and \code{cat()}.
#'
#' @param ... Arguments passed to \code{sprintf}.
#' @param sep A character string to separate values (default: "").
#'
#' @return None.
#' @export
sprint <- function(..., sep = "") {
    cat(sprintf(paste(..., sep = sep)))
}


#' Save a Data Frame as a Markdown-Formatted Table
#'
#' Save a data frame in markdown format using \code{insight::export_table}.
#'
#' @param x A data.frame to save.
#' @param file A character string indicating the file path.
#' @param signif Integer; number of significant digits (default: 3).
#' @param col.names Logical; whether to include column names (default: FALSE).
#' @param row.names Logical; whether to include row names (default: FALSE).
#' @param quote Logical; whether to quote each value (default: FALSE).
#' @param show Logical; if \code{TRUE}, print the table content (default: TRUE).
#' @param append Logical; if \code{TRUE}, append to the file (default: FALSE).
#' @param nsep Integer; number of dashes used to separate appended tables (default: 30).
#' @param ... Additional arguments for \code{insight::export_table}.
#'
#' @return None.
#' @export
savetab <- function(
    x,
    file,
    signif = 3,
    col.names = F,
    row.names = F,
    quote = F,
    show = T,
    append = F,
    nsep = 30,
    ...
) {
    message(' >>> Saving table in MD format...')
    if (append & !is.null(nsep)) {
        write.table(
            paste(rep("-", nsep), collapse = ''),
            file = paste_wd(file),
            quote = F,
            row.names = F,
            col.names = F,
            append = append
        )
    }

    if (row.names) {
        rn <- rownames(x)
        x <- dplyr::relocate(dplyr::mutate(x, `_` = rn), `_`, .before = 1)
    }

    write.table(
        insight::export_table(
            x,
            digits = paste("signif", signif, sep = ''),
            format = 'md',
            ...
        ),
        file = paste_wd(file),
        col.names = col.names,
        row.names = F,
        quote = quote,
        append = append
    )
    if (show) sys_cat(file)
}

#' Append a Line to an Existing File
#'
#' Append content to a file and optionally print it.
#'
#' @param x A character or object to write.
#' @param file A character string specifying the file path.
#' @param show Logical; if \code{TRUE}, print the file content (default: TRUE).
#'
#' @return None.
#' @export
append.line <- function(x, file, show = T) {
    message(' >>> Appending lines in a file...')
    write.table(
        x,
        file = paste_wd(file),
        append = T,
        quote = F,
        row.names = F,
        col.names = F
    )
    if (show) sys_cat(file)
}

# Plotting ----
#' Save a ggplot Object to File
#'
#' Save a ggplot object to a file using \code{ggpubr::ggexport}.
#'
#' @param plot A ggplot object to save.
#' @param file A character string indicating the file path.
#' @param width A numeric value indicating the width of the plot (default: 8).
#' @param height A numeric value indicating the height of the plot (default: 5).
#' @param show Logical; if \code{TRUE}, display and return the plot after saving.
#' @param verbose Logical; if \code{TRUE}, print saving message.
#' @param ... Additional arguments passed to \code{ggpubr::ggexport}.
#'
#' @return A ggplot object if \code{show = TRUE}, otherwise \code{NULL}.
#' @export
savefig <- function(
    plot,
    file,
    width = 8,
    height = 5,
    show = T,
    verbose = F,
    ...
) {
    message_verb(' >>> Saving...', v = verbose)
    ggpubr::ggexport(
        plot,
        filename = paste_wd(file),
        width = width,
        height = height,
        verbose = verbose,
        ...
    )
    if (show) {
        return(plot)
    }
}

#' Plot a Single Oscillogram
#'
#' Generate a line plot of a single time series (oscillogram).
#'
#' @param ts A numeric time series object.
#' @param tzero A numeric value indicating time shift (default: NULL).
#' @param trange A numeric vector specifying time range to display.
#' @param ylim Either 'pm' or a numeric vector of length 2.
#' @param title A character string specifying the plot title.
#' @param size A numeric value for line width (default: 0.3).
#' @param ... Additional arguments for \code{ggplot2::geom_line}.
#'
#' @return A ggplot object.
#' @export
plot_oscillo <- function(
    ts,
    tzero = NULL,
    trange = NULL,
    ylim = 'pm',
    title = NULL,
    size = 0.3,
    ...
) {
    if (is.null(trange)) {
        trange <- c(time(ts)[1], time(ts)[length(ts)])
    }
    ts.df <- data.frame('time' = time(ts), 'strain' = ts)
    ts.df <- dplyr::mutate(
        dplyr::filter(ts.df, time >= trange[1] & time <= trange[2]),
        'time' = time - ifelse(is.null(tzero), 0, tzero)
    )
    value.order <- floor(log10(amax(ts.df$strain)))
    plot <- ggplot2::ggplot() +
        ggplot2::geom_line(
            data = ts.df,
            ggplot2::aes(x = time, y = strain),
            size = size,
            ...
        ) +
        ggplot2::scale_x_continuous(
            expand = c(0, 0),
            breaks = scales::breaks_pretty(10)
        ) +
        ggplot2::scale_y_continuous(
            expand = c(0, 0),
            breaks = scales::breaks_pretty(5),
            labels = scales::label_number(scale = 1 / 10^(value.order))
        ) +
        ggplot2::labs(
            x = ifelse(is.null(tzero), "Time (s)", TeX('Time - $t_0$ (s)')),
            y = latex2exp::TeX(paste('$h~(10^{', value.order, '})$', sep = '')),
            title = title
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    if (is.character(ylim)) {
        if (ylim == 'pm') {
            limiting <- get_limit(ts.df$strain)
            plot <- plot + ggplot2::coord_cartesian(ylim = limiting)
        }
    } else if (is.vector(ylim)) {
        plot <- plot + ggplot2::coord_cartesian(ylim = ylim)
    }

    return(plot)
}

#' Plot Multiple Oscillograms in Facet Layout
#'
#' Plot multiple time series with \code{facet_wrap}.
#'
#' @param ts.df A data frame containing time and multiple variables.
#' @param tzero A numeric value indicating time shift (default: NULL).
#' @param trange A numeric vector specifying time range to display.
#' @param facet.scale One of "fixed", "free_y", or "free_x".
#' @param facet.label Character vector for facet labels.
#' @param order.of.mag Logical; whether to rescale y-axis using magnitude order.
#' @param title A character string specifying the plot title.
#' @param color A character for line color.
#' @param width A numeric value for line width (default: 0.3).
#' @param ... Additional arguments for \code{geom_line}.
#'
#' @return A ggplot object.
#' @export
plot_oscillo_multi <- function(
    ts.df,
    tzero = NULL,
    trange = NULL,
    facet.scale = 'fixed',
    facet.label = NULL,
    order.of.mag = TRUE,
    title = NULL,
    color = NULL,
    width = 0.3,
    ...
) {
    if (is.null(ncol(ts.df))) {
        stop("For one ts, use Oscillo()")
    }

    if (is.null(trange)) {
        trange <- c(ts.df$time[1], tail(ts.df$time, 1))
    }

    if (!('time' %in% colnames(ts.df))) {
        ts.df <- dplyr::bind_cols(time = time(ts.df), as.data.frame(ts.df))
    }

    if (!is.null(facet.label)) {
        suppressWarnings({
            colnames(ts.df)[-which(colnames(ts.df) == "time")] <- facet.label
        })
    }

    ts.df <- dplyr::mutate(
        dplyr::filter(ts.df, time >= trange[1] & time <= trange[2]),
        'time' = time - ifelse(is.null(tzero), 0, tzero)
    )
    ts.df.molten <- reshape2::melt(ts.df, id.vars = "time")

    value.order <- floor(log10(amax(ts.df.molten$value)))
    plot <- ggplot2::ggplot(
        ts.df.molten,
        ggplot2::aes(x = time, y = value, color = color)
    ) +
        ggplot2::geom_line(size = width, ...) +
        ggplot2::scale_x_continuous(
            expand = c(0, 0),
            breaks = scales::breaks_pretty(10)
        ) +
        ggplot2::facet_wrap(
            ~variable,
            ncol = 1,
            strip.position = 'right',
            scales = facet.scale
        ) +
        ggplot2::labs(
            x = ifelse(is.null(tzero), "Time (s)", TeX('Time - $t_0$ (s)')),
            y = latex2exp::TeX(paste('$h~(10^{', value.order, '})$', sep = '')),
            title = title
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position = "none",
            panel.spacing = unit(0, "lines")
        )

    if (order.of.mag) {
        plot <- plot +
            ggplot2::scale_y_continuous(
                expand = c(0, 0),
                breaks = scales::breaks_pretty(5),
                labels = scales::label_number(scale = 1 / 10^(value.order))
            )
    } else {
        plot <- plot +
            ggplot2::scale_y_continuous(
                expand = c(0, 0),
                breaks = scales::breaks_pretty(5)
            )
    }
    return(plot)
}

#' Common ggplot Components for Oscillogram Plot
#'
#' Generate a list of ggplot2 components for consistent oscillogram theme.
#'
#' @param ts A time series object.
#' @param tzero A numeric value for time offset.
#' @param title A character string for the plot title.
#'
#' @return A list of ggplot2 elements.
#' @export
oscillo_option <- function(ts, tzero = 0, title = NULL) {
    value.order <- floor(log10(amax(ts)))
    list(
        ggplot2::scale_x_continuous(
            expand = c(0, 0),
            breaks = scales::breaks_pretty(5)
        ),
        ggplot2::scale_y_continuous(
            expand = c(0, 0),
            breaks = scales::breaks_pretty(5),
            labels = scales::label_number(scale = 1 / get_order(ts)),
            limits = get_limit(ts, 1.5)
        ),
        ggplot2::labs(
            x = ifelse(tzero != 0, paste0('Time (s) from ', tzero), 'Time (s)'),
            y = latex2exp::TeX(paste(
                '$\\textit{h}~(10^{',
                value.order,
                '})$',
                sep = ''
            )),
            title = title
        ),
        ggplot2::theme_bw(),
        ggplot2::theme(legend.position = 'none')
    )
}

#' Plot Q-Transform Spectrogram with Oscillogram
#'
#' Create a Q-transform spectrogram stacked with its oscillogram using ggplot2.
#'
#' @param ts A time series object.
#' @param tzero Time offset (default: 0).
#' @param trange Time range for cropping the data.
#' @param frange Frequency range for spectrogram.
#' @param qrange Q-factor range for Q-transform.
#' @param crange Color range (z-axis) for spectrogram.
#' @param tres Time resolution.
#' @param fres Frequency resolution.
#' @param logf Logical; log-scale y-axis (default: TRUE).
#' @param title Title of the plot.
#' @param specScale Show colorbar for spectrogram.
#' @param specXlabel, specYlabel Show axis labels for spectrogram.
#' @param specGrid Show grid lines ("x", "y", "xy").
#' @param specColorPal Color palette.
#' @param trans Transformation function applied to spectrogram values.
#' @param specScaleDir Direction of colorbar.
#' @param specScalePos Position of colorbar ("ul", "ur", "bl", "br").
#' @param osciLegend, osciXlabel, osciYlabel Options for oscillogram.
#' @param stack Logical; stack spectrogram and oscillogram (default: TRUE).
#' @param ... Additional arguments passed to \code{ggarrange}.
#'
#' @return A ggplot object, or a list with \code{spec.plot} and \code{osci.plot} if \code{stack = FALSE}.
#' @export
plot_spectro <- function(
    ts,
    tzero = 0,
    trange = NULL,
    frange = c(32, 512),
    qrange = c(40, 1),
    crange = NULL,
    tres = 1000,
    fres = 1000,
    logf = T,
    title = NULL,
    specScale = FALSE,
    specXlabel = FALSE,
    specYlabel = TRUE,
    specGrid = "none",
    specColorPal = viridis::viridis(256),
    trans = NULL,
    specScaleDir = "vertical",
    specScalePos = "ul",
    linecolor = 'black',
    osciLegend = FALSE,
    osciXlabel = TRUE,
    osciYlabel = TRUE,
    stack = T,
    ...
) {
    # Frequency
    sampling.freq <- frequency(ts)

    # Crop ts with given trange argument
    if (is.null(trange)) {
        trange <- tr(ts)
    }
    ts.crop <- window_to(ts, trange)

    # Resolutions
    delt <- 1 / tres
    if (logf) {
        delf <- NULL
        logfstep <- fres
    } else {
        delf <- 1 / fres
        logfstep <- NULL
    }

    # Q-transform
    qspecdata <- qtransform(
        ts = ts.crop,
        delta_t = delt,
        delta_f = delf,
        logfsteps = logfstep,
        frange = frange,
        qrange = qrange,
        mismatch = 0.2,
        return_complex = F
    )
    names(qspecdata) <- c("t", "f", "S")

    # Applying transform function on z-axis
    if (!is.null(trans)) {
        qspecdata[['S']] <- trans(qspecdata[['S']])
    }

    # Limit on color (z-axis)
    if (!is.null(crange)) {
        qspecdata[['S']][qspecdata[['S']] <= crange[1]] <- crange[1]
        qspecdata[['S']][qspecdata[['S']] >= crange[2]] <- crange[2]
    }

    # Convert qspecdata (list) to melted qspecdata.df (data.frame)
    qspecdata.df <- data.frame(
        'f' = rep(qspecdata$f, times = length(qspecdata$t)),
        't' = rep(qspecdata$t, each = length(qspecdata$f)),
        'S' = c(qspecdata$S)
    )

    # Filter qspecdata.df with time range and shift with tzero
    qspecdata.df <- qspecdata.df |>
        dplyr::filter(dplyr::between(t, trange[1], trange[2])) |>
        dplyr::mutate('t' = t - tzero)

    # Save memory
    qspecdata.df <- qspecdata.df |>
        dplyr::mutate(dplyr::across(t:S, ~ round(.x, digits = 4)))

    # Spectrogram by GGPLOT2
    spec.plot <- ggplot2::ggplot() +
        ggplot2::geom_raster(
            data = qspecdata.df,
            ggplot2::aes(x = t, y = f, fill = S),
            interpolate = T,
            ...
        ) +
        ggplot2::scale_x_continuous(
            expand = c(0, 0),
            breaks = scales::breaks_pretty(10)
        ) +
        ggplot2::scale_y_continuous(
            expand = c(0, 0),
            breaks = ifelse(
                logf,
                scales::log_breaks(7),
                scales::breaks_pretty(7)
            ),
            transform = ifelse(logf, "log10", "identity")
        ) +
        ggplot2::scale_fill_gradientn(colors = specColorPal) +
        ggplot2::labs(
            x = TeX('Time - $t_0$ (s)'),
            y = 'Frequency (Hz)',
            fill = 'Normalized Power',
            title = NULL
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position = "none",
            plot.margin = ggplot2::margin(1, 1, 0, 1, "lines")
        )

    if (stringr::str_detect(specGrid, "x")) {
        spec.plot <- spec.plot +
            ggplot2::geom_vline(
                xintercept = na.omit(
                    ggplot2::ggplot_build(spec.plot)$layout$panel_params[[
                        1
                    ]]$x$breaks
                ),
                color = "grey92",
                alpha = 0.3,
                linewidth = 0.1
            )
    }

    if (stringr::str_detect(specGrid, "y")) {
        spec.plot <- spec.plot +
            ggplot2::geom_hline(
                yintercept = ifelse(
                    logf,
                    10^na.omit(
                        ggplot2::ggplot_build(spec.plot)$layout$panel_params[[
                            1
                        ]]$y$breaks
                    ),
                    na.omit(
                        ggplot2::ggplot_build(spec.plot)$layout$panel_params[[
                            1
                        ]]$y$breaks
                    )
                ),
                color = "grey92",
                alpha = 0.3,
                linewidth = 0.1
            )
    }

    if (!specXlabel) {
        spec.plot <- spec.plot +
            ggplot2::theme(
                axis.title.x = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )
    }
    if (!specYlabel) {
        spec.plot <- spec.plot +
            ggplot2::theme(
                axis.title.y = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank()
            )
    }

    # Color bar for spectrogram
    if (specScale) {
        if (specScaleDir == "vertical") {
            lgndttlpos <- "left"
            lgndttlang <- 90
        } else if (specScaleDir == "horizontal") {
            lgndttlpos <- "top"
            lgndttlang <- 0
        } else {
            warning(
                '`legend.dir` is not correct ("vertical" or "horizontal"). Assume legend.dir="horizontal"'
            )
            specScaleDir <- "horizontal"
            lgndttlpos <- "top"
            lgndttlang <- 0
        }

        lgndpos <- switch(
            specScalePos,
            "ul" = c(0, 1),
            "ur" = c(1, 1),
            "bl" = c(0, 0),
            "br" = c(1, 0)
        )
        lgndjus <- switch(
            specScalePos,
            "ul" = c(0, 1),
            "ur" = c(1, 1),
            "bl" = c(0, 0),
            "br" = c(1, 0)
        )
        spec.plot <- spec.plot +
            ggplot2::guides(
                size = "none",
                fill = ggplot2::guide_colourbar(title.position = lgndttlpos),
                color = FALSE
            ) +
            ggplot2::theme(
                legend.position = lgndpos,
                legend.justification = lgndjus,
                legend.direction = specScaleDir,
                legend.title = ggplot2::element_text(
                    size = 9,
                    angle = lgndttlang
                ),
                legend.title.align = 0.5,
                legend.text = ggplot2::element_text(size = 8),
                legend.background = ggplot2::element_rect(
                    fill = ggplot2::alpha('white', 0.4)
                )
            )
    }

    # Drawing oscillogram by GGPLOT2
    ts_df <- ts_df(ts.crop, tzero)

    limiting <- get_limit(ts_df$x)
    value.order <- floor(log10(amax(ts_df$x)))
    osci.plot <- ggplot2::ggplot() +
        ggplot2::geom_line(
            data = ts_df,
            ggplot2::aes(x = time, y = x),
            linewidth = 0.3,
            color = linecolor
        ) +
        ggplot2::scale_x_continuous(
            expand = c(0, 0),
            breaks = scales::breaks_pretty(10)
        ) +
        ggplot2::scale_y_continuous(
            expand = c(0, 0),
            breaks = scales::breaks_pretty(5),
            labels = scales::label_number(scale = 1 / 10^(value.order)),
            limits = limiting
        ) +
        ggplot2::labs(
            x = TeX('Time - $t_0$ (s)'),
            y = TeX(paste('$h~(10^{', value.order, '})$', sep = '')),
            title = NULL
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position = "none",
            plot.margin = ggplot2::margin(0, 1, 1, 1, "lines")
        )
    if (!osciXlabel) {
        osci.plot <- osci.plot +
            ggplot2::theme(
                axis.title.x = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank()
            )
    }
    if (!osciYlabel) {
        osci.plot <- osci.plot +
            ggplot2::theme(
                axis.title.y = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank()
            )
    }
    if (osciLegend) {
        osci.plot <- osci.plot +
            ggplot2::theme(legend.position = "right") +
            ggplot2::theme_bw()
    }

    # Return a stacked plot or two separated plots
    if (stack) {
        p <- ggpubr::ggarrange(
            spec.plot,
            osci.plot,
            heights = c(0.7, 0.3),
            nrow = 2,
            align = 'v'
        )

        if (!is.null(title)) {
            p <- p |>
                ggpubr::annotate_figure(
                    top = ggpubr::text_grob(title, face = "bold", size = 14)
                )
        }
        return(p)
    } else {
        spec.plot <- spec.plot +
            ggplot2::theme(plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
        osci.plot <- osci.plot +
            ggplot2::theme(plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))
        if (!is.null(title)) {
            spec.plot <- spec.plot |>
                ggpubr::annotate_figure(
                    top = ggpubr::text_grob(title, face = "bold", size = 14)
                )
        }
        if (!is.null(title)) {
            osci.plot <- osci.plot |>
                ggpubr::annotate_figure(
                    top = ggpubr::text_grob(title, face = "bold", size = 14)
                )
        }
        return(list('spec.plot' = spec.plot, 'osci.plot' = osci.plot))
    }
}

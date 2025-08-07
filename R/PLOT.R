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
        message(cat(paste(msg, ..., sep = "\n")))
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

#' Save console output to a file
#'
#' Redirects printed output of an expression to a file using \code{sink()}.
#'
#' @param expr An R expression to evaluate and capture output from.
#' @param file A character string. File path to save the console output.
#'
#' @return None. The console output of \code{expr} is written to \code{file}.
#' @examples
#' \dontrun{
#' sinking(summary(cars), "summary.txt")
#' }
#'
#' @export
sinking <- function(expr, file) {
    sink(file)
    print(expr)
    sink()
}

#' Suppress all output, messages, and warnings
#'
#' Executes an expression while suppressing all output, including messages,
#' warnings, and printed output.
#'
#' @param expr An R expression to evaluate.
#'
#' @return The (invisible) result of the evaluated expression.
#' @examples
#' \dontrun{
#' suppressALL({
#'     warning("This is suppressed")
#'     message("Also suppressed")
#'     print("This too")
#' })
#' }
#' @export
suppressALL <- function(expr) {
    invisible(capture.output(suppressMessages(suppressWarnings(expr))))
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
    ...) {
    message(" >>> Saving table in MD format...")
    if (append & !is.null(nsep)) {
        write.table(
            paste(rep("-", nsep), collapse = ""),
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
            digits = paste("signif", signif, sep = ""),
            format = "md",
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
    message(" >>> Appending lines in a file...")
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
    ...) {
    message_verb(" >>> Saving...", v = verbose)
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
    ylim = "pm",
    title = NULL,
    size = 0.3,
    ...) {
    if (is.null(trange)) {
        trange <- c(time(ts)[1], time(ts)[length(ts)])
    }
    ts.df <- data.frame("time" = time(ts), "strain" = ts)
    ts.df <- dplyr::mutate(
        dplyr::filter(ts.df, time >= trange[1] & time <= trange[2]),
        "time" = time - ifelse(is.null(tzero), 0, tzero)
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
            x = ifelse(
                is.null(tzero),
                "Time (s)",
                paste0("Time (s) from ", tzero)
                # latex2exp::TeX("Time - $t_0$ (s)")
            ),
            y = latex2exp::TeX(paste("$h~(10^{", value.order, "})$", sep = "")),
            title = title
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    if (is.character(ylim)) {
        if (ylim == "pm") {
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
    facet.scale = "fixed",
    facet.label = NULL,
    order.of.mag = TRUE,
    title = NULL,
    color = NULL,
    width = 0.3,
    ...) {
    if (is.null(ncol(ts.df))) {
        stop("For one ts, use Oscillo()")
    }

    if (is.null(trange)) {
        trange <- c(ts.df$time[1], tail(ts.df$time, 1))
    }

    if (!("time" %in% colnames(ts.df))) {
        ts.df <- dplyr::bind_cols(time = time(ts.df), as.data.frame(ts.df))
    }

    if (!is.null(facet.label)) {
        suppressWarnings({
            colnames(ts.df)[-which(colnames(ts.df) == "time")] <- facet.label
        })
    }

    ts.df <- dplyr::mutate(
        dplyr::filter(ts.df, time >= trange[1] & time <= trange[2]),
        "time" = time - ifelse(is.null(tzero), 0, tzero)
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
            strip.position = "right",
            scales = facet.scale
        ) +
        ggplot2::labs(
            x = ifelse(
                is.null(tzero),
                "Time (s)",
                paste0("Time (s) from ", tzero)
                # latex2exp::TeX("Time - $t_0$ (s)")
            ),
            y = latex2exp::TeX(paste("$h~(10^{", value.order, "})$", sep = "")),
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
            x = ifelse(tzero != 0, paste0("Time (s) from ", tzero), "Time (s)"),
            y = latex2exp::TeX(paste(
                "$\\textit{h}~(10^{",
                value.order,
                "})$",
                sep = ""
            )),
            title = title
        ),
        ggplot2::theme_bw(),
        ggplot2::theme(legend.position = "none")
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
    linecolor = "black",
    osciLegend = FALSE,
    osciXlabel = TRUE,
    osciYlabel = TRUE,
    stack = T,
    ...) {
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
        qspecdata[["S"]] <- trans(qspecdata[["S"]])
    }

    # Limit on color (z-axis)
    if (!is.null(crange)) {
        qspecdata[["S"]][qspecdata[["S"]] <= crange[1]] <- crange[1]
        qspecdata[["S"]][qspecdata[["S"]] >= crange[2]] <- crange[2]
    }

    # Convert qspecdata (list) to melted qspecdata.df (data.frame)
    qspecdata.df <- data.frame(
        "f" = rep(qspecdata$f, times = length(qspecdata$t)),
        "t" = rep(qspecdata$t, each = length(qspecdata$f)),
        "S" = c(qspecdata$S)
    )

    # Filter qspecdata.df with time range and shift with tzero
    qspecdata.df <- dplyr::mutate(
        dplyr::filter(qspecdata.df, dplyr::between(t, trange[1], trange[2])),
        "t" = t - tzero
    )

    # Save memory
    qspecdata.df <- dplyr::mutate(
        qspecdata.df,
        dplyr::across(t:S, ~ round(.x, digits = 4))
    )

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
            x = paste0("Time (s) from ", tzero),
            # x = latex2exp::TeX("Time - $t_0$ (s)"),
            y = "Frequency (Hz)",
            fill = "Normalized Power",
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
                alpha = 0.4,
                linewidth = 0.25
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
                alpha = 0.4,
                linewidth = 0.25
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

        lgndpos <- switch(specScalePos,
            "ul" = c(0, 1),
            "ur" = c(1, 1),
            "bl" = c(0, 0),
            "br" = c(1, 0)
        )
        lgndjus <- switch(specScalePos,
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
                    fill = ggplot2::alpha("white", 0.4)
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
            x = paste0("Time (s) from ", tzero),
            # x = latex2exp::TeX("Time - $t_0$ (s)"),
            y = latex2exp::TeX(paste("$h~(10^{", value.order, "})$", sep = "")),
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
            align = "v"
        )

        if (!is.null(title)) {
            p <- ggpubr::annotate_figure(
                p,
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
            spec.plot <- ggpubr::annotate_figure(
                spec.plot,
                top = ggpubr::text_grob(title, face = "bold", size = 14)
            )
        }
        if (!is.null(title)) {
            osci.plot <- ggpubr::annotate_figure(
                osci.plot,
                top = ggpubr::text_grob(title, face = "bold", size = 14)
            )
        }
        return(list("spec.plot" = spec.plot, "osci.plot" = osci.plot))
    }
}

#' Plot anomalies with error bands and significance
#'
#' @param anom.df A data.frame with anomaly results.
#' @param tzero Optional time zero to align the time axis.
#' @param val_col Column name for the observed signal value.
#' @param time_col Column name for the time values (usually "GPS").
#' @param err_lwr Column name for lower error band. If NULL, auto-detect from column names.
#' @param err_upr Column name for upper error band. If NULL, auto-detect from column names.
#' @param p_crit Critical p-value threshold (default: 0.05).
#' @param p_col Column name for p-values.
#'
#' @return A ggplot object showing anomalies and uncertainty bands.
#' @export
plot_anomalies <- function(
    anom.df,
    tzero = NULL,
    val_col = "observed",
    time_col = "GPS",
    err_lwr = NULL,
    err_upr = NULL,
    p_crit = 0.05,
    p_col = "P0") {
    # tzero
    if (is.null(tzero)) {
        tzero <- anom.df[1, time_col, drop = T]
    }

    # ts for auto-theme
    ts.recons <- ts(
        anom.df[, val_col, drop = T],
        start = anom.df[, time_col, drop = T][1L] - tzero,
        deltat = uniqdif(anom.df[, time_col, drop = T])[1L]
    )

    # Error bar
    if (is.null(err_lwr) | is.null(err_upr)) {
        err_lwr <- colnames(anom.df)[grepl("_l1", colnames(anom.df))][1L]
        err_upr <- colnames(anom.df)[grepl("_l2", colnames(anom.df))][1L]
    }
    p <- ggplot2::ggplot(
        anom.df,
        ggplot2::aes(x = .data[[time_col]] - tzero, y = .data[[val_col]])
    ) +
        ggplot2::geom_ribbon(
            ggplot2::aes(ymin = .data[[err_lwr]], ymax = .data[[err_upr]]),
            na.rm = F,
            fill = "grey50",
            alpha = 0.5
        )

    # Line and point
    p <- p + ggplot2::geom_line(color = "grey15")
    if (!is.null(p_crit)) {
        anom.df <- dplyr::mutate(
            dplyr::filter(
                anom.df,
                anomaly == 1
            ),
            lt.p_crit = ifelse(.data[[p_col]] < p_crit, "signif", "lowsig")
        )
        p <- p +
            ggplot2::geom_point(
                data = anom.df,
                ggplot2::aes(color = lt.p_crit),
                shape = 20,
                size = 2,
                alpha = 0.35
            ) +
            ggplot2::geom_point(
                data = anom.df,
                ggplot2::aes(color = lt.p_crit),
                shape = 21,
                size = 3,
                alpha = 0.35
            ) +
            ggplot2::scale_color_manual(
                values = c("signif" = "red", "lowsig" = "grey50")
            )
    } else {
        p <- p +
            ggplot2::geom_point(
                data = dplyr::filter(anom.df, anomaly == 1),
                color = "red",
                shape = 20,
                size = 2,
                alpha = 0.35
            ) +
            ggplot2::geom_point(
                data = dplyr::filter(anom.df, anomaly == 1),
                color = "red",
                shape = 21,
                size = 3,
                alpha = 0.35
            )
    }

    p <- p +
        oscillo_option(ts.recons, tzero) +
        ggplot2::theme(
            legend.direction = "horizontal",
            legend.position.inside = c(1, 1),
            legend.justification = c(1, 1),
            legend.background = ggplot2::element_rect(
                colour = ggplot2::alpha("black", 0.5),
                fill = ggplot2::alpha("white", 0.5)
            )
        )
    return(p)
}

#' Plot anomalies for multiple detectors
#'
#' @param anom.det A data.frame with anomaly results that includes detector info in column "det".
#' @param ... Passed to \code{plot_anomalies()}.
#'
#' @return A faceted ggplot object showing anomaly results per detector.
#' @export
plot_anomalies_multi <- function(anom.det, ...) {
    plot_anomalies(anom.det, ...) +
        ggplot2::labs(y = "strain") +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::geom_text(
            data = dplyr::filter(
                dplyr::filter(
                    dplyr::distinct(
                        dplyr::arrange(anom.det, P0),
                        det,
                        P0,
                        .keep_all = TRUE
                    ),
                    !is.na(P0)
                ),
                P0 < 0.05
            ),
            ggplot2::aes(label = signif(P0, 3L)),
            size = 2.5,
            hjust = -0.2,
            check_overlap = T,
            na.rm = T
        ) +
        ggplot2::facet_wrap(
            facets = ggplot2::vars(det),
            nrow = 4L,
            strip.position = "right",
            scales = "free_y"
        ) +
        ggplot2::theme(
            panel.border = ggplot2::element_rect(linewidth = 0.2),
            strip.background = ggplot2::element_blank(),
            strip.placement = "outside"
        ) +
        ggplot2::coord_cartesian(clip = "off")
}

#' Plot lambda values over time
#'
#' @param res.net A named list of detector results, each including $lamb with lambda statistics.
#' @param lambda One of "a" or "c", indicating \eqn{\lambda_a} or \eqn{\lambda_c} (default: "a").
#' @param chunk_len Chunk duration in seconds for time axis.
#' @param t_from Optional origin time to annotate time axis.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot object showing the update history of lambda values.
#' @export
plot_lambda <- function(
    res.net,
    lambda = c("a", "c"),
    chunk_len = 1,
    t_from = NULL,
    ...) {
    lambda <- match.arg(lambda)
    extract_key <- if (lambda == "a") "a" else "c"
    y_label <- if (lambda == "a") "$\\lambda_a$" else "$\\lambda_c$"
    title_label <- if (lambda == "a") {
        "Update history of $\\textit{\\lambda_a}$"
    } else {
        "Update history of $\\textit{\\lambda_c}$"
    }

    tmp1 <- lapply(res.net, function(x) {
        unlist(lapply(x$lamb, function(lam) lam[[extract_key]]))
    })
    tmp2 <- dplyr::bind_cols(tmp1)
    tmp2$tt <- chunk_len * seq(nrow(tmp2))
    # tmp2$tt <- chunk_len * dplyr::row_number(tmp2)
    tmp2 <- tmp2[, c("tt", setdiff(names(tmp2), "tt"))]
    result <- reshape2::melt(tmp2, id.vars = "tt")

    ggplot2::ggplot(result, ggplot2::aes(x = tt, y = value)) +
        ggplot2::geom_line(ggplot2::aes(
            color = variable,
            linetype = variable
        )) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::scale_color_manual(values = c("H1" = "red", "L1" = "blue")) +
        ggplot2::labs(
            x = if (is.null(t_from)) {
                latex2exp::TeX("$\\textit{t}~(s)$")
            } else {
                latex2exp::TeX(paste0("$\\textit{t}~(s)$ from ", t_from))
            },
            y = latex2exp::TeX(y_label, italic = TRUE),
            color = "Detector",
            linetype = "Detector",
            title = latex2exp::TeX(title_label)
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            panel.grid.minor = ggplot2::element_blank()
        ) +
        legend_inside(legend.direction = "horizontal")
}

#' Plot coincidence significance values over time
#'
#' @param coinc.res A data.frame returned from \code{coincide_P0()}.
#' @param tzero Optional GPS time to align x-axis (default: first timestamp in \code{coinc.res}).
#' @param p_crit Critical p-value to draw significance threshold (default: 0.05).
#' @param a Scaling factor for significance function (default: 3).
#' @param legend.position Position of legend inside plot. One of "tr", "tl", "br", "bl".
#' @param annotate.vals A logical. Annotate values above threshold (default: FALSE).
#'
#' @return A ggplot object with time-series of coincidence significance.
#' @export
plot_coinc <- function(coinc.res,
                       tzero = NULL,
                       p_crit = 0.05,
                       a = 3,
                       legend.position = "tr",
                       annotate.vals = FALSE) {
    # Lazy way...
    P0_names <- c("P0_net", "P0_H1_bin", "P0_L1_bin")
    new_names <- structure(c("coinc", "H1", "L1"), names = P0_names)
    det_colors <- structure(c("black", "red", "blue"), names = P0_names)
    det_alphas <- structure(c(1, 0.3, 0.3), names = P0_names)
    det_ltypes <- structure(c(1, 2, 2), names = P0_names)
    legpos <- switch(legend.position,
        "tr" = list(pos = c(1, 1), jus = c(1, 1)),
        "tl" = list(pos = c(0, 1), jus = c(0, 1)),
        "br" = list(pos = c(1, 0), jus = c(1, 0)),
        "bl" = list(pos = c(0, 0), jus = c(0, 1))
    )

    coinc_melt <- reshape2::melt(
        dplyr::select(coinc.res, "time_bin", dplyr::contains("P0_")),
        id.vars = "time_bin"
    )
    coinc_melt$variable <- factor(coinc_melt$variable, levels = P0_names)
    coinc_melt$value[is.nan(coinc_melt$value)] <- 1

    # Set default tzero (if input tzero is NULL)
    if (is.null(tzero)) {
        tzero <- utc2gps(coinc.res$time_bin[1])
    }

    yval <- Significance(coinc_melt$value, a)
    coinc_melt$y <- yval
    coinc_melt$label <- ifelse(
        yval > Significance(p_crit, a),
        sprintf("%.2f", yval),
        NA_character_
    )

    p <- ggplot2::ggplot(coinc_melt, ggplot2::aes(x = utc2gps(time_bin) - tzero)) +
        ggplot2::geom_line(ggplot2::aes(
            y = y,
            color = variable,
            alpha = variable,
            linetype = variable
        )) +
        ggplot2::geom_point(ggplot2::aes(
            y = y,
            color = variable,
            alpha = variable
        )) +
        ggplot2::geom_hline(
            yintercept = Significance(p_crit, a),
            linetype = "dashed"
        ) +
        ggplot2::labs(
            x = paste0("Time (s) from ", tzero),
            y = expression(italic(S)),
            color = NULL,
            linetype = NULL
        ) +
        ggplot2::scale_color_manual(values = det_colors, labels = new_names) +
        ggplot2::scale_alpha_manual(
            values = det_alphas,
            labels = new_names,
            guide = "none"
        ) +
        ggplot2::scale_linetype_manual(
            values = det_ltypes,
            labels = new_names
        ) +
        ggplot2::theme_bw() +
        legend_inside(
            pos = legpos$pos,
            jus = legpos$jus,
            legend.direction = "horizontal"
        )

    if (annotate.vals) {
        p <- p + ggrepel::geom_text_repel(
            data = subset(coinc_melt, !is.na(label)),
            ggplot2::aes(
                x = utc2gps(time_bin) - tzero,
                y = y,
                label = label,
                color = variable,
                alpha = variable,
            ),
            size = 3,
            # color = "black",
            min.segment.length = 0,
            max.overlaps = Inf,
            show.legend = FALSE
        )
    }

    return(p)
}


#' Theme wrapper for legend inside plot area
#'
#' @param pos Numeric vector of (x, y) position inside plot area (default: c(1, 1)).
#' @param jus Numeric vector for justification (default: c(1, 1)).
#' @param ... Additional theme arguments passed to \code{theme()}.
#'
#' @return A list of ggplot2 theme settings for internal legend placement.
#' @export
legend_inside <- function(pos = c(1, 1), jus = c(1, 1), ...) {
    list(
        theme(
            legend.position = "inside",
            legend.position.inside = pos,
            legend.justification.inside = jus,
            legend.background = element_rect(fill = alpha("white", 0.3)),
            legend.box.background = element_rect(
                fill = alpha("white", 0.3),
                linewidth = 0.5
            ),
            legend.spacing.y = unit(-0.35, "cm"),
            ...
        )
    )
}


#' Compute break points with rounded range
#'
#' @param x A numeric vector of values to compute breaks for.
#' @param bin A numeric scalar indicating the bin width (default: 0.2).
#'
#' @return A numeric vector of break points from minimum to maximum of \code{x}, rounded to fit the bin width.
#' @export
get_break_rngdgt <- function(x, bin = 0.2) {
    rng <- range.width(x, bin)
    seq(rng[1], rng[2], bin)
}

#' Plot on-source detection statistic as discrete points
#'
#' @param S.vec A numeric vector of on-source detection statistic (e.g., \eqn{\mathcal{S}} values).
#' @param color A character string specifying point color (default: "black").
#' @param shape An integer specifying the ggplot2 point shape (default: 24, triangle).
#' @param binwidth A numeric scalar indicating the histogram bin width (default: 0.2).
#'
#' @return A ggplot object showing discrete histogram-like point distribution.
#' @export
plot_onsource <- function(S.vec, color = "black", shape = 24, binwidth = 0.2) {
    his <- hist(S.vec, breaks = get_break_rngdgt(S.vec, binwidth), plot = F)
    df_ons <- dplyr::filter(
        data.frame(x = his$breaks[-1], y = his$counts),
        y != 0
    )
    ggplot2::ggplot() +
        ggplot2::geom_point(
            data = df_ons,
            ggplot2::aes(x = x, y = y),
            shape = shape,
            color = color
        )
}

#' Plot histogram of background detection statistic
#'
#' @param S.bkg.vec A numeric vector of background detection statistic (e.g., \eqn{\mathcal{S}} values).
#' @param color A character string specifying histogram color.
#' @param binwidth Numeric. Width of histogram bins (default: 0.2).
#' @param xlimit Numeric vector of length 2. Limits of x-axis (default: c(7, 30)).
#' @param ylimit Numeric vector of length 2. Limits of y-axis (log10 scale, default: c(5e-3, 5e2)).
#' @param factor Numeric. Rescaling factor for counts (default: 1).
#'
#' @return A list of one ggplot object named \code{histogram}.
#' @export
plot_background <- function(
    S.bkg.vec,
    color = "black",
    binwidth = 0.2,
    xlimit = c(7, 30),
    ylimit = c(5e-3, 5e2),
    factor = 1) {
    # Histogram setup
    rng <- range.width(S.bkg.vec, binwidth)
    his <- hist(S.bkg.vec, breaks = seq(rng[1], rng[2], binwidth), plot = FALSE)

    # Histogram plot only
    p <- ggplot2::ggplot() +
        ggplot2::geom_step(
            data = data.frame(x = his$breaks[-1], y = his$counts / factor),
            ggplot2::aes(x = x, y = y),
            color = color,
            linewidth = 0.65,
            alpha = 0.5
        ) +
        ggplot2::scale_x_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.3)),
            breaks = scales::breaks_pretty(10)
        ) +
        ggplot2::scale_y_continuous(
            trans = "log10",
            breaks = scales::breaks_log(7)
        ) +
        ggplot2::coord_cartesian(xlim = xlimit, ylim = ylimit) +
        ggplot2::labs(x = expression(italic(S)), y = "Number of Triggers") +
        ggplot2::theme_bw(base_size = 15, base_line_size = 0.3) +
        ggplot2::annotation_logticks(sides = "rl") +
        ggplot2::theme(
            plot.margin = ggplot2::unit(c(0, 0, 0, 0), "null"),
            panel.margin = ggplot2::unit(c(0, 0, 0, 0), "null"),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank()
        )
    p
}

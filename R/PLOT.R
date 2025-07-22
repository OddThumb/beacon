# Saving ----

#' Save ggplot plot
#'
#' @param plot A ggplot object.
#' @param file A character. A file path for save.
#' @param width A numeric (default: 8). Width of plot size.
#' @param heigth A numeric (default: 5). Height of plot size.
#' @param show A logical (default: TRUE). Whether it shows and returns the plot on pane after save.
#' @param ... Additional arguments for '\code{ggpubr::ggexport}'.
#' @return A ggplot object, if \code{show=TRUE}.
#' @export
savefig <- function(plot, file, width=8, height=5, show=T, verbose=F,...) {
    message.verb(' >>> Saving...', v=verbose)
    ggpubr::ggexport(plot, filename=paste.wd(file), width=width, height=height, verbose = verbose, ...)
    if (show) {
        return(plot)
    }
}

#' Save message to txt file
#'
#' @param msg A character. A message to save.
#' @param ... A character. Additional messages to save.
#' @param file A character. A file path for save.
#' @param show A logical (default: TRUE). Whether it prints the messages on console.
#' @export
savemsg <- function(msg, ..., file, show=T) {
    write.table(rbind(msg, ...), file=paste.wd(file), quote = F, row.names = F, col.names = F, append = T)
    if (show) {
        message(cat(paste(msg, ..., sep='\n')))
    }
}

#' Message out by each iteration or No verbose
#'
#' @param i An expression. A iterable arguments in for loop.
#' @param verbose A logical or An integer. If 'verbose=TRUE', it message out by every iteration. Else 'verbose=integer()', it message out by every verbose-th iteration.
#' @param msg A character. A message to print out.
#' @export
message.by <- function(i=i, verbose=T, msg) {
    if (verbose) {
        verbby <- 1
        if (i%%verbby == 0) {
            message(msg)
        }
    } else if (is.numeric(verbose)) {
        verbby <- verbose
        if (i%%verbby == 0) {
            message(msg)
        }
    }
}

#' @export
message.verb <- function(..., v=TRUE) {
    if (v) {
        message(...)
    } else {
        NULL
    }
}

#' Show file contents on console
#'
#' This uses unix command '\code{cat}'
#'
#' @param file A character. A file path for print.
#' @export
sys.cat <- function(file) {
    system(paste("cat",paste.wd(file)))
}

#' Save table as txt file
#'
#' This uses 'insight' package to convert a data frame to neat "Markdown" version of table.
#'
#' @param x A data frame.
#' @param file A character. A file path for save.
#' @param signif An integer (default: 3). A significant digits.
#' @param col.names A logical (default: FALSE). Whether it saves column names together.
#' @param row.naems A logical (default: FALSE). Whether it saves row names together.
#' @param quote A logical (default: FALSE). Whether it saves each elements with quote.
#' @param show A logical (default: TRUE). Whether it prints the table on console.
#' @param append A logical (default: FALSE). Whether it appends on an existing file.
#' @param nsep An integer (default: 30). If '\code{append=TRUE}', it adds separation dashed line with the length of 'nsep' before adding.
#' @param ... Additional arguments for '\code{insight::export_table}'.
#' @export
savetab <- function(x, file, signif=3, col.names=F, row.names=F, quote=F, show=T, append=F, nsep=30, ...) {
    check.installed('insight')

    message(' >>> Saving table in MD format...')
    if (append & !is.null(nsep)) {
        write.table(paste(rep("-",nsep),collapse=''), file=paste.wd(file), quote=F, row.names=F, col.names=F, append=append)
    }

    if (row.names) {
        rn <- rownames(x)
        x <- relocate(mutate(x, `_`=rn), `_`, .before=1)
    }

    write.table( insight::export_table(x, digits = paste("signif",signif,sep=''), format = 'md', ...),
                 file=paste.wd(file), col.names=col.names, row.names=F, quote=quote, append=append)
    if (show) sys.cat(file)
}

#' Append line in txt file
#'
#' @param x ANY.
#' @param file A character. An existing file path to append.
#' @param show A logical (default: TRUE). Whether it prints the file on console.
#' @export
append.line <- function(x, file, show=T) {
    message(' >>> Appending lines in a file...')
    write.table(x, file=paste.wd(file), append=T, quote=F, row.names=F, col.names=F)
    if (show) sys.cat(file)
}

# Plotting ----
#' Printing with `cat` and `sprintf` for parsing
#'
#' @export
sprint <- function(..., sep="") {
    cat(sprintf(paste(..., sep = sep)))
}

#' Plot oscillogram
#'
#' @param ts A time series (ts) object.
#' @param tzero  A numeric (default: 0). A zero time. Times will be shifted by 'tzero'.
#' @param trange A vector. A time range for x axis limit.
#' @param ylim A character of "pm" (default) or A vector. "pm": from \code{-1.1*max(abs(ts))} to \code{1.1*max(abs(ts))}. \code{c(lwr, upr)}: from lwr to upr.
#' @param title A character. A title for the plot.
#' @param color A character. A line color.
#' @param width A numeric. A line width.
#' @param ... Additional arguments for '\code{ggplot2::geom_line}'.
#' @return A ggplot object.
#' @export
plot_oscillo <- function(ts, tzero=NULL, trange=NULL, ylim='pm',
                         title=NULL, size=0.3, ...) {
    check.installed(c('scales', 'latex2exp'))
    breaks_pretty <- scales::breaks_pretty
    label_number  <- scales::label_number
    TeX           <- latex2exp::TeX

    if (is.null(trange)) {
        trange <- c(time(ts)[1], time(ts)[length(ts)])
    }

    ts.df <- data.frame('time'=time(ts), 'strain'=ts)
    ts.df <- mutate(filter(ts.df, time >= trange[1] & time <= trange[2]), 'time' = time-ifelse(is.null(tzero),0,tzero))

    value.order <- floor(log10(max(abs(ts.df$strain))))
    plot <- ggplot() +
        geom_line(data = ts.df, aes(x = time, y = strain), size=size, ...) +
        scale_x_continuous(expand = c(0,0),
                           breaks = breaks_pretty(10)) +
        scale_y_continuous(expand = c(0,0),
                           breaks = breaks_pretty(5),
                           labels = label_number(scale = 1/10^(value.order))) +
        labs(x = ifelse(is.null(tzero),"Time (s)",TeX('Time - $t_0$ (s)')),
             y = TeX(paste('$h~(10^{',value.order,'})$',sep='')),
             title = title) +
        theme_bw() +
        theme(legend.position = "none")

    if (is.character(ylim)) {
        if (ylim == 'pm') {
            limiting <- get.limit(ts.df$strain)
            plot <- plot + coord_cartesian(ylim=limiting)
        }
    } else if (is.vector(ylim)) {
        plot <- plot + coord_cartesian(ylim=ylim)
    }

    return(plot)
}

#' Plot multi-panel oscillograms by facet_wrap
#'
#' @param ts.df A data frame. This should be generated by 'ts.df' or contains 'time' column.
#' @param tzero  A numeric (default: 0). A zero time. Times will be shifted by 'tzero'.
#' @param trange A vector. A time range for x axis limit.
#' @param facet_scale A character. How to scale each facets. (One of "fixed", "free_y", or "free_x")
#' @param ylim A character of "pm" (default) or A vector. "pm": from `-1.1*max(abs(ts))` to `1.1*max(abs(ts))`. `c(lwr, upr)`: from lwr to upr.
#' @param title A character. A title for the plot.
#' @param color A character. A line color.
#' @param width A numeric. A line width.
#' @param ... Additional arguments for `ggplot2::geom_line`.
#' @return A ggplot object.
#' @export
plot_oscillo.multi <- function(ts.df, tzero=NULL, trange=NULL,
                               facet.scale='fixed', facet.label=NULL,
                               order.of.mag=TRUE,
                               title=NULL, color=NULL, width=0.3, ...) {
    check.installed(c('scales', 'latex2exp', 'reshape2'))
    breaks_pretty <- scales::breaks_pretty
    label_number  <- scales::label_number
    TeX           <- latex2exp::TeX

    if (is.null(ncol(ts.df))) {
        stop("For one ts, use Oscillo()")
    }

    if (is.null(trange)) {
        trange <- c(ts.df$time[1], tail(ts.df$time,1))
    }

    if (!('time' %in% colnames(ts.df))) {
        ts.df <- dplyr::bind_cols(time=time(ts.df), as.data.frame(ts.df))
    }

    if (!is.null(facet.label)) {
        suppressWarnings({
            colnames(ts.df)[-which(colnames(ts.df)=="time")] <- facet.label
        })
    }

    ts.df <- mutate(filter(ts.df, time >= trange[1] & time <= trange[2]), 'time' = time-ifelse(is.null(tzero),0,tzero))
    ts.df.molten <- reshape2::melt(ts.df, id.vars="time")

    value.order <- floor(log10(amax(ts.df.molten$value)))
    plot <- ggplot(ts.df.molten, aes(x = time, y = value, color=color)) +
        geom_line(size=width, ...) +
        scale_x_continuous(expand = c(0,0),
                           breaks = breaks_pretty(10)) +
        facet_wrap(~variable, ncol=1, strip.position = 'right', scales=facet.scale) +
        labs(x = ifelse(is.null(tzero),"Time (s)",TeX('Time - $t_0$ (s)')),
             y = TeX(paste('$h~(10^{',value.order,'})$',sep='')),
             title = title) +
        theme_bw() +
        theme(legend.position = "none",
              panel.spacing = unit(0, "lines"))

    if (order.of.mag) {
        plot <- plot +
            scale_y_continuous(expand = c(0,0),
                               breaks = breaks_pretty(5),
                               labels = label_number(scale = 1/10^(value.order)))
    } else {
        plot <- plot +
            scale_y_continuous(expand = c(0,0),
                               breaks = breaks_pretty(5))
    }
    return(plot)
}

#' Common ggplot options for oscillogram
#'
#' For applying common theme.
#'
#' @param ts  A time series (ts) object.
#' @param tzero  A numeric (default: 0). A zero time. Times will be shifted by 'tzero'.
#' @param title A character. A title for the plot.
#' @return A list. This can be added to ggplot object to apply same theme.
#' @export
oscillo.option <- function(ts, tzero=0, title=NULL) {
    check.installed(c('latex2exp', 'scales'))
    breaks_pretty <- scales::breaks_pretty
    label_number  <- scales::label_number
    TeX           <- latex2exp::TeX

    value.order <- floor(log10(amax(ts)))
    list(
        scale_x_continuous(expand = c(0,0),
                           breaks = breaks_pretty(5)),
        
        scale_y_continuous(expand = c(0,0),
                           breaks = breaks_pretty(5),
                           labels = label_number(scale = 1/get.order(ts)),
                           limits = get.limit(ts,1.5)),
        
        labs(x = ifelse(tzero != 0,
                        paste0('Time (s) from ', tzero),
                        'Time (s)'),
             y = TeX(paste('$\\textit{h}~(10^{',value.order,'})$',sep='')),
             title = title),
        
        theme_bw(),
        
        theme(legend.position = 'none')
    )
}

#' Plot spectrogram
#'
#' Using ggplot2 to generate q-transformed spectrogram & oscillogram together!
#'
#' @param ts         A time series (\code{ts}) object.
#' @param tzero      A numeric (default: \code{0}). A zero time. Times will be shifted by \code{tzero}.
#' @param trange     A vector (default: \code{tr(ts)}). A time range for x axis limit.
#' @param frange     A vector (default: \code{c(32, 512)}). A frequency range of y axis limit.
#' @param qrange     A vector (default: \code{c(40, 1)}). A q range as an input to \code{qtransform} function.
#' @param crange     A vector (default: \code{NULL}). A color range. A z axis range.
#' @param tres       A numeric (default: \code{0.001}). A time resolution.
#' @param fres       A numeric (default: \code{1000}). A frequency resolution.
#' @param logf       A logical (dafault: \code{TRUE}). Whether transform frequency axis to log (base: 10) scale.
#' @param title      A character. A title for the plot.
#' @param specScale  A logical (default: \code{FALSE}). Whether it shows color scale bar for spectrogram.
#' @param specXlabel A logical (default: \code{FALSE}). Whether it shows x axis label for spectrogram.
#' @param specYlabel A logical (default: \code{TRUE}). Whether it shows y axis label for spectrogram.
#' @param specGrid   A character (default: \code{"none"}). Overlay grid on x- ("x"), y-axis ("y"), or both ("xy").
#' @param specColorPal A Palette for spectrogram (default: \code{viridis::viridis(256)}).
#' @param trans      A function (defualt: \code{NULL}). If function is given, spectrogram will be transformed by a function of \code{trans} (e.g. \code{trans = sqrt} for square-root).
#' @param specScaleDir A character (default: \code{"vertical"}). If \code{specScale=TRUE}, the direction of color bar. (\code{"vertical"} or \code{"horizontal"})
#' @param specScalePos A character (default: \code{"ul"}). If \code{specScale=TRUE}, the position of color bar. (\code{"ul"}, \code{"ur"}, \code{"bl"}, or \code{"br"})
#' @param osciLegend A logical (default: \code{FALSE}). Whether it shows a legend for oscillogram.
#' @param osciXlabel A logical (default: \code{FALSE}). Whether it shows x axis label for oscillogram.
#' @param osciYlabel A logical (default: \code{TRUE}). Whether it shows y axis label for oscillogram.
#' @param stack      A logical (default: \code{TRUE}). Whether it stack spectrogram and oscillogram.
#' @param ...        Additional arguments for \code{ggpubr::ggarrange}.
#' @return A ggplot object. If \code{stack=FALSE}, A list with \code{$spec.plot}: spectrogram ggplot object, \code{$osci.plot}: oscillogram ggplot object.
#' @export
plot_spectro <- function(ts, tzero=0,
                         trange = NULL,
                         frange = c(32, 512),
                         qrange = c(40, 1),
                         crange = NULL,
                         tres=1000, fres=1000, logf=T,
                         title=NULL,
                         specScale=FALSE, specXlabel=FALSE, specYlabel=TRUE, specGrid="none",
                         specColorPal=viridis::viridis(256), trans = NULL, 
                         specScaleDir="vertical", specScalePos="ul",
                         linecolor = 'black',
                         osciLegend=FALSE, osciXlabel=TRUE, osciYlabel=TRUE,
                         stack=T, ...) {
    
    check.installed(c('scales', 'ggpubr', 'latex2exp', 'viridis'))
    library(ggplot2)
    breaks_pretty <- scales::breaks_pretty
    log_breaks    <- scales::log_breaks
    label_number  <- scales::label_number
    TeX           <- latex2exp::TeX
    ggarrange     <- ggpubr::ggarrange
    
    # Frequency
    sampling.freq <- frequency(ts)
    
    # Crop ts with given trange argument
    if (is.null(trange)) {
        trange <- tr(ts)
    }
    ts.crop <- window_to(ts, trange)
    
    # Resolutions
    delt <- 1/tres
    if (logf) {
        delf <- NULL
        logfstep <- fres
    } else {
        delf <- 1/fres
        logfstep <- NULL
    }
    
    # Q-transform
    qspecdata <- qtransform(ts = ts.crop,
                            delta_t = delt,
                            delta_f = delf,
                            logfsteps = logfstep,
                            frange = frange,
                            qrange = qrange,
                            mismatch = 0.2,
                            return_complex = F)
    names(qspecdata) <- c("t", "f", "S")
    
    # Applying transform function on z-axis
    if (!is.null(trans)) {
        qspecdata[['S']] <- trans(qspecdata[['S']])
    }
    
    
    # Limit on color (z-axis)
    if (!is.null(crange)) {
        qspecdata[['S']][ qspecdata[['S']] <= crange[1] ] <- crange[1]
        qspecdata[['S']][ qspecdata[['S']] >= crange[2] ] <- crange[2]
    }
    
    # Convert qspecdata (list) to melted qspecdata.df (data.frame)
    qspecdata.df <- data.frame('f' = rep(qspecdata$f, times = length(qspecdata$t)),
                               't' = rep(qspecdata$t, each  = length(qspecdata$f)),
                               'S' = c(qspecdata$S))
    
    # Filter qspecdata.df with time range and shift with tzero
    qspecdata.df <- qspecdata.df |>
        filter(between(t, trange[1], trange[2])) |>
        mutate('t' = t-tzero)
    
    # Save memory
    qspecdata.df <- qspecdata.df |> mutate(across(t:S, ~ round(.x, digits=4)))
    
    # Spectrogram by GGPLOT2
    spec.plot <- ggplot() +
        geom_raster(data = qspecdata.df, aes(x = t, y = f, fill = S), interpolate = T, ...) +
        scale_x_continuous(expand = c(0,0),
                           breaks = breaks_pretty(10)) +
        scale_y_continuous(expand = c(0,0),
                           breaks = ifelse(logf,log_breaks(7),breaks_pretty(7)),
                           transform = ifelse(logf,"log10","identity")) +
        scale_fill_gradientn(colors = specColorPal)+
        labs(x = TeX('Time - $t_0$ (s)'),
             y = 'Frequency (Hz)',
             fill = 'Normalized Power',
             title = NULL) +
        theme_bw() +
        theme(legend.position = "none",
              plot.margin = margin(1,1,0,1,"lines"))
    
    if (str_detect(specGrid,"x")) {
        spec.plot <- spec.plot +
            geom_vline(xintercept = na.omit(ggplot_build(spec.plot)$layout$panel_params[[1]]$x$breaks),
                       color="grey92", alpha=0.3, linewidth=0.1)
    }
    
    if (str_detect(specGrid,"y")) {
        spec.plot <- spec.plot +
            geom_hline(yintercept = ifelse(logf,
                                           10^na.omit(ggplot_build(spec.plot)$layout$panel_params[[1]]$y$breaks),
                                           na.omit(ggplot_build(spec.plot)$layout$panel_params[[1]]$y$breaks)),
                       color="grey92", alpha=0.3, linewidth=0.1)
    }
    
    if (!specXlabel) {
        spec.plot <- spec.plot +
            theme(axis.title.x = element_blank(),
                  axis.text.x  = element_blank(),
                  axis.ticks.x = element_blank())
    }
    if (!specYlabel) {
        spec.plot <- spec.plot +
            theme(axis.title.y = element_blank(),
                  axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank())
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
            warning('(InputError) `legend.dir` is not correct ("vertical" or "horizontal"). Assume legend.dir="horizontal"')
            specScaleDir <-  "horizontal"
            lgndttlpos <- "top"
            lgndttlang <- 0
        }
        
        lgndpos <- switch(specScalePos,
                          "ul"=c(0,1),
                          "ur"=c(1,1),
                          "bl"=c(0,0),
                          "br"=c(1,0))
        lgndjus <- switch(specScalePos,
                          "ul"=c(0,1),
                          "ur"=c(1,1),
                          "bl"=c(0,0),
                          "br"=c(1,0))
        spec.plot <- spec.plot +
            guides(
                size = "none",
                fill = guide_colourbar(title.position = lgndttlpos),
                color= FALSE
            ) +
            theme(legend.position      = lgndpos,
                  legend.justification = lgndjus,
                  legend.direction     = specScaleDir,
                  legend.title         = element_text(size = 9, angle = lgndttlang),
                  legend.title.align   = 0.5,
                  legend.text          = element_text(size = 8),
                  legend.background    = element_rect(fill = alpha('white', 0.4)))
    }
    
    # Drawing oscillogram by GGPLOT2
    ts_df <- ts.df(ts.crop, tzero)
    
    limiting <- get.limit(ts_df$x)
    value.order <- floor(log10(amax(ts_df$x)))
    osci.plot <- ggplot() +
        geom_line(data = ts_df, aes(x = time, y = x), linewidth=0.3, color = linecolor) +
        scale_x_continuous(expand = c(0,0),
                           breaks = breaks_pretty(10)) +
        scale_y_continuous(expand = c(0,0),
                           breaks = breaks_pretty(5),
                           labels = label_number(scale = 1/10^(value.order)),
                           limits = limiting) +
        labs(x = TeX('Time - $t_0$ (s)'),
             y = TeX(paste('$h~(10^{',value.order,'})$',sep='')),
             title = NULL) +
        theme_bw() +
        theme(legend.position = "none",
              plot.margin = margin(0,1,1,1,"lines"))
    if (!osciXlabel) {
        osci.plot <- osci.plot +
            theme(axis.title.x = element_blank(),
                  axis.text.x  = element_blank(),
                  axis.ticks.x = element_blank())
    }
    if (!osciYlabel) {
        osci.plot <- osci.plot +
            theme(axis.title.y = element_blank(),
                  axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank())
    }
    if (osciLegend) {
        osci.plot <- osci.plot +
            theme(legend.position = "right") +
            theme_bw()
    }
    
    # Return a stacked plot or two separated plots
    if (stack) {
        p <- ggpubr::ggarrange(spec.plot, osci.plot, heights=c(0.7, 0.3), nrow=2, align='v')
        
        if (!is.null(title)) {
            p <- p |> ggpubr::annotate_figure(top = ggpubr::text_grob(title, face = "bold", size = 14))
        }
        return(p)
        
    } else {
        spec.plot <- spec.plot + theme(plot.margin = margin(5.5,5.5,5.5,5.5))
        osci.plot <- osci.plot + theme(plot.margin = margin(5.5,5.5,5.5,5.5))
        if (!is.null(title)) {
            spec.plot <- spec.plot |> 
                ggpubr::annotate_figure(top = ggpubr::text_grob(title, face = "bold", size = 14))
        }
        if (!is.null(title)) {
            osci.plot <- osci.plot |> 
                ggpubr::annotate_figure(top = ggpubr::text_grob(title, face = "bold", size = 14))
        }
        return(list('spec.plot'=spec.plot, 'osci.plot'=osci.plot))
    }
}



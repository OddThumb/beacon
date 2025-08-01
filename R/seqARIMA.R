#' Sequential ARIMA pipeline components
#'
#' This file contains all key components used in the seqarima pipeline,
#' including differencing, autoregressive (AR) fitting, and moving average (MA) smoothing functions.
#'

#' Identify missing values in a time series relative to a reference
#'
#' @param ts A time series (`ts`) object that may have NA values.
#' @param ref A reference `ts` object used for comparison.
#'
#' @return A list with:
#' \describe{
#'   \item{all}{All time stamps with NA in `ts` compared to `ref`}
#'   \item{head}{NA timestamps at the start of the series}
#'   \item{tail}{NA timestamps at the end of the series}
#' }
#' @export
get_MissingValues <- function(ts, ref) {
    NA.idx <- is.na(match(time(ref), time(ts)))
    head.NA.idx <- 1:(which(NA.idx == FALSE)[1] - 1)
    tail.NA.idx <- (tail(which(NA.idx == FALSE), 1) + 1):length(NA.idx)

    NA.times <- time(ref)[NA.idx]
    NA.times.head <- time(ref)[head.NA.idx]
    NA.times.tail <- time(ref)[tail.NA.idx]
    NA.times.list <- list(
        "all" = NA.times,
        "head" = NA.times.head,
        "tail" = NA.times.tail
    )
    NA.times.list
}

#' Perform PCA with fixed sign for interpretability
#'
#' @param x A data frame or matrix.
#' @param retx,center,scale.,tol,rank.,... Passed to `prcomp()`.
#'
#' @return A modified PCA object with signs adjusted.
#' @export
apply_pca <- function(
    x,
    retx = T,
    center = F,
    scale. = F,
    tol = NULL,
    rank. = NULL,
    ...) {
    # Original prcomp
    pca <- prcomp(
        x,
        retx = retx,
        center = center,
        scale. = scale.,
        tol = tol,
        rank. = rank.,
        ...
    )

    # Find the first-row-negative-element's columns and convert them by multiplying -1
    pca$rotation[, which(pca$rotation[1, ] < 0)] <- -1 *
        pca$rotation[, which(pca$rotation[1, ] < 0)]

    # Reproduce pca results
    pca$x <- as.matrix(x) %*% pca$rotation
    class(pca) <- c(class(pca), "fix_sign")

    return(pca)
}

#' Extract principal component(s) from input
#'
#' @param x A data frame or matrix.
#' @param pc A string or vector of PC names (e.g., "PC1").
#'
#' @return A matrix or vector of selected components.
#' @export
extract_pc <- function(x, pc = "PC1") {
    unname(apply_pca(na.omit(x))$x[, pc])
}

#' Check for stationarity using the KPSS test across segments
#'
#' @param ts A `ts` object.
#' @param t.seg Segment duration in seconds.
#'
#' @return A matrix of KPSS p-values for level and trend.
#' @export
check_stationary <- function(ts, t.seg = 0.5) {
    kpss.test <- function(x) {
        c(
            tseries::kpss.test(x, null = "Level", lshort = F)$p.value,
            tseries::kpss.test(x, null = "Trend", lshort = F)$p.value
        )
    }

    suppressMessages({
        ts <- evenify(ts) # If the given ts length is odd, an error about NA result occurs
    })
    sampling.freq <- frequency(ts)
    chunk.length <- t.seg * sampling.freq
    split.times <- split(time(ts), ceiling(seq_along(ts) / chunk.length))
    split.trs <- lapply(split.times, function(times) {
        c(head(times, 1), tail(times, 1))
    })
    suppressWarnings({
        p.values <- sapply(split.trs, function(tr) {
            kpss.test(window_to(ts, tr))
        })
        colnames(p.values) <- seq_along(split.trs)
        rownames(p.values) <- c("Level", "Trend")
    })

    return(p.values)
}

#' Check for normality using the Anderson-Darling test
#'
#' @param ts A `ts` object.
#' @param t.seg Segment duration in seconds.
#'
#' @return A numeric vector of p-values.
#' @export
check_normality <- function(ts, t.seg = 0.5) {
    norm.test <- function(x) {
        nortest::ad.test(x)$p.value
    }

    suppressMessages({
        ts <- evenify(ts) # If the given ts length is odd, an error about NA result occurs
    })
    sampling.freq <- frequency(ts)
    chunk.length <- t.seg * sampling.freq
    split.times <- split(time(ts), ceiling(seq_along(ts) / chunk.length))
    split.trs <- lapply(split.times, function(times) {
        c(head(times, 1), tail(times, 1))
    })
    p.values <- sapply(split.trs, function(tr) {
        norm.test(window_to(ts, tr))
    })
    return(p.values)
}

#' Automatically determine differencing order via KPSS
#'
#' @param ts A `ts` object.
#' @param t.seg Segment duration.
#' @param d_max Maximum differencing order.
#' @param verbose Whether to print messages.
#'
#' @return A list with selected order, differenced output, and KPSS p-values.
#' @export
auto_diff <- function(ts, t.seg = 0.5, d_max = 2, verbose = TRUE) {
    ret.list <- list()
    pval.list <- list()
    out <- ts

    d <- 0

    message_verb(
        "|> KPSS test on segments (each ",
        t.seg,
        " second)",
        v = verbose
    )
    message_verb("||> d=", d, v = verbose)
    p.values <- suppressWarnings(check_stationary(out, t.seg))
    pval.list[[paste("d", d, sep = "")]] <- p.values

    while (!all(p.values == 0.1)) {
        message_verb("|||> Non-stationary. p-value < 10 %", v = verbose)

        d <- d + 1
        out <- diff(out, difference = 1)

        message_verb("||> d=", d, v = verbose)
        p.values <- suppressWarnings(check_stationary(out, t.seg))
        pval.list[[paste("d", d, sep = "")]] <- p.values
        if (!is.null(d_max)) {
            if (d == d_max) break
        }
    }
    ret.list[["d"]] <- d
    ret.list[["out"]] <- out
    ret.list[["p.values"]] <- pval.list

    return(ret.list)
}

#' Difference a time series manually or automatically
#'
#' @param ts A `ts` object.
#' @param d Differencing order. Use 0 for none, integer for fixed, 'auto' for automatic.
#' @param t.seg Segment duration.
#' @param return.pvals Return KPSS p-values.
#' @param verbose Whether to print messages.
#'
#' @return A differenced `ts` object with metadata.
#' @export
Differencing <- function(
    ts,
    d,
    t.seg = 0.5,
    return.pvals = FALSE,
    verbose = TRUE) {
    if (missing(d)) {
        stop(
            "Argument 'd' must be specified as either 'auto', 0, or a positive integer."
        )
    }

    if (identical(d, "auto")) {
        # KPSS-based auto differencing (unbounded)
        diff.res <- auto_diff(ts, t.seg = t.seg, verbose = verbose)
        message_verb("|> d=", diff.res$d, " selected!", v = verbose)

        out <- diff.res$out
        d.order <- diff.res$d
        method <- "auto"

        if (return.pvals) {
            attr(out, "p_values") <- diff.res$p_values
        }

        attr(out, "unbounded") <- TRUE
    } else if (is.numeric(d) && d > 0) {
        # Fixed-order differencing
        out <- diff(ts, differences = d)
        d.order <- d
        method <- "fixed"
        diff.res <- NULL
    } else if (identical(d, 0L) || identical(d, 0)) {
        # Explicit no differencing
        out <- ts
        d.order <- 0
        method <- NULL
        diff.res <- NULL
    } else {
        stop("Argument 'd' must be 'auto', 0, or a positive integer.")
    }

    # Attach metadata as individual attributes
    attr(out, "d_order") <- d.order
    attr(out, "method") <- method
    attr(out, "diff_res") <- diff.res

    return(out)
}


#' Compute residuals using AR coefficients
#'
#' @param x Time series data.
#' @param ar AR coefficients.
#' @param numCores Number of CPU cores to use.
#'
#' @return Residual time series.
#' @export
residual <- function(x, ar, numCores = parallel::detectCores()) {
    order <- length(ar)
    ts(
        c(
            rep(NA, order),
            embedParallelCpp(x, order + 1L, numCores) %*% c(1, -ar)
        ),
        start = ti(x),
        frequency = frequency(x)
    )
}

#' AR Burg core (C interface)
#'
#' @param x Input series.
#' @param order.max Maximum AR order.
#'
#' @return List with AR coefficient matrix and prediction variances.
#' @export
C_Burg <- function(x, order.max) {
    z <- .Call(stats:::C_Burg, x, order.max)
    list(
        "coefs" = matrix(z[[1L]], order.max, order.max),
        "var.pred1" = z[[2L]],
        "var.pred2" = z[[3L]]
    )
}

#' Fit AR model using modified Burg method
#'
#' @param x A `ts` object.
#' @param ic Information criterion: logical or character.
#' @param order.max Maximum AR order.
#' @param na.action How to handle NA.
#' @param demean Whether to subtract the mean.
#' @param series Series name.
#' @param var.method Method for innovation variance.
#' @param ... Additional args.
#'
#' @return An `ar` object.
#' @export
burgar <- function(
    x,
    ic = TRUE,
    order.max = NULL,
    na.action = na.fail,
    demean = TRUE,
    series = NULL,
    var.method = 2L,
    numCores = NULL,
    ...) {
    AIC <- function(order.max, vars.pred, n.used) {
        2 * (0L:order.max) + n.used * log(vars.pred)
    }
    BIC <- function(order.max, vars.pred, n.used) {
        (0L:order.max) * log(n.used) + n.used * log(vars.pred)
    }
    FPE <- function(order.max, vars.pred, n.used) {
        (n.used + ((0L:order.max) + 1)) /
            (n.used - ((0L:order.max) + 1)) *
            vars.pred
    }

    if (is.null(series)) {
        series <- deparse1(substitute(x))
    }

    if (ists <- is.ts(x)) {
        xtsp <- tsp(x)
    }

    x <- na.action(as.ts(x))

    if (anyNA(x)) {
        stop("NAs in 'x'")
    }

    if (ists) {
        xtsp <- tsp(x)
    }

    xfreq <- frequency(x)

    x <- as.vector(x)

    if (demean) {
        x.mean <- mean(x)
        x <- x - x.mean
    } else {
        x.mean <- 0
    }

    n.used <- length(x)

    order.max <- if (is.null(order.max)) {
        min(n.used - 1L, floor(10 * log10(n.used)))
    } else {
        floor(order.max)
    }

    if (order.max < 1L) {
        stop("'order.max' must be >= 1")
    } else if (order.max >= n.used) {
        stop("'order.max' must be < 'n.used'")
    }

    xic <- numeric(order.max + 1L)

    z <- .Call(stats:::C_Burg, x, order.max)
    coefs <- matrix(z[[1L]], order.max, order.max)

    # partialacf
    partialacf <- array(diag(coefs), dim = c(order.max, 1L, 1L))

    # var.pred (as a role of maximum likelihood function)
    vars.pred <- if (var.method == 1L) {
        z[[2L]]
    } else {
        z[[3L]]
    }
    if (any(is.nan(vars.pred))) {
        stop("zero-variance series")
    }

    # Information Criteria part
    if (is.logical(ic)) {
        xic <- n.used * log(vars.pred) + 2 * (0L:order.max) + 2 * demean
        attr(xic, "ic") <- "default"

        mic <- min(xic)
        xic <- setNames(
            if (is.finite(mic)) {
                xic - min(xic)
            } else {
                ifelse(xic == mic, 0, Inf)
            },
            0L:order.max
        )

        order <- if (ic) {
            (0L:order.max)[xic == 0]
        } else {
            order.max
        }
    } else {
        ic_fun <- switch(ic,
            "AIC" = AIC,
            "BIC" = BIC,
            "FPE" = FPE
        )
        xic <- ic_fun(order.max, vars.pred, n.used)
        attr(xic, "ic") <- ic

        mic <- min(xic)
        xic <- setNames(
            if (is.finite(mic)) {
                xic - min(xic)
            } else {
                ifelse(xic == mic, 0, Inf)
            },
            0L:order.max
        )

        order <- (0L:order.max)[xic == 0]
    }

    # AR coefficients
    ar <- if (order) {
        coefs[order, 1L:order]
    } else {
        numeric()
    }

    var.pred <- vars.pred[order + 1L]

    # To calculate residuals, C++ version of embed function is employed.
    if (is.null(numCores)) {
        maxcores <- parallel::detectCores()
        numCores <- ifelse(order.max > 4e3, maxcores, maxcores / 2)
    }
    resid <- if (order) {
        c(
            rep(NA, order),
            embedParallelCpp(x, order + 1L, numCores) %*% c(1, -ar)
        )
    } else {
        x
    }

    if (ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "ts"
    }

    res <- list(
        order = order,
        ar = ar,
        var.pred = var.pred,
        vars.pred = vars.pred,
        x.mean = x.mean,
        ic = xic,
        n.used = n.used,
        n.obs = n.used,
        order.max = order.max,
        partialacf = partialacf,
        resid = resid,
        method = ifelse(var.method == 1L, "Burg", "Burg2"),
        series = series,
        frequency = xfreq,
        call = match.call()
    )

    res$asy.var.coef <- NULL
    # WE DON'T NEED THIS WHICH TAKES TIME A LOT!
    # if (order) {
    #    xacf <- acf(x, type = "covariance", lag.max = order,
    #                plot = FALSE)$acf
    #    res$asy.var.coef <- solve(toeplitz(drop(xacf)[seq_len(order)])) *
    #        var.pred/n.used
    # }

    class(res) <- "ar"

    res
}

#' Simple wrapper for AR fitting
#'
#' @param ts A `ts` object.
#' @param ... Arguments passed to `burgar()`.
#'
#' @return Residual `ts` object with metadata.
#' @export
sar <- function(ts, ...) {
    AR <- burgar(x = ts, ...)
    resid <- na.omit(AR$resid)

    attr(resid, "collector") <- "single"
    attr(resid, "feature") <- AR$ar
    attr(resid, "p_order") <- AR$order
    return(resid)
}

#' Plot AIC curve for AR model
#'
#' @param ar An `ar` object.
#'
#' @return A ggplot AIC plot with zoomed-in inset.
#' @export
plot_aic <- function(ar) {
    ar.aic.df <- data.frame(p = seq(0, length(ar$aic) - 1), AIC = ar$aic)
    ar.order <- filter(ar.aic.df, p == ar$order)

    aic.plot <- ggplot2::ggplot(
        data = ar.aic.df,
        ggplot2::aes(x = p, y = AIC)
    ) +
        ggplot2::geom_line(color = "blue") +
        ggplot2::geom_point(
            ggplot2::aes(alpha = ""),
            data = ar.order,
            color = "red",
            size = 2
        ) +
        ggplot2::geom_vline(
            xintercept = ar.order$p,
            linetype = "dashed",
            size = 1,
            color = "red"
        ) +
        ggplot2::scale_alpha_manual(values = 1) +
        ggplot2::labs(x = "AR orders", y = "AIC") +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position = "none",
            plot.margin = ggplot2::unit(c(0, 1, 1, 1), "lines")
        )

    plot.xlim <- c(
        ar.order$p - 3 - (ar.order$p %% 5),
        (ar.order$p + 5) - ((ar.order$p + 5) %% 5)
    )
    plot.ylim <- c(0, ar.order$AIC + 5)
    aic.plot.zoom <- aic.plot +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::coord_cartesian(xlim = plot.xlim, ylim = plot.ylim) +
        ggplot2::annotate(
            "text",
            x = ar.order$p + 0.2,
            y = plot.ylim[2] / 2,
            label = latex2exp::TeX(
                paste("p=", ar.order$p, sep = ""),
                italic = T,
                bold = T
            ),
            color = "red",
            hjust = 0,
            size = 5
        ) +
        ggplot2::theme(legend.position = "none")

    aic.plot.final <- aic.plot +
        ggplot2::geom_rect(
            ggplot2::aes(
                xmin = ar.order$p - 500,
                xmax = ar.order$p + 500,
                ymin = ar.order$AIC,
                ymax = plot.ylim[2] + ar.order$AIC
            ),
            color = "black",
            linetype = "dashed",
            alpha = 0,
            size = 0.3
        ) +

        ggplot2::geom_segment(
            ggplot2::aes(
                x = ar.order$p - 500,
                xend = 5,
                y = ar.order$AIC,
                yend = 1e1 + ar.order$AIC
            ),
            linetype = "dashed",
            size = 0.3
        ) +

        ggplot2::geom_segment(
            ggplot2::aes(
                x = ar.order$p - 500,
                xend = 500,
                y = plot.ylim[2] + ar.order$AIC,
                yend = 5e3 + ar.order$AIC
            ),
            linetype = "dashed",
            size = 0.3
        ) +

        ggplot2::annotation_custom(
            grob = ggplot2::ggplotGrob(aic.plot.zoom),
            xmin = log10(5),
            xmax = log10(500),
            ymin = log10(1e1 + ar.order$AIC),
            ymax = log10(5e3 + ar.order$AIC)
        ) +

        ggplot2::eom_rect(
            ggplot2::aes(
                xmin = 5,
                xmax = 500,
                ymin = 1e1 + ar.order$AIC,
                ymax = 5e3 + ar.order$AIC
            ),
            color = "black",
            linetype = "dashed",
            alpha = 0,
            size = 0.4
        ) +

        ggplot2::scale_x_continuous(expand = c(0, 0), trans = "log10") +
        ggplot2::scale_y_continuous(expand = c(0, 0), trans = "log10") +
        ggplot2::annotation_logticks()

    return(aic.plot.final)
}

#' Ensemble AR model fitting across multiple orders
#'
#' @param ts A `ts` object.
#' @param ps Vector of AR orders.
#' @param aic Use AIC selection.
#' @param return.vec Flatten features.
#' @param return.var Include prediction variance.
#' @param return.mean Include series mean.
#' @param collector Aggregation method.
#' @param ... Additional args passed to `burgar()`.
#'
#' @return Residual `ts` object with ensemble features.
#' @export
ear <- function(
    ts,
    ps,
    aic = T,
    return.vec = T,
    return.var = T,
    return.mean = T,
    collector = "median",
    ...) {
    # Apply ARfeat over ps
    AR.fits <- lapply(ps, function(p) burgar(ts, ic = aic, order.max = p, ...))

    # Check duplicated orders exist
    order.lis <- sapply(AR.fits, function(x) x$order)
    AR.fits <- AR.fits[!duplicated(order.lis)]
    psel <- order.lis[!duplicated(order.lis)]
    names(order.lis) <- ps

    # Extracting residuals
    resid.lis <- lapply(AR.fits, function(arp) arp$resid)
    names(resid.lis) <- paste0("AR", psel)
    if (length(resid.lis) == 1) {
        resid.ens <- resid.lis[[1]]
        collector <- "Not aggregated because selected orders are duplicated in given input orders"
    } else {
        resid.mts <- do.call("ts.intersect", resid.lis)
        resid.ens <- switch(collector,
            pca = tsfy(
                extract_pc(resid.mts),
                ref = na.omit(resid.mts[, ncol(resid.mts)])
            ),
            mean = tsfy(
                apply(resid.mts, 1, mean),
                ref = resid.mts[, 1]
            ),
            median = tsfy(
                apply(resid.mts, 1, median),
                ref = resid.mts[, 1]
            )
        )
    }

    # Extracting features (coefficients, variance (option), mean (option))
    feat.lis <- lapply(AR.fits, function(arp) {
        ret <- c("ar" = arp$ar)
        if (return.var) {
            ret <- c(ret, "var" = arp$var.pred)
        }
        if (return.mean) {
            ret <- c(ret, "mean" = arp$x.mean)
        }
        ret
    })

    # Naming features
    for (i in seq_along(psel)) {
        p <- psel[[i]]
        if (p == 0) {
            newname <- NULL
        } else {
            prefix <- paste0("ar", p)
            newname <- paste0(prefix, "_", 1:p)
        }

        if (return.var) {
            newname <- c(newname, paste0(prefix, "_var"))
        }
        if (return.mean) {
            newname <- c(newname, paste0(prefix, "_mean"))
        }
        names(feat.lis[[i]]) <- newname
    }

    if (return.vec) {
        feat.lis <- unlist(feat.lis)
    }

    attr(resid.ens, "collector") <- collector
    attr(resid.ens, "feature") <- feat.lis
    attr(resid.ens, "p_order") <- psel
    return(resid.ens)
}

#' Fit AR model using either single or ensemble approach
#'
#' @param ts A `ts` object.
#' @param p AR order(s).
#' @param aic Use AIC.
#' @param verbose Verbose output.
#' @param ... Additional args.
#'
#' @return Residual `ts` object with AR metadata.
#' @export
Autoregressive <- function(ts, p, aic = TRUE, verbose = TRUE, ...) {
    if (missing(p)) {
        stop(
            "Argument 'p' must be specified as either a single integer or a vector of candidate orders."
        )
    }

    if (length(p) > 1) {
        ar_res <- ear(ts, ps = p, aic = aic, ...)
        p_order <- attr(ar_res, "p_order")
        message_verb(
            "|> p={",
            paste(p_order, collapse = ", "),
            "} is selected and aggregated by a collector: ",
            attr(ar_res, "collector"),
            v = verbose
        )
    } else {
        # length(p) == 1 : Single AR
        ar_res <- sar(ts, ic = aic, order.max = p, ...)
        p_order <- attr(ar_res, "p_order")
        message_verb("|> p=", p_order, " is selected!", v = verbose)
    }

    return(ar_res)
}

#' Two-sided Centered Moving Average Smoother
#'
#' Applies a centered moving average filter to a time series, with optional custom weighting.
#' If no weights are provided, a rectangular window (simple average) is used.
#'
#' @param ts A `ts` object to be smoothed.
#' @param order An integer specifying the window size (moving average order).
#' @param weights Optional numeric vector of weights of length `order`.
#'   If supplied, it will be normalized internally. If `NULL`, a rectangular window is used.
#' @param na.rm Logical. If `TRUE` (default), `NA`s introduced by the filtering at the edges
#'   will be removed via `na.omit()`.
#'
#' @return A smoothed `ts` object with attributes:
#' \describe{
#'   \item{\code{collector}}{A character string set to "single" for tracking.}
#'   \item{\code{q_order}}{The integer order of the moving average used.}
#' }
#'
#' @export
ma <- function(ts, order, weights = NULL, na.rm = T) {
    if (abs(order - round(order)) > 1e-8) {
        stop("order must be an integer")
    }
    if (!is.null(weights)) {
        if (length(weights) != order) {
            stop("Length of weights must match 'order'")
        }
        w <- weights / sum(weights) # normalize
    } else {
        # rectangular
        if (order %% 2 == 0) {
            w <- c(0.5, rep(1, order - 1), 0.5) / order
        } else {
            w <- rep(1, order) / order
        }
    }
    out <- stats::filter(ts, filter = w, sides = 2)

    if (na.rm) {
        out <- na.omit(out)
    }
    attr(out, "collector") <- "single"
    attr(out, "q_order") <- order
    return(out)
}

#' Apply Ensemble of Averages (EoA)
#'
#' @param ts A `ts` object.
#' @param qs MA orders.
#' @param collector Aggregation method.
#'
#' @return Smoothed `ts` object with MA ensemble metadata.
#' @export
eoa <- function(ts, qs, collector = "median") {
    mas <- ts(
        sapply(qs, function(q) ma(ts, q, na.rm = F)),
        start = ti(ts),
        frequency = frequency(ts)
    )
    colnames(mas) <- paste("q", qs, sep = "")

    res <- switch(collector,
        "pca" = tsfy(
            extract_pc(mas),
            ref = na.omit(mas[, ncol(mas)])
        ),
        "mean" = tsfy(
            apply(mas, 1, mean),
            ref = mas[, 1]
        ),
        "median" = tsfy(
            apply(mas, 1, median),
            ref = mas[, 1]
        )
    )
    attr(res, "collector") <- collector
    attr(res, "q_order") <- qs
    attr(res, "mas") <- mas
    return(res)
}

#' Apply MA or EoA depending on `q`
#'
#' @param ts A `ts` object.
#' @param q MA order(s).
#' @param verbose Verbose output.
#' @param ... Additional args passed to `eoa()`.
#'
#' @return Smoothed `ts` object.
#' @export
MovingAverage <- function(ts, q, verbose = TRUE, ...) {
    if (missing(q)) {
        stop(
            "Argument 'q' must be specified as either a single integer or a vector of candidate orders."
        )
    }

    if (length(q) > 1) {
        # Ensemble of Average (EoA)
        message_verb("> (3) EoA (Ensemble of Averages) stage", v = verbose)
        ma_res <- eoa(ts, q, ...)
        q_order <- attr(ma_res, "q_order")
        message_verb(
            "|> q={",
            paste(q_order, collapse = ","),
            "} by a collector: ",
            attr(ma_res, "collector"),
            v = verbose
        )
    } else {
        # Single MA
        message_verb("> (3) MA stage with q=", q, v = verbose)
        ma_res <- ma(ts, q)
        message_verb("|> q=", attr(ma_res, "q_order"), v = verbose)
    }

    return(ma_res)
}

#' Apply sequential ARIMA pipeline
#'
#' Applies a pipeline of (1) differencing, (2) autoregressive (AR) modeling,
#' (3) moving average smoothing, and (4) band-pass filtering.
#'
#' @param ts A time series (`ts`) object.
#' @param d Differencing order. Use 0 for no differencing, or 'auto' for KPSS-based auto-differencing.
#' @param p AR order. Can be a single integer or a vector for ensemble.
#' @param q MA order. Can be a single integer or a vector for ensemble.
#' @param fl Lower frequency bound for band-pass filter.
#' @param fu Upper frequency bound for band-pass filter.
#' @param ar.aic Logical. Whether to use AIC-based AR model selection.
#' @param ar.collector Collector for AR ensemble. One of 'mean', 'median', or 'pca'.
#' @param ma.collector Collector for MA ensemble. One of 'mean', 'median', or 'pca'.
#' @param return.step Logical. If TRUE, intermediate stages are returned.
#' @param verbose Logical. If TRUE, messages are printed during execution.
#'
#' @return A `ts` object with processed output and meta attributes.
#'
#' @examples
#' # Generate noisy sinusoid
#' set.seed(123)
#' fs <- 1024
#' t <- seq(0, 1, by = 1 / fs)
#' x <- sin(2 * pi * 60 * t) + rnorm(length(t), sd = 0.5)
#' ts_obj <- ts(x, start = t[1], frequency = fs)
#'
#' # Apply sequential ARIMA pipeline
#' out <- seqarima(
#'     ts_obj,
#'     d = 1,
#'     p = c(50, 100, 150),
#'     q = 20,
#'     fl = 30,
#'     fu = 300,
#'     ar.collector = "median",
#'     ma.collector = "mean",
#'     return.step = TRUE,
#'     verbose = FALSE
#' )
#'
#' # Plot result
#' plot(out, main = "Output of seqarima()")
#'
#' # View AR model metadata
#' attr(out, "meta")$ar_feat
#'
#' @export
seqarima <- function(
    ts,
    d = NULL,
    p = NULL,
    q = NULL,
    fl = NULL,
    fu = NULL,
    ar.aic = TRUE,
    ar.collector = "median",
    ma.collector = "median",
    return.step = FALSE,
    verbose = TRUE) {
    # Argument matching
    ar.collector <- match.arg(ar.collector, c("mean", "median", "pca"))
    ma.collector <- match.arg(ma.collector, c("mean", "median", "pca"))

    message_verb("> Running seqarima...", v = verbose)

    # Check ts object
    if (!is.ts(ts)) {
        stop("Error) Input object type is not 'ts'")
    }

    sampling.freq <- frequency(ts)
    return.list <- list()
    if (return.step) {
        return.list[["steps"]][["input"]] <- ts
    }
    out <- ts

    # 1) Differencing
    if (!is.null(d)) {
        message_verb("> (1) Difference stage", v = verbose)
        x_d <- Differencing(out, d = d, verbose = verbose)
        return.list[["d"]] <- attr(x_d, "d_order")
        if (return.step) {
            return.list[["steps"]][["I"]] <- x_d
        }
        out <- x_d
    }

    # 2) Auto-Regressive (EAR)
    if (!is.null(p)) {
        message_verb("> (2) Autoregressive stage", v = verbose)
        x_ar <- Autoregressive(
            ts = out,
            p = p,
            aic = ar.aic,
            verbose = verbose,
            collector = ar.collector
        )
        return.list[["ar_feat"]] <- attr(x_ar, "feature")
        return.list[["p"]] <- attr(x_ar, "p_order")
        if (return.step) {
            return.list[["steps"]][["AR"]] <- x_ar
        }
        out <- x_ar
    }

    # 3) Moving Average (EoA)
    if (!is.null(q)) {
        x_ma <- MovingAverage(
            ts = out,
            q = q,
            verbose = verbose,
            collector = ma.collector
        )
        if (return.step) {
            return.list[["steps"]][["MA"]] <- x_ma
        }
        return.list[["q"]] <- attr(x_ma, "q_order")
        out <- x_ma
    }

    # 4) Band-pass filter
    if ((fl != 0 || fu != 0) && (!is.null(fl) || !is.null(fu))) {
        message_verb("> (4) Pass filter stage", v = verbose)

        x_bp <- BandPass(out, fl, fu, verb = verbose)
        if (return.step) {
            return.list[["steps"]][["BP"]] <- x_bp
        }
        return.list[["cutoff"]] <- attr(x_bp, "cutoff")
        out <- x_bp
    }

    # Also return missing values caused by ARIMA
    return.list[["NA.times"]] <- get_MissingValues(
        ts = out,
        ref = ts
    )

    attr(out, "meta") <- return.list

    return(out)
}

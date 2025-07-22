# Denoise ----

#' Get Missing Values (NA) between ts and ref in terms of time stamps
#'
#' @param ts A time series (`ts`) object who has NA values.
#' @param ref A time series (`ts`) object for comparison.
#' @return A list with
#'      \code{$all}: time stamps whose values are NA,
#'      \code{$head}: time stamps whose values are NA at the head,
#'      \code{$tail}: time stamps whose values are NA at the tail.
#' @export
get.MissingValues <- function(ts, ref) {
    NA.idx <- is.na(match(time(ref), time(ts)))
    head.NA.idx <- 1:(which(NA.idx == FALSE)[1] - 1)
    tail.NA.idx <- (tail(which(NA.idx == FALSE), 1) + 1):length(NA.idx)

    NA.times <- time(ref)[NA.idx]
    NA.times.head <- time(ref)[head.NA.idx]
    NA.times.tail <- time(ref)[tail.NA.idx]
    NA.times.list <- list(
        'all' = NA.times,
        'head' = NA.times.head,
        'tail' = NA.times.tail
    )
    NA.times.list
}

#' Check stationary with KPSS test
#'
#' @param ts A time series (`ts`) object.
#' @param t.seg A numeric (default: 0.5). Time length of each segment in second.
#' @return A data frame of p.values for both level and trend.
#' @export
check_stationary <- function(ts, t.seg = 0.5) {
    check.installed("tseries")
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
        }) |>
            `colnames<-`(seq_along(split.trs)) |>
            `rownames<-`(c("Level", "Trend"))
    })

    return(p.values)
}

#' Check stationary with AD test
#'
#' @param ts A time series (`ts`) object.
#' @param t.seg A numeric (default: 0.5). Time length of each segment in second.
#' @return A numeric vector of p.values.
#' @export
check_normality <- function(ts, t.seg = 0.5) {
    check.installed("nortest")
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

#' Automatic difference by KPSS test
#'
#' @param ts A time series (`ts`) object.
#' @param t.seg A numeric (default: 0.5). Time length of each segment in second.
#' @return A list with `$d`: the optimal order of difference, `$out`: A differenced time series (`ts`) object, and `$p.values`: A list containing data frames of p-values for KPSS test result.
#' @export
auto.diff <- function(ts, t.seg = 0.5, d_max = 2, verbose = TRUE) {
    ret.list <- list()
    pval.list <- list()
    out <- ts

    d <- 0

    message.verb(
        "|> KPSS test on segments (each ",
        t.seg,
        " second)",
        v = verbose
    )
    message.verb("||> d=", d, v = verbose)
    p.values <- suppressWarnings(check_stationary(out, t.seg))
    pval.list[[paste("d", d, sep = "")]] <- p.values

    while (!all(p.values == 0.1)) {
        message.verb('|||> Non-stationary. p-value < 10 %', v = verbose)

        d <- d + 1
        out <- diff(out, difference = 1)

        message.verb("||> d=", d, v = verbose)
        p.values <- suppressWarnings(check_stationary(out, t.seg))
        pval.list[[paste("d", d, sep = "")]] <- p.values
        if (!is.null(d_max)) {
            if (d == d_max) break
        }
    }
    ret.list[['d']] <- d
    ret.list[['out']] <- out
    ret.list[['p.values']] <- pval.list

    return(ret.list)
}

#' Get residual from stats::ar
#'
#' @export
residual <- function(x, ar, order, numCores = parallel::detectCores()) {
    ts(
        c(
            rep(NA, order),
            embedParallelCpp(x, order + 1L, numCores) %*% c(1, -ar)
        ),
        start = ti(x),
        frequency = frequency(x)
    )
}

#' The core function of ar.burg (C++)
#'
#' @export
C_Burg <- function(x, order.max) {
    z <- .Call(stats:::C_Burg, x, order.max)
    list(
        "coefs" = matrix(z[[1L]], order.max, order.max),
        "var.pred1" = z[[2L]],
        "var.pred2" = z[[3L]]
    )
}

#' AR burg method
#'  This function does not return "asy.var.coef" which takes long computation time
#' when `order.max` is very large.
#'  Also, for calculating 'residual (resid)', custom embed function written in C++
#' will be used for minimizing the running time.
#'
#' @param ts         A time series (`ts`) object.
#' @param ic         A logical or character. If `ic=TRUE`, default AIC computation method
#'                   will be used. Or if `ic="AIC"`, or `ic="BIC"`, the given information
#'                   criteria will be used as its definition.
#' @param order.max  A numeric. The maximum order of AR. If `ic=FALSAE`, the model
#'                   of `order.max` will be chosen.
#' @param na.action  function to be called to handle missing values. Currently,
#'                   via na.action = na.pass, only Yule-Walker method can handle
#'                   missing values which must be consistent within a time point:
#'                   either all variables must be missing or none.
#' @param demean     should a mean be estimated during fitting?
#' @param series     names for the series. Defaults to deparse1(substitute(x)).
#' @param var.method A numeric (default: 2). the method to estimate the innovations variance (see ‘Details’).
#' @param ...	     additional arguments for specific methods.
#' @return A list of AR object.
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
    ...
) {
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
        ic.fun <- switch(
            ic,
            "AIC" = function(order.max, vars.pred, n.used) {
                2 * (0L:order.max) + n.used * log(vars.pred)
            },
            "BIC" = function(order.max, vars.pred, n.used) {
                (0L:order.max) * log(n.used) + n.used * log(vars.pred)
            },
            "FPE" = function(order.max, vars.pred, n.used) {
                (n.used + ((0L:order.max) + 1)) /
                    (n.used - ((0L:order.max) + 1)) *
                    vars.pred
            }
        )
        xic <- ic.fun(order.max, vars.pred, n.used)
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
    #if (order) {
    #    xacf <- acf(x, type = "covariance", lag.max = order,
    #                plot = FALSE)$acf
    #    res$asy.var.coef <- solve(toeplitz(drop(xacf)[seq_len(order)])) *
    #        var.pred/n.used
    #}

    class(res) <- "ar"

    res
}

#' Simple AR wrapper
#'
#' @param ts A time series (`ts`) object.
#' @param ... Additional arguments for 'ar'.
#' @return A list with `$res: ar(ts, ...)$resid` and `$AR: ar(ts, ...)` result itself.
#' @export
ar.resid <- function(ts, ...) {
    AR <- burgar(x = ts, ...)
    resid <- AR$resid %>% na.omit()
    return(list("res" = resid, "AR" = AR))
}

#' Plot AIC
#'
#' @param ar An AR object.
#' @return A ggplot object.
#' @export
plot.aic <- function(ar) {
    check.installed('latex2exp')

    ar.aic.df <- data.frame(p = seq(0, length(ar$aic) - 1), AIC = ar$aic)
    ar.order <- filter(ar.aic.df, p == ar$order)

    aic.plot <- ggplot(data = ar.aic.df, aes(x = p, y = AIC)) +
        geom_line(color = 'blue') +
        geom_point(aes(alpha = ""), data = ar.order, color = 'red', size = 2) +
        geom_vline(
            xintercept = ar.order$p,
            linetype = 'dashed',
            size = 1,
            color = 'red'
        ) +
        scale_alpha_manual(values = 1) +
        labs(x = 'AR orders', y = 'AIC') +
        theme_bw() +
        theme(
            legend.position = 'none',
            plot.margin = unit(c(0, 1, 1, 1), "lines")
        )

    plot.xlim <- c(
        ar.order$p - 3 - (ar.order$p %% 5),
        (ar.order$p + 5) - ((ar.order$p + 5) %% 5)
    )
    plot.ylim <- c(0, ar.order$AIC + 5)
    aic.plot.zoom <- aic.plot +
        labs(x = NULL, y = NULL) +
        coord_cartesian(xlim = plot.xlim, ylim = plot.ylim) +
        annotate(
            'text',
            x = ar.order$p + 0.2,
            y = plot.ylim[2] / 2,
            label = latex2exp::TeX(
                paste('p=', ar.order$p, sep = ''),
                italic = T,
                bold = T
            ),
            color = 'red',
            hjust = 0,
            size = 5
        ) +
        theme(legend.position = 'none')

    aic.plot.final <- aic.plot +
        geom_rect(
            aes(
                xmin = ar.order$p - 500,
                xmax = ar.order$p + 500,
                ymin = ar.order$AIC,
                ymax = plot.ylim[2] + ar.order$AIC
            ),
            color = "black",
            linetype = 'dashed',
            alpha = 0,
            size = 0.3
        ) +

        geom_segment(
            aes(
                x = ar.order$p - 500,
                xend = 5,
                y = ar.order$AIC,
                yend = 1e1 + ar.order$AIC
            ),
            linetype = 'dashed',
            size = 0.3
        ) +

        geom_segment(
            aes(
                x = ar.order$p - 500,
                xend = 500,
                y = plot.ylim[2] + ar.order$AIC,
                yend = 5e3 + ar.order$AIC
            ),
            linetype = 'dashed',
            size = 0.3
        ) +

        annotation_custom(
            grob = ggplotGrob(aic.plot.zoom),
            xmin = log10(5),
            xmax = log10(500),
            ymin = log10(1e1 + ar.order$AIC),
            ymax = log10(5e3 + ar.order$AIC)
        ) +

        geom_rect(
            aes(
                xmin = 5,
                xmax = 500,
                ymin = 1e1 + ar.order$AIC,
                ymax = 5e3 + ar.order$AIC
            ),
            color = "black",
            linetype = 'dashed',
            alpha = 0,
            size = 0.4
        ) +

        scale_x_continuous(expand = c(0, 0), trans = 'log10') +
        scale_y_continuous(expand = c(0, 0), trans = 'log10') +
        annotation_logticks()

    return(aic.plot.final)
}

#' Two-sided Moving Average (MA) smoother
#'
#' @param ts A time series (`ts`) object.
#' @param order An integer for the order of MA.
#' @param na.rm A logical whether remove NA values or not.
#' @return A MA smoothed time series (`ts`) object.
#' @export
ma <- function(ts, order, na.rm = T) {
    check.installed('forecast')

    out <- forecast::ma(ts, order, centre = T)
    if (na.rm) {
        out <- na.omit(out)
    }

    return(out)
}


#' Apply PCA
#'
#' This function preserves the sign of loadings
#'
#' @param x A data frame or a matrix.
#' @param retx A logical (default: TRUE). See 'prcomp'.
#' @param center A logical (default: FALSE). See 'prcomp'.
#' @param scale. A logical (default: FALSE). See 'prcomp'.
#' @param tol See 'prcomp'.
#' @param rank. See 'prcomp'.
#' @param ... Additional arguments for 'prcomp'.
#' @return PCA object with fixed loadings' sign.
#' @export
apply.pca <- function(
    x,
    retx = T,
    center = F,
    scale. = F,
    tol = NULL,
    rank. = NULL,
    ...
) {
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

#' Extract PC(s)
#'
#' @param x A data frame or a matrix.
#' @param pc A character or a vector. Which PCs need to be extract? Naming convension: "PC#" (e.g. PC1, PC23, ...).
#' @return A matrix or a vector.
#' @export
extract_pc <- function(x, pc = "PC1") {
    unname(apply.pca(na.omit(x))$x[, pc])
}

#' Ensemble of Averages (EoA)
#'
#' @param ts A time series (`ts`) object.
#' @param qs A vector of length 2 whose first value is lower bound of q and second is upper bound of q for 'ma'.
#' @param collector A character (default: "median"). A function name for collecting various MA models. (one of "mean", "median", or "pca)
#' @return A list. \code{$res}: A EoA smoothed time series (`ts`) object. `$df`: A data frame containing various MA(q) along the column.
#' @export
eoa <- function(ts, qs, collector = "median") {
    ret.list <- list()

    mas <- sapply(qs, function(q) ma(ts, q, na.rm = F)) |>
        ts(start = ti(ts), frequency = frequency(ts))
    colnames(mas) <- paste("q", qs, sep = "")

    res <- switch(
        collector,
        "pca" = extract_pc(mas) %>% tsfy(ref = na.omit(mas[, ncol(mas)])),
        "mean" = apply(mas, 1, mean) %>% tsfy(ref = mas[, 1]),
        "median" = apply(mas, 1, median) %>% tsfy(ref = mas[, 1])
    )
    attr(res, "collector") <- collector

    ret.list[["df"]] <- mas
    ret.list[["res"]] <- res
    return(ret.list)
}


#' Sequential ARIMA function
#'
#' (Difference -> AR -> MA -> band-pass)
#'
#' @param ts           A time series (`ts`) object.
#' @param d            An integer. The FIXED order of difference.
#' @param p            An integer. The FIXED order of AR.
#' @param q            An integer. The FIXED order of MA.
#' @param t.seg        A numeric (default: 0.5). A time length in each segment.
#' @param d_max        An integer (default: NULL). If d_max is given, the order d will not exceed d_max.
#' @param p_max        An integer. The maximum order of AR.
#' @param min.q        An integer (default: 1). The lower bound of q for MAC.
#' @param q_max        An integer. The upper bound of q for MAC.
#' @param fl           A numeric. The frequency lower bound for the band-pass filter.
#' @param fu           A numeric. The frequency upper bound for the band-pass filter.
#' @param ar.choose    A character (default: "residual"). Choose 'residual' or 'fitted'.
#' @param var.method   An integer (default: 1). The method to estimate the innovations variance for AR (See `ar.burg`)
#' @param ma.collector A character (default: "median"). A function name for collecting various MA models. (one of "mean", "median", or "pca)
#' @param return.mas   A logical (default: TRUE). Whether it returns a data frame 'mas' containing various MAs.
#' @param elap.t       A logical (default: FALSE). Whether it calculates elapsed time 'elap.t'.
#' @param verbose      A logical (default: TRUE). Whether it message out about its processing.
#' @return A list.
#'      `$x`: A original ts,
#'      `$out`: Final result ts,
#'      `$DD`: A list of intermediate result of difference,
#'      `$AR`: A list of intermediate result of AR,
#'      `$MA`: A ts of intermediate result of MA,
#'      `$EOA`: A ts of intermediate result of MAC,
#'      `$mas`: A data frame. Various MAs.
#'      `$NA.times`: A list with
#'          `$all` : time stamps whose values are NA,
#'          `$head`: time stamps whose values are NA at the head,
#'          `$tail`: time stamps whose values are NA at the tail,
#'      with attributes:
#'          `$d`: The optimal order of difference,
#'          `$p`: The optimal order of AR,
#'          `$q`: The order(s) of MA(C),
#'          `$ma.collector`: Used collector function for MAC,
#'          `$elap.t`: Elapsed time in second.
#'          `$suffix`: Final model name, e.g. "ARIMAC(10|1~10|1)"
#' @export
seqarima <- function(
    ts, # Input time series (ts object)
    d = NULL,
    p = NULL,
    q = NULL, # Sequential ARIMA with fixed p,q,d
    t.seg = .5, # These activate KPSS test for selecting d
    d_max = NULL, # Upper limit of d
    p_max = NULL, # AIC test is performed for selecting p
    min.q = 1,
    q_max = NULL, # These activate MAC (Moving Average Collection)
    fl = NULL,
    fu = NULL, # For Band-pass filter
    ar.choose = 'residual', # Choose 'residual' or 'fitted'
    var.method = 1, # The method to estimate the innovations variance for AR
    numCores = NULL, # Only for parallel computing in calculating AR residual. [Default: 4 cores (p_max<4000), 8 cores (p_max>4000)]
    ma.collector = "median", # PCA, median, mean
    return.mas = FALSE, # Returning option
    elap.t = FALSE, # Miscellaneous options
    verbose = TRUE # Miscellaneous options
) {
    # Argument matching
    ar.choose <- match.arg(ar.choose, c("fitted", "residual"))
    ma.collector <- match.arg(ma.collector, c("mean", "median", "pca"))

    if (elap.t) {
        check.installed('tictoc')
        tictoc::tic() # Start measuring execution time
    }

    message.verb("> Running seqarima...", v = verbose)

    # Check ts object
    if (!is.ts(ts)) {
        stop("Error) Input object type is not 'ts'")
    }

    # Some info
    source <- attr(ts, "source")
    sampling.freq <- frequency(ts)

    # Initializing
    return.list <- list()
    return.list[['x']] <- ts

    # Difference
    if (is.null(d)) {
        message.verb("> (1) Difference stage", v = verbose)
        diff.res <- auto.diff(
            ts,
            t.seg = t.seg,
            d_max = d_max,
            verbose = verbose
        )

        ts <- diff.res$out

        return.list[['out']] <- ts
        return.list[['DD']] <- diff.res
        d.order <- diff.res$d
        attr(return.list, 'd') <- d.order

        message.verb("|> d=", d.order, " is selected!", v = verbose)
    } else {
        if (d == 0) {
            d.order <- d
            attr(return.list, 'd') <- d.order
        } else {
            message.verb("> (1) Difference stage with d=", d, v = verbose)
            ts <- diff(ts, diff = d)
            return.list[['out']] <- ts
            return.list[['DD']] <- ts
            d.order <- d
            attr(return.list, 'd') <- d.order
        }
    }

    # Auto-Regressive (AR)
    if (is.null(p)) {
        p.order <- 0

        if ((p_max != 0) && !is.null(p_max)) {
            message.verb("> (2) AR stage", v = verbose)
            message.verb("|> Selecting p by AIC...", v = verbose)
            ar.res <- ar.resid(
                ts,
                ic = T,
                order.max = p_max,
                numCores = numCores
            )
            ar.obj <- ar.res$AR
            p.order <- ar.obj$order
            message.verb("|> p=", p.order, " is selected!", v = verbose)

            if (ar.choose == "fitted") {
                ts <- ts - ar.res$res
            } else {
                ts <- ar.res$res
            }
            message.verb(
                "|> ",
                ar.choose,
                " is chosen for a result.",
                v = verbose
            )

            return.list[['out']] <- ts
            return.list[['AR']] <- ar.obj
            return.list[["ar.choose"]] <- ar.choose
            attr(return.list, 'p') <- p.order
        }
    } else {
        message.verb("> (2) AR stage with p=", p, v = verbose)
        ar.res <- ar.resid(ts, ic = F, order.max = p)
        p.order <- ar.res$AR$order
        if (ar.choose == "fitted") {
            ts <- ts - ar.res$res
        } else {
            ts <- ar.res$res
        }
        message.verb("|> ", ar.choose, " is chosen for a result.", v = verbose)

        return.list[['out']] <- ts
        return.list[['AR']] <- ar.res$AR
        attr(return.list, 'p') <- p.order
    }

    # Moving Average (ma)
    if (is.null(q)) {
        q.order <- 0

        if ((q_max != 0) && !is.null(q_max)) {
            message.verb("> (3) EoA (Ensemble of Averages) stage", v = verbose)
            qs <- seq(min.q, q_max)
            eoa <- eoa(ts, qs, ma.collector)
            mas.df <- eoa$df
            eoa.res <- eoa$res
            attr(mas.df, "qs") <- qs

            if (return.mas) {
                return.list[['MAs']] <- mas.df
                message.verb("|> MAs data frame is also returned!", v = verbose)
            }

            ts <- eoa.res
            q.order <- paste(min.q, "~", q_max, sep = '')

            return.list[['out']] <- ts
            return.list[['EOA']] <- ts

            attr(return.list, 'q') <- q.order
            attr(return.list, 'ma.collector') <- ma.collector

            message.verb(
                "|> q=",
                q.order,
                " by a collector: ",
                ma.collector,
                v = verbose
            )
        }
    } else {
        message.verb("> (3) MA stage with q=", q, v = verbose)
        ts <- ma(ts, q)
        q.order <- q
        return.list[['out']] <- ts
        return.list[['MA']] <- ts
        attr(return.list, 'q') <- q.order
    }

    # Band-pass filter
    if ((fl != 0 || fu != 0) && (!is.null(fl) || !is.null(fu))) {
        message.verb("> (4) Pass filter stage", v = verbose)

        ts <- bandpass(ts, fl, fu, verb = verbose)
        attr(ts, "type") <- NULL
        attr(ts, "order") <- NULL
        return.list[['out']] <- ts

        attr(return.list, 'lower.freq') <- fl
        attr(return.list, 'upper.freq') <- fu
    }

    # Execution time measuring
    if (elap.t) {
        tctc <- tictoc::toc(quiet = T) # Finish measuring execution time
        attr(return.list, 'elap.t') <- (tctc$toc - tctc$tic)[[1]] # Store elap.t as an attribute
        message.verb("> ", tctc$callback_msg, v = verbose)
    }

    # Also return missing values caused by ARIMA
    return.list[['NA.times']] <- get.MissingValues(
        ts = return.list[['out']],
        ref = return.list[['x']]
    )

    # Additional attributes
    attr(return.list[['out']], 'model') <- paste(
        'ARIMA(',
        p.order,
        '|',
        q.order,
        '|',
        d.order,
        ')',
        sep = ''
    )
    attr(return.list[['out']], 'source') <- source

    return(return.list)
}

# SEQARIMA Ver. 2 ----
# Ensemble of AR
ear <- function(
    ts,
    ps = c(100, 500, 1000),
    aic = T,
    return.vec = T,
    return.var = T,
    return.mean = T,
    collector = 'median',
    ...
) {
    # Apply ARfeat over ps
    AR.fits <- lapply(ps, function(p) burgar(ts, ic = aic, order.max = p, ...))

    # Check duplicated orders exist
    order.lis <- sapply(AR.fits, function(x) x$order)
    AR.fits <- AR.fits[!duplicated(order.lis)]
    psel <- order.lis[!duplicated(order.lis)]
    names(order.lis) <- ps

    # Extracting residuals
    resid.lis <- lapply(AR.fits, function(arp) arp$resid)
    names(resid.lis) <- paste0('AR', psel)
    if (length(resid.lis) == 1) {
        resid.ens <- resid.lis[[1]]
        collector <- 'Not aggregated because selected orders are duplicated in given input orders'
    } else {
        resid.mts <- do.call('ts.intersect', resid.lis)
        resid.ens <- switch(
            collector,
            pca = extract_pc(resid.mts) |>
                tsfy(ref = na.omit(resid.mts[, ncol(resid.mts)])),
            mean = apply(resid.mts, 1, mean) |> tsfy(ref = resid.mts[, 1]),
            median = apply(resid.mts, 1, median) |> tsfy(ref = resid.mts[, 1])
        )
    }

    # Extracting features (coefficients, variance (option), mean (option))
    feat.lis <- lapply(AR.fits, function(arp) {
        ret <- c('ar' = arp$ar)
        if (return.var) {
            ret <- c(ret, 'var' = arp$var.pred)
        }
        if (return.mean) {
            ret <- c(ret, 'mean' = arp$x.mean)
        }
        ret
    })
    feat.lis
    # Naming features
    for (i in seq_along(psel)) {
        p <- psel[[i]]
        if (p == 0) {
            newname <- NULL
        } else {
            prefix <- paste0('ar', p)
            newname <- paste0(prefix, '_', 1:p) #sum(str_detect(names(feat.lis[[i]]), '^ar')))
        }

        if (return.var) {
            newname <- c(newname, paste0(prefix, '_var'))
        }
        if (return.mean) {
            newname <- c(newname, paste0(prefix, '_mean'))
        }
        names(feat.lis[[i]]) <- newname
    }

    if (return.vec) {
        feat.lis <- unlist(feat.lis)
    }

    attr(resid.ens, 'collector') <- collector
    list(
        'res' = resid.ens,
        'feature' = feat.lis,
        'p.order' = psel,
        'selected' = order.lis
    )
}

# Differencing
differencing <- function(ts, d = NULL, t.seg = 0.5, kpss = T, verbose = T) {
    if (is.null(d)) {
        # Automatic selection of d
        message.verb("> (1) Difference stage", v = verbose)
        diff.res <- auto.diff(ts, t.seg = t.seg, verbose = verbose)
        message.verb("|> d=", diff.res$d, " is selected!", v = verbose)

        out <- diff.res$out
        DD <- diff.res
        d.order <- diff.res$d
    } else if (d == 0) {
        out <- ts
        DD <- NULL
        d.order <- 0
    } else {
        if (kpss) {
            # d_max limited auto.diff
            message.verb("> (1) Difference stage with d_max=", d, v = verbose)
            diff.res <- auto.diff(
                ts,
                t.seg = t.seg,
                d_max = d,
                verbose = verbose
            )
            message.verb("|> d=", diff.res$d, " is selected!", v = verbose)

            out <- diff.res$out
            DD <- diff.res
            d.order <- diff.res$d
        } else {
            # Specific d
            message.verb("> (1) Difference stage with d=", d, v = verbose)
            ts <- diff(ts, diff = d)

            out <- ts
            DD <- NULL
            d.order <- d
        }
    }
    list("out" = out, "DD" = DD, "d.order" = d.order)
}


# Autoregressive model fitting
autoregressive <- function(ts, p = NULL, aic = T, verbose = TRUE, ...) {
    if (length(p) > 1) {
        # length(p) > 1 : Ensemble of AR (EAR)
        message.verb(
            "> (2) EAR stage with p={",
            paste(p, collapse = ','),
            "}",
            v = verbose
        )
        ar.fit <- ear(ts, ps = p, aic = aic, ...)
        p.order <- ar.fit$p.order
        message.verb(
            "|> p={",
            paste(p.order, collapse = ', '),
            "} is selected and aggregated by a collector: ",
            attr(ar.fit$res, 'collector'),
            v = verbose
        )
    } else {
        # length(p) == 1 : Single AR
        message.verb("> (2) AR stage with p_max=", p, v = verbose)
        ar.res <- ar.resid(ts, ic = aic, order.max = p, ...)
        p.order <- ar.res$AR$order
        feature <- c('ar' = ar.res$AR$ar)
        ar.fit <- list(
            'res' = ar.res$res,
            'feature' = feature,
            'p.selected' = p.order
        )
        message.verb("|> p=", p.order, " is selected!", v = verbose)
    }
    list("AR" = ar.fit, "p.order" = p.order)
}


# Moving-Average (Ensemble of Average)
movingaverage <- function(ts, q = NULL, verbose = TRUE, ...) {
    if (length(q) > 1) {
        # length(p) > 1 : Ensemble of Average (EoA)
        message.verb("> (3) EoA (Ensemble of Averages) stage", v = verbose)
        eoa <- eoa(ts, q, ...)
        out <- eoa$res
        q.order <- q
        message.verb(
            "|> q={",
            paste(q.order, collapse = ','),
            "} by a collector: ",
            attr(out, "collector"),
            v = verbose
        )
    } else {
        # length(q) == 1 : Single MA
        message.verb("> (3) MA stage with q=", q, v = verbose)
        out <- ma(ts, q)
        q.order <- q
        message.verb("|> q=", q.order, v = verbose)
    }
    list("x_eoa" = out, "q.order" = q.order)
}

#' Sequential ARIMA function
#'
#' (Difference -> AR -> MA -> band-pass)
#'
#' @param ts           A time series (`ts`) object.
#' @param d            An integer. The order of difference.
#' @param p            An integer. The order of AR.
#' @param q            An integer. The order of MA.
#' @param fl           A numeric. The frequency lower bound for the band-pass filter.
#' @param fu           A numeric. The frequency upper bound for the band-pass filter.
#' @param ar.aic       A logical (default: TRUE). Whether p is selected by AIC or fixed.
#' @param ar.collector A character (default: "median"). A function name for collecting various AR residuals. (one of "mean", "median", or "pca")
#' @param ma.collector A character (default: "median"). A function name for collecting various MA models. (one of "mean", "median", or "pca")
#' @param return.every A logical (default: FALSE). Whether returns ts of every stage.
#' @param elap.t       A logical (default: FALSE). Whether it calculates elapsed time 'elap.t'.
#' @param verbose      A logical (default: TRUE). Whether it message out about its processing.
#' @return A list.
#'      `$x`: A original ts,
#'      `$out`: Final result ts,
#'      `$DD`: A list of intermediate result of difference,
#'      `$AR`: A list of intermediate result of AR,
#'      `$MA`: A ts of intermediate result of MA,
#'      `$EOA`: A ts of intermediate result of EoA,
#'      `$NA.times`: A list with
#'          `$all` : time stamps whose values are NA,
#'          `$head`: time stamps whose values are NA at the head,
#'          `$tail`: time stamps whose values are NA at the tail,
#'      with attributes:
#'          `$d`: The optimal order of difference,
#'          `$p`: The optimal order of AR,
#'          `$q`: The order(s) of MA(C),
#'          `$ar.collector`: Used collector function for EAR,
#'          `$ma.collector`: Used collector function for EoA,
#'          `$elap.t`: Elapsed time in second.
#'          `$suffix`: Final model name, e.g. "ARIMAC(10|1~10|1)"
#' @export
seqarima.new <- function(
    ts, # Input time series (ts object)
    d = NULL,
    p = NULL,
    q = NULL, # p, q, d
    fl = NULL,
    fu = NULL, # For Band-pass filter
    ar.aic = TRUE, # Select AR model by AIC
    ar.collector = "median", # PCA, median, mean
    ma.collector = "median", # PCA, median, mean
    return.every = FALSE, #
    elap.t = FALSE, # Miscellaneous options
    verbose = TRUE # Miscellaneous options
) {
    # Argument matching
    ar.collector <- match.arg(ar.collector, c("mean", "median", "pca"))
    ma.collector <- match.arg(ma.collector, c("mean", "median", "pca"))

    if (elap.t) {
        check.installed('tictoc')
        tictoc::tic() # Start measuring execution time
    }

    message.verb("> Running seqarima...", v = verbose)

    # Check ts object
    if (!is.ts(ts)) {
        stop("Error) Input object type is not 'ts'")
    }

    # Initializing
    sampling.freq <- frequency(ts)
    return.list <- list()
    return.list[['x']] <- ts

    # 1) Differencing
    diff.res <- differencing(ts, d = d, verbose = verbose)
    x_d <- diff.res$out
    if (return.every) {
        return.list[['steps']][['DD']] <- diff.res$DD
    }
    attr(return.list, 'd') <- diff.res$d.order
    out <- x_d

    # 2) Auto-Regressive (EAR)
    if (!is.null(p)) {
        ar.res <- autoregressive(
            ts = out,
            p = p,
            aic = ar.aic,
            verbose = verbose,
            collector = ar.collector
        )
        x_r <- ar.res$AR$res
        if (return.every) {
            return.list[['steps']][['AR']] <- ar.res$AR
        }
        return.list[['feature']] <- ar.res$AR$feature
        attr(return.list, 'p') <- ar.res$AR$p.order
        out <- x_r
    }

    # 3) Moving Average (EoA)
    if (!is.null(q)) {
        ma.res <- movingaverage(
            ts = out,
            q = q,
            verbose = verbose,
            collector = ma.collector
        )
        x_eoa <- ma.res$x_eoa
        if (return.every) {
            return.list[['steps']][['MA']] <- x_eoa
        }
        attr(return.list, 'q') <- ma.res$q.order
        out <- x_eoa
    }

    # 4) Band-pass filter
    if ((fl != 0 || fu != 0) && (!is.null(fl) || !is.null(fu))) {
        message.verb("> (4) Pass filter stage", v = verbose)

        x_bp <- bandpass(out, fl, fu, verb = verbose)
        attr(x_bp, "type") <- NULL
        attr(x_bp, "order") <- NULL
        if (return.every) {
            return.list[['steps']][['BP']] <- x_bp
        }
        attr(return.list, 'lower.freq') <- fl
        attr(return.list, 'upper.freq') <- fu
        out <- x_bp
    }
    return.list[['out']] <- out

    # Execution time measuring
    if (elap.t) {
        tctc <- tictoc::toc(quiet = T) # Finish measuring execution time
        attr(return.list, 'elap.t') <- (tctc$toc - tctc$tic)[[1]] # Store elap.t as an attribute
        message.verb("> ", tctc$callback_msg, v = verbose)
    }

    # Also return missing values caused by ARIMA
    return.list[['NA.times']] <- get.MissingValues(
        ts = return.list[['out']],
        ref = return.list[['x']]
    )

    return(return.list)
}

# ETC ----
#' Reinstall beacon
#'
#' @param beacon.path A character. A beacon package path.
#' @return reloading
#' @export
reinstall.beacon <- function(beacon.path = "~/projects/GW/beacon") {
    library(devtools)
    detach(name = package:beacon, unload = T)
    message("1) Removing 'beacon'...")
    remove.packages("beacon")

    message("2) Checking 'beacon'...")
    checked <- check(beacon.path)

    if (length(checked$errors) == 0) {
        message("3) Re-installing 'beacon'...")
        install(beacon.path)
    } else {
        cat(checked$errors)
        stop("\nPlease check output")
    }
}

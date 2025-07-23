#' Read H5 file
#'
#' @param file          A character. A file path to read.
#' @param sampling.freq A numeric. A sampling frequency.
#' @return A ts object.
#' @export
read_H5 <- function(file, sampling.freq, dq.level = "BURST_CAT2") {
    tmp <- hdf5r::h5file(filename = file, mode = "r")
    tstart <- tmp[["meta"]]$open(name = "GPSstart")$read()
    Data <- tmp[["strain"]]$open(name = "Strain")$read()
    dqmask <- tmp[['quality']][['simple']][['DQmask']]$read()
    tmp$close_all()
    rm(tmp)

    dqmask <- if (dq.level == 'all') {
        ts(
            t(sapply(dqmask, DQlev, level = dq.level)),
            start = tstart,
            frequency = 1
        )
    } else {
        ts(
            sapply(dqmask, DQlev, level = dq.level),
            start = tstart,
            frequency = 1
        )
    }
    attr(dqmask, 'level') <- dq.level

    res <- ts(Data, start = tstart, frequency = sampling.freq)
    attr(res, 'dqmask') <- dqmask
    return(res)
}


# Data Quality ----
DQ_Dec2Bits <- function(decimal.dq, len = 7) {
    as.integer(intToBits(decimal.dq))[1:len]
}
DQ_ShortNames <- function() {
    c(
        'DATA' = 0L,
        'CBC_CAT1' = 1L,
        'CBC_CAT2' = 2L,
        'CBC_CAT3' = 3L,
        'BURST_CAT1' = 4L,
        'BURST_CAT2' = 5L,
        'BURST_CAT3' = 6L
    )
}
DQlev <- function(dq, level = 'BURST_CAT2') {
    if (level == 'all') {
        ret <- DQ_Dec2Bits(dq)
        names(ret) <- names(DQ_ShortNames())
    } else {
        ret <- bitwShiftR(dq, DQ_ShortNames()[level]) |> bitwAnd(1)
    }
    return(ret)
}
read_DQ <- function(file, dq.level = "BURST_CAT2") {
    tmp <- hdf5r::h5file(filename = file, mode = "r")
    tstart <- tmp[["meta"]]$open(name = "GPSstart")$read()
    dqmask <- tmp[['quality']][['simple']][['DQmask']]$read()
    tmp$close_all()
    rm(tmp)
    if (dq.level == 'all') {
        ts(
            t(sapply(dqmask, DQlev, level = dq.level)),
            start = tstart,
            frequency = 1
        )
    } else {
        ts(
            sapply(dqmask, DQlev, level = dq.level),
            start = tstart,
            frequency = 1
        )
    }
}

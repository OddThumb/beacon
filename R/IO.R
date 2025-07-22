
#' Read H5 file
#'
#' @param file          A character. A file path to read.
#' @param sampling.freq A numeric. A sampling frequency.
#' @return A ts object.
#' @export
read.H5 <- function(file, sampling.freq, dq.level="BURST_CAT2") {
    check.installed("hdf5r")
    tmp <- hdf5r::h5file(filename = file, mode = "r")
    tstart <- tmp[["meta"]]$open(name = "GPSstart")$read()
    Data <- tmp[["strain"]]$open(name = "Strain")$read()
    dqmask <- tmp[['quality']][['simple']][['DQmask']]$read()
    tmp$close_all()
    rm(tmp)
    
    dqmask <- if(dq.level=='all') {
        ts(t(sapply(dqmask, DQlev, level=dq.level)), start=tstart, frequency=1)
    } else {
        ts(sapply(dqmask, DQlev, level=dq.level), start=tstart, frequency=1)
    }
    attr(dqmask, 'level') <- dq.level
    
    res <- ts(Data, start = tstart, frequency = sampling.freq)
    attr(res, 'dqmask') <- dqmask
    return(res)
}


#' Write Cleaned Data with a H5 file
#'
#' @param object A list. A result from "seqarima" (Not an object 'sequential.arima').
#' @param file   A character. A file path to save.
#' @param source A character. An event name.
#' @param det    A character. A detector name.
#' @export
write.H5 <- function(object, file, source, det) {
    check.installed("hdf5r")
    
    # Creating a h5 file
    file.h5 <- h5file(file, mode='w')
    
    # Creating groups
    meta.grp <- file.h5$create_group('meta')
    strain.grp <- file.h5$create_group('strain')
    
    # Storing meta info
    meta.grp[['sampling_rate']] <- frequency(object$out)
    meta.grp[['duration']] <- time(object$out)[length(object$out)]-time(object$out)[1]
    meta.grp[['detector']] <- det
    meta.grp[['source']]   <- source
    meta.grp[['cleaning_method']] <- "seqARIMA"
    namelist <- list('p'='p_order','q'='q_order','d'='d_order','lower.freq'='freq_lo','upper.freq'='freq_up')
    
    for (nm in names(namelist)) {
        meta.grp[[ namelist[[nm]] ]] <- attributes(object)[[nm]]
    }
    
    # Storing strain data with time stamp
    strain.grp[['strain']] <- object$out
    strain.grp[['time']] <- time(object$out)
    
    # Closing
    file.h5$close_all()
}

# Data Quality ----
DQ_Dec2Bits <- function(decimal.dq, len=7) {
    as.integer(intToBits(decimal.dq))[1:len]
}
DQ_ShortNames <- function() {
    c('DATA' = 0,
      'CBC_CAT1'   = 1, 'CBC_CAT2'   = 2, 'CBC_CAT3'   = 3,
      'BURST_CAT1' = 4, 'BURST_CAT2' = 5, 'BURST_CAT3' = 6)
}
DQlev <- function(dq, level='BURST_CAT2') {
    if (level == 'all') {
        ret <- DQ_Dec2Bits(dq)
        names(ret) <- names(DQ_ShortNames())
    } else {
        ret <- bitwShiftR(dq, DQ_ShortNames()[level]) |> bitwAnd(1)
    }
    return(ret)
}
read.DQ <- function(file, dq.level="BURST_CAT2") {
    check.installed("hdf5r")
    tmp <- hdf5r::h5file(filename = file, mode = "r")
    tstart <- tmp[["meta"]]$open(name = "GPSstart")$read()
    dqmask <- tmp[['quality']][['simple']][['DQmask']]$read()
    tmp$close_all()
    rm(tmp)
    if (dq.level == 'all') {
        ts(t(sapply(dqmask, DQlev, level = dq.level)), start = tstart, frequency = 1)
    } else {
        ts(sapply(dqmask, DQlev, level = dq.level), start = tstart, frequency = 1)
    }
}

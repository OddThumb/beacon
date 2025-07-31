#' Read HDF5 Time Series Data with Data Quality Mask
#'
#' Reads a single-channel strain data from an HDF5 file along with its DQ mask.
#'
#' @param file A character string. Path to the HDF5 file.
#' @param sampling.freq A numeric scalar. Sampling frequency (Hz) of the strain data.
#' @param dq.level A character string. DQ level to extract (default: "BURST_CAT2").
#'                 Use \code{"all"} to extract all DQ bits.
#'
#' @return A \code{ts} object containing the strain data with the following attributes:
#' \describe{
#'   \item{\code{dqmask}}{A \code{ts} object of the same length representing the DQ mask.}
#' }
#'
#' @details
#' This function assumes the HDF5 file has the structure: \code{meta/GPSstart},
#' \code{strain/Strain}, and \code{quality/simple/DQmask}.
#' The extracted strain data will carry \code{dqmask} as an attribute.
#'
#' @export
read_H5 <- function(file, sampling.freq, dq.level = "BURST_CAT2") {
    tmp <- hdf5r::h5file(filename = file, mode = "r")
    tstart <- tmp[["meta"]]$open(name = "GPSstart")$read()
    Data <- tmp[["strain"]]$open(name = "Strain")$read()
    res <- ts(Data, start = tstart, frequency = sampling.freq)

    if (!is.null(dq.level)) {
        dqmask <- tmp[["quality"]][["simple"]][["DQmask"]]$read()
        dqmask <- if (dq.level == "all") {
            dqm <- ts(
                t(sapply(dqmask, DQlev, level = dq.level)),
                start = tstart,
                frequency = 1
            )
            class(dqm) <- c("matrix", "array")
        } else {
            dqm <- ts(
                sapply(dqmask, DQlev, level = dq.level),
                start = tstart,
                frequency = 1
            )
            class(dqm) <- "numeric"
        }
        attr(dqmask, "level") <- dq.level
        attr(res, "dqmask") <- dqmask
    }
    tmp$close_all()

    return(res)
}

#' Convert Decimal DQ Flag to Bit Representation
#'
#' @param decimal.dq An integer. Decimal representation of the DQ flag.
#' @param len An integer (default: 7). Number of bits to extract.
#'
#' @return An integer vector of length \code{len} containing bitwise flags.
#'
#' @export
DQ_Dec2Bits <- function(decimal.dq, len = 7) {
    as.integer(intToBits(decimal.dq))[1:len]
}

#' List of DQ Bit Names with Their Bit Positions
#'
#' @return An integer-named character vector indicating short names and their bit index.
#'
#' @details
#' The default mapping is:
#' \itemize{
#'   \item \code{DATA}       = 0
#'   \item \code{CBC_CAT1}   = 1
#'   \item \code{CBC_CAT2}   = 2
#'   \item \code{CBC_CAT3}   = 3
#'   \item \code{BURST_CAT1} = 4
#'   \item \code{BURST_CAT2} = 5
#'   \item \code{BURST_CAT3} = 6
#' }
#'
#' @export
DQ_ShortNames <- function() {
    c(
        "DATA" = 0L,
        "CBC_CAT1" = 1L,
        "CBC_CAT2" = 2L,
        "CBC_CAT3" = 3L,
        "BURST_CAT1" = 4L,
        "BURST_CAT2" = 5L,
        "BURST_CAT3" = 6L
    )
}

#' Extract Specific DQ Level from Decimal Flag
#'
#' @param dq An integer. Decimal DQ flag.
#' @param level A character string. DQ level to extract (e.g., \code{"BURST_CAT2"} or \code{"all"}).
#'
#' @return
#' If \code{level = "all"}, returns a named integer vector of bit flags.\cr
#' Otherwise, returns a scalar (0 or 1) indicating the presence of the selected DQ level.
#'
#' @export
DQlev <- function(dq, level = "BURST_CAT2") {
    if (level == "all") {
        ret <- DQ_Dec2Bits(dq)
        names(ret) <- names(DQ_ShortNames())
    } else {
        ret <- bitwAnd(bitwShiftR(dq, DQ_ShortNames()[level]), 1)
    }
    return(ret)
}

#' Read Only the DQ Mask from HDF5 File
#'
#' @param file A character string. Path to the HDF5 file.
#' @param dq.level A character string. DQ level to extract (default: "BURST_CAT2").
#'                 Use \code{"all"} to extract all DQ bits.
#'
#' @return A \code{ts} object representing the DQ mask.
#'
#' @import hdf5r
#' @export
read_DQ <- function(file, dq.level = "BURST_CAT2") {
    tmp <- hdf5r::h5file(filename = file, mode = "r")
    tstart <- tmp[["meta"]]$open(name = "GPSstart")$read()
    dqmask <- tmp[["quality"]][["simple"]][["DQmask"]]$read()
    tmp$close_all()
    rm(tmp)
    if (dq.level == "all") {
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

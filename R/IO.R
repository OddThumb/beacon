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
            dqm
        } else {
            dqm <- ts(
                sapply(dqmask, DQlev, level = dq.level),
                start = tstart,
                frequency = 1
            )
            class(dqm) <- "numeric"
            dqm
        }
        attr(dqmask, "level") <- dq.level
        attr(res, "dqmask") <- dqmask
    }
    tmp$close_all()

    return(res)
}


#' Write a time series and metadata into HDF5 format
#'
#' @param file Path to output HDF5 file.
#' @param tsobj A time series object of class `ts` (e.g., output of `ts()`).
#' @param meta.list A named list of additional metadata to store.
#'        Names should be full HDF5 paths (e.g., "quality/simple/DQmask").
#'
#' @return Invisibly returns the HDF5 file path.
#' @export
write_H5 <- function(file, tsobj, meta.list = NULL) {
    stopifnot(inherits(tsobj, "ts"))

    # Extract data and time reference
    y <- as.numeric(tsobj)
    start_time <- start(tsobj)[1] # only supports regular sampling
    fs <- frequency(tsobj)

    # Open HDF5 for writing
    h5 <- hdf5r::H5File$new(filename = file, mode = "w")

    # Write strain data
    h5$create_group("strain")
    h5[["strain"]]$create_dataset("Strain", robj = y)

    # Write meta data (GPSstart)
    h5$create_group("meta")
    h5[["meta"]]$create_dataset("GPSstart", robj = start_time)

    # Optional: write additional metadata
    if (!is.null(meta.list)) {
        for (path in names(meta.list)) {
            # Ensure the group exists
            parts <- strsplit(path, "/")[[1]]
            groups <- parts[-length(parts)]
            final_name <- parts[length(parts)]
            current <- h5
            for (g in groups) {
                if (!g %in% names(current)) current$create_group(g)
                current <- current[[g]]
            }
            # Write dataset
            current$create_dataset(final_name, robj = meta.list[[path]])
        }
    }

    h5$close_all()
    invisible(file)
}

#' Reconstruct dqmask as a ts or mts object
#'
#' Converts the `dqmask` metadata attribute stored in an object (typically removed
#' from `ts` class during storage) back to a formal time‑series (`ts` or `mts`) object.
#' Uses the stored `tsp` attribute (or fallback to the parent object’s `tsp`)
#' to reconstruct the appropriate start, end, and frequency information.
#'
#' @param obj An R object (e.g. returned by \code{read_H5()}) containing
#'   an attribute named \code{"dqmask"}, which is either a numeric vector
#'   or a numeric matrix/array (multi‑series).
#'
#' @returns A \code{ts} object for univariate data or \code{mts} object for
#'   multivariate data, reconstructed using \code{tsp} and with
#'   the original \code{"level"} attribute re‑attached. Returns \code{NULL}
#'   if \code{dqmask} is missing or \code{NULL}.
#'
#' @details
#' This helper retrieves \code{dqmask} from \code{obj}, determines the appropriate
#' time-series parameters via \code{tsp(obj)} or \code{tsp(dqmask)}, and then recreates
#' the time-series using \code{\link[stats]{ts}()}. The original
#' \code{"level"} attribute (e.g. \code{"all"} vs specific level) is preserved.
#'
#' @examples
#' # Example for univariate case:
#' res <- read_H5("example.h5", sampling.freq = 1000, dq.level = "CAT1")
#' dq_ts <- get_dqmask(res)
#' print(dq_ts)
#'
#' # Example for multivariate case:
#' res2 <- read_H5("example_multi.h5", sampling.freq = 1000, dq.level = "all")
#' dq_ts2 <- get_dqmask(res2)
#' str(dq_ts2)
#'
#' @export
get_dqmask <- function(obj) {
    dq <- attr(obj, "dqmask", exact = TRUE)
    if (is.null(dq)) {
        return(NULL)
    }

    tsp_info <- attr(dq, "tsp", exact = TRUE)
    if (is.null(tsp_info)) {
        tsp_info <- attr(obj, "tsp", exact = TRUE)
    }
    if (!is.numeric(tsp_info) || length(tsp_info) != 3) {
        stop("tsp attribute (start, end, frequency) missing or invalid")
    }

    data_vec <- dq
    if (is.matrix(data_vec)) {
        # multivariate case
        ts_obj <- ts(data_vec, start = tsp_info[1], frequency = tsp_info[3])
    } else {
        # univariate case
        ts_obj <- ts(data_vec, start = tsp_info[1], frequency = tsp_info[3])
    }
    attr(ts_obj, "level") <- attr(dq, "level", exact = TRUE)
    ts_obj
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

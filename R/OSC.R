#' Retrieve metadata from GWOSC
#'
#' Download or load offline metadata table of gravitational wave events from GWOSC.
#'
#' @param offline Logical. If TRUE, read metadata from a local CSV file instead of querying online.
#' @param csvpath Character. Required if `offline = TRUE`; path to the local CSV file.
#' @return A data.frame containing metadata of events.
#' @export
get_gwosc <- function(
    offline = F,
    csvpath = NULL
) {
    # If you are in off-line circumstance, you need pre-downloaded csv query csv file
    if (offline) {
        stopifnot(!is.null(csvpath))
        return.obj <- read.csv(csvpath)
    } else {
        check.installed(c("curl", "request"))
        # Check internet
        if (!curl::has_internet()) {
            stop("No internet connection")
        } else {
            message("> Loading from GWOSC...")
        }

        # Try to get information online
        # Somehow, around 6 PM (KST), GWOSC server gets some maintaining time.
        base_url <- "https://www.gw-openscience.org"

        # All events info will be stored as a data frame
        full_url <- paste(base_url, "/eventapi/jsonfull/query/show", sep = '')
        query.list <- suppressMessages({
            full_url |>
                request::api() |>
                request::http()
        })
        query.df <- dplyr::bind_rows(query.list$events, .id = "commonName") |>
            tidyr::separate(commonName, c("commonName", "v"), "-")

        query.df_strain <- rlist::list.rbind(lapply(
            query.df$strain,
            rlist::list.cbind
        ))
        return.obj <- dplyr::bind_cols(query.df, query.df_strain) |>
            dplyr::relocate(colnames(query.df_strain), .before = strain) |>
            dplyr::select(-strain)
    }
    return(return.obj)
}

#' Extract specific parameters from GWOSC metadata
#'
#' @param gwosc.list A data.frame returned by `get_gwosc()`.
#' @param source.names Character vector. Names of the events (`commonName`) to extract.
#' @param param Character or character vector. Column name(s) to extract.
#' @return A vector or data.frame depending on the number of requested parameters.
#' @export
get_gwosc_param <- function(gwosc.list, source.names, param) {
    step1 <- gwosc.list |>
        dplyr::filter(commonName %in% source.names) |>
        dplyr::arrange(dplyr::desc(version)) |>
        dplyr::distinct(commonName, .keep_all = T)
    if (length(param) > 1) {
        step1 |> dplyr::select(param)
    } else {
        step1 |> dplyr::pull(param)
    }
}

#' Download strain data from GWOSC
#'
#' Downloads a specified strain data file for a given GW event and detector.
#'
#' @param destfile Character. Path to save the downloaded file (default: "/tmp/tmp.hdf5").
#' @param event_name Character. Name of the event (e.g., "GW150914").
#' @param det Character. Detector name (e.g., "H1", "L1").
#' @param dur Integer. Duration of the data segment in seconds (default: 32).
#' @param file.format Character. File format ("gwf", "hdf5", or "txt", default: "hdf5").
#' @param sampling.freq Numeric. Desired sampling frequency (default: 4096).
#' @param version Character. Version label (default: "latest").
#' @param timeout Integer. Timeout in seconds for download (default: 300).
#' @param load Logical. If TRUE, load the HDF5 data directly into memory as a `ts` object (default: FALSE).
#' @param remove Logical. If TRUE and `load=TRUE`, remove the downloaded file after loading (default: FALSE).
#' @param direct.url Character. Optional direct URL for downloading the file.
#' @return If `load=TRUE`, returns a `ts` object of the strain data. Otherwise, NULL.
#' @export
download_gwosc <- function(
    destfile = "/tmp/tmp.hdf5",
    event_name,
    det,
    dur = 32,
    file.format = "hdf5",
    sampling.freq = 4096,
    version = "latest",
    timeout = 300, # If internet connection is poor, increase this.
    load = FALSE,
    remove = FALSE,
    direct.url = NULL
) {
    # Check internet
    if (!curl::has_internet()) {
        stop("No internet connection")
    }

    # Change timeout option to prevent interupting download.
    original.timeout <- getOption("timeout")
    options(timeout = max(timeout, original.timeout))

    if (is.null(direct.url)) {
        # Step 1: Get gwosc list
        step1 <- get_gwosc()

        # Step 2: Filter by commonName
        step2 <- step1 |> dplyr::filter(commonName == event_name)

        # Step 3: Choose version
        if (version == "latest") {
            step3 <- step2 |>
                dplyr::arrange(dplyr::desc(version)) |> # [[ LATEST ]]
                dplyr::distinct(
                    commonName,
                    detector,
                    sampling_rate,
                    duration,
                    format,
                    .keep_all = T
                )
        } else {
            if (!(version %in% step2$version)) {
                stop(paste0(
                    "No version=",
                    version,
                    " for event_name=",
                    event_name
                ))
            }
            step3 <- step2 |>
                dplyr::filter(version == version)
        }

        # Step 4: Filter by detector
        if (!(det %in% step3$detector)) {
            stop(paste(
                "No det=",
                det,
                " for event_name=",
                event_name,
                " and version=",
                version
            ))
        }
        step4 <- step3 |> dplyr::filter(detector == det)

        # Step 5: Filter by dur, file.format, sampling.freq
        step5 <- step4 |>
            dplyr::filter(duration == dur) |>
            dplyr::filter(format == file.format) |>
            dplyr::filter(sampling_rate == sampling.freq)

        # Step 6: Get url
        file.url <- step5$url

        # Downloading...
        message("> Downloading a file to: ", destfile)
        download.file(url = file.url, destfile = destfile, quiet = F)
    } else {
        download.file(url = direct.url, destfile = destfile, quiet = F)
    }

    # Timeout option back to normal
    options(timeout = original.timeout)

    # If you want to load the data directly into your R environment,
    #   change 'load=TRUE' and assign it like: 'data <- download.gwosc(...)'.
    if (load) {
        stopifnot(file.format == "hdf5")
        tmp <- hdf5r::h5file(filename = destfile, mode = 'r')
        tstart <- tmp[['meta']]$open(name = 'GPSstart')$read()
        Data <- tmp[['strain']]$open(name = 'Strain')$read()
        tmp$close_all()
        rm(tmp)

        Data <- ts(Data, start = tstart, frequency = sampling.freq)
        attr(Data, "source.name") <- event_name

        # If 'remove' is requested (TRUE), it will automatically remove 'destfile'
        if (remove) {
            file.remove(destfile)
            message("> ", destfile, " is removed")
        }
        return(Data)
    }
}

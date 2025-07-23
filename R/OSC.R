# GWOSC API ----

#' Get information from GWOSC!
#'
#' @param event_name A character (default: "ALL).
#' @param offline A logical (default: FALSE). Whether it searchs online or gets information from pre-downloaded csv file offline.
#' @param csvpath A character. A pre-downloaded csv file path. If 'offline=TRUE', this need to be given.
#' @return A data frame.
#' @export
get_gwosc <- function(
    #event_name="ALL", (DEPRECATED FOR A MOMENT)
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
        # Somehow, around 6 PM (KST), GWOSC server has some maintaining time.
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

#' Get GWTC parameter from the return of get.gwosc()
#'
#' @param gwossc.list A data.frame. The return object from get.gwosc()
#' @param source.names A character vector.
#'                     Source names need to follow the commonName convention.
#'                     It can be either one character or a vector of source.names.
#' @param param A character vector.
#'              Parameters that want to get.
#'              It can be either one character or a vector of source.names.
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
#' @param destfile      A character (default: "/tmp/tmp.hdf5"). A file name to download.
#' @param event_name    A character. A specific event name.
#' @param detector      A character. A specific detector name.
#' @param dur           An integer (default: 32). A duration in second of the data.
#' @param file.format   A character (default: "hdf5"). A file format. (one of "gwf", "hdf5", or "txt")
#' @param sampling.freq A numeric (default: 4096). A sampling frequency of the data.
#' @param timeout       An integer (default: 300). To extend timeout for downloading.
#' @param load          A logical (default: FALSE). Whether it assigns the data directly into the R environment. 'file.format' must be "hdf5".
#' @param remove        A logical (default: FALSE). Whether it removes data file after loading.
#' @param direct.url    A character (default: NULL). The file URL can be directly given.
#' @return A list. $Data: A vector of strain, $tstart: A numeric of GPS start time, if 'load=TRUE'.
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
        step1 <- get.gwosc()

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

#' Retrieve metadata from GWOSC
#'
#' Download or load offline metadata table of gravitational wave events from GWOSC.
#'
#' @param offline Logical. If TRUE, read metadata from a local CSV file instead of querying online.
#' @param csvpath Character. Required if `offline = TRUE`; path to the local CSV file.
#' @return A data.frame containing metadata of events.
#'
#' @export
get_gwosc <- function(
    offline = F,
    csvpath = NULL) {
    # If you are in off-line circumstance, you need pre-downloaded csv query csv file
    if (offline) {
        stopifnot(!is.null(csvpath))
        return.obj <- read.csv(csvpath)
    } else {
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
        full_url <- paste(base_url, "/eventapi/jsonfull/query/show", sep = "")
        query.list <- suppressMessages({
            request::http(request::api(full_url))
        })
        query.df <- tidyr::separate(
            dplyr::bind_rows(query.list$events, .id = "commonName"),
            commonName,
            c("commonName", "v"),
            "-"
        )

        query.df_strain <- rlist::list.rbind(lapply(
            query.df$strain,
            rlist::list.cbind
        ))
        return.obj <- dplyr::select(
            dplyr::relocate(
                dplyr::bind_cols(query.df, query.df_strain),
                colnames(query.df_strain),
                .before = strain
            ),
            -strain
        )
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
    step1 <- dplyr::distinct(
        dplyr::arrange(
            dplyr::filter(gwosc.list, commonName %in% source.names),
            dplyr::desc(version)
        ),
        commonName,
        .keep_all = T
    )
    if (length(param) > 1) {
        dplyr::select(step1, param)
    } else {
        dplyr::pull(step1, param)
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
    direct.url = NULL) {
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
        step2 <- dplyr::filter(step1, commonName == event_name)

        # Step 3: Choose version
        if (version == "latest") {
            step3 <- dplyr::distinct(
                dplyr::arrange(
                    step2,
                    dplyr::desc(version)
                ),
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
            step3 <- dplyr::filter(step2, version == version)
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
        step4 <- dplyr::filter(step3, detector == det)

        # Step 5: Filter by dur, file.format, sampling.freq
        step5 <- dplyr::filter(
            dplyr::filter(
                dplyr::filter(step4, duration == dur),
                format == file.format
            ),
            sampling_rate == sampling.freq
        )

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
        tmp <- hdf5r::h5file(filename = destfile, mode = "r")
        tstart <- tmp[["meta"]]$open(name = "GPSstart")$read()
        Data <- tmp[["strain"]]$open(name = "Strain")$read()
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

#' List available observation runs from the GWOSC API
#'
#' Retrieve the canonical names of observation runs available on GWOSC (for
#' example `"O1"`, `"O2"`, `"O3a"`, ...). This is the recommended first call
#' for interactive usage: use the returned run names with \code{list_detector()}
#' and \code{get_segments()} so you don't have to guess valid run identifiers.
#'
#' The function uses the GWOSC v2 API and robustly handles common JSON shapes
#' returned by the server.
#'
#' @param verbose Logical, default \code{FALSE}. If \code{TRUE}, print a short
#'   structure summary of the JSON returned by the GWOSC endpoint to help with
#'   debugging connectivity or API changes.
#'
#' @return A character vector of available observation-run names (no attributes).
#'
#' @details
#' This helper queries \url{https://gwosc.org/api/v2/runs}. If the network
#' request fails or the API response shape is unexpected, a descriptive error
#' will be raised. Use \code{verbose = TRUE} to inspect the raw JSON structure
#' when diagnosing problems.
#'
#' @examples
#' \dontrun{
#' # Typical interactive workflow:
#' runs <- list_obsrun()
#' print(runs)
#' # Choose a run, then inspect detectors:
#' list_detector(runs[1])
#' }
#'
#' @seealso \code{\link{list_detector}}, \code{\link{get_segments}}
#' @references \url{https://gwosc.org/api/}
#' @export
list_obsrun <- function(verbose = FALSE) {
    url <- "https://gwosc.org/api/v2/runs"
    tryCatch(
        {
            res <- httr::GET(url, httr::user_agent("R (httr)"))
            if (httr::http_error(res)) stop("HTTP error from GWOSC API: ", httr::status_code(res))

            text <- httr::content(res, as = "text", encoding = "UTF-8")
            json <- jsonlite::fromJSON(text, simplifyVector = FALSE)

            if (verbose) {
                message("Top-level JSON structure:")
                print(names(json))
                str(json, max.level = 2)
            }

            # 'results' may be a list-of-lists or a data.frame (depending on jsonlite options)
            if (!"results" %in% names(json)) stop("Unexpected JSON structure: 'results' not present.")

            results <- json$results

            if (is.data.frame(results)) {
                # data.frame case: expect a column named "name"
                if (!"name" %in% colnames(results)) stop("Unexpected results structure: no 'name' column.")
                runs <- as.character(results[["name"]])
            } else if (is.list(results)) {
                # list of entries; each entry should have $name
                # handle both list-of-lists and list-of-named vectors
                runs <- vapply(results, function(x) {
                    if (is.null(x[["name"]])) {
                        stop("One of the result entries does not contain 'name'.")
                    }
                    as.character(x[["name"]])
                }, FUN.VALUE = character(1))
            } else {
                stop("Unsupported type for 'results' field: ", class(results)[1])
            }

            names(runs) <- NULL
            return(runs)
        },
        error = function(e) {
            stop("Failed to fetch available observation runs. ", e$message)
        }
    )
}

#' List detectors available for a given GWOSC observation run
#'
#' Return the detector identifiers available for a particular GWOSC observation
#' run (for example \code{"H1"}, \code{"L1"}, \code{"V1"}). Use this to avoid
#' passing invalid detector names to \code{get_segments()}.
#'
#' @param obsrun Character scalar. Observation run identifier (e.g. \code{"O1"},
#'   \code{"O2"}, \code{"O3a"}). Must be a single string returned by
#'   \code{list_obsrun()} (or otherwise known to exist).
#' @param verbose Logical, default \code{FALSE}. If \code{TRUE}, print a short
#'   structure summary of the JSON returned by the GWOSC endpoint for debugging.
#'
#' @return A character vector of detector names available in the requested run.
#'
#' @details
#' The function queries \code{https://gwosc.org/api/v2/runs/<obsrun>} and
#' extracts the \code{detectors} field from the returned JSON. If the field is
#' missing or the request fails, a descriptive error is raised. Use
#' \code{list_obsrun()} to discover valid run names.
#'
#' @examples
#' \dontrun{
#' runs <- list_obsrun()
#' detectors <- list_detector(runs[1])
#' print(detectors)
#' }
#'
#' @seealso \code{\link{list_obsrun}}, \code{\link{get_segments}}
#' @references \url{https://gwosc.org/api/}
#' @export
list_detector <- function(obsrun, verbose = FALSE) {
    url <- sprintf("https://gwosc.org/api/v2/runs/%s", obsrun)
    tryCatch(
        {
            res <- httr::GET(url, httr::user_agent("R (httr)"))
            if (httr::http_error(res)) stop("HTTP error from GWOSC API: ", httr::status_code(res))

            text <- httr::content(res, as = "text", encoding = "UTF-8")
            json <- jsonlite::fromJSON(text, simplifyVector = FALSE)

            if (verbose) {
                message("Run JSON structure:")
                str(json, max.level = 2)
            }

            # detectors may be present at top-level of this JSON
            if ("detectors" %in% names(json)) {
                dets <- json$detectors
                # ensure character vector
                dets <- as.character(dets)
                return(dets)
            }

            # fallback: maybe results/data frame shape or nested structure
            if ("results" %in% names(json)) {
                results <- json$results
                # if results is data.frame with 'detectors' column
                if (is.data.frame(results) && "detectors" %in% colnames(results)) {
                    # take first row's detectors (or collapse unique)
                    dets <- results$detectors[[1]]
                    return(as.character(dets))
                }
            }

            stop("No 'detectors' field found in API response for run: ", obsrun)
        },
        error = function(e) {
            stop("Failed to fetch detector list for obsrun '", obsrun, "'. ", e$message)
        }
    )
}

#' Retrieve strain-file metadata from GWOSC (API v2)
#'
#' Query GWOSC's v2 \code{/strain-files} endpoint and return a tidy data.frame
#' describing available strain files that match the supplied criteria.
#'
#' This function is designed to be used as part of a simple workflow:
#' \code{list_obsrun()} → \code{list_detector()} → \code{get_segments()} →
#' \code{download_segments()}. You may pass multiple detectors (e.g.
#' \code{c("H1", "L1")}) and the function will query each detector and merge the
#' results.
#'
#' @param obsrun Character. Observation run name (e.g. \code{"O1"}). This value
#'   is used for informative messages and does not affect server-side filtering.
#' @param detector Character vector. One or more detector names (e.g.
#'   \code{c("H1", "L1")}).
#' @param GPSstart Integer. Start time in GPS seconds.
#' @param GPSend Integer. End time in GPS seconds. If omitted, defaults to
#'   \code{GPSstart + dur}.
#' @param sampling.freq Integer (default 4096). Sampling frequency in Hz. Only
#'   4096 (≈4 kHz) and 16384 (≈16 kHz) are supported because the API expects a
#'   sample-rate in kHz (4 or 16).
#' @param dur Integer (default 4096). Requested segment duration in seconds.
#' @param file.format Character (default \code{"hdf5"}). Desired file format;
#'   typical values are \code{"hdf5"} or \code{"gwf"}.
#' @param duty.cycle.lwr Numeric (default 95). NOTE: the \code{/strain-files}
#'   endpoint does **not** provide a \code{duty_cycle} field. This argument is
#'   accepted for compatibility with earlier code, but is ignored by the API
#'   query; see \code{Details} for how to perform timeline-based duty-cycle
#'   filtering if you need it.
#'
#' @return A data.frame with one row per matching file and columns including
#'   \code{GPSstart} (integer), \code{utc_start} (ISO string or NA),
#'   \code{detector}, \code{sample_rate_kHz}, \code{url} (download URL),
#'   \code{detail_url} (if provided by the API), and \code{duration}.
#'
#' @details
#' The API v2 \code{/strain-files} endpoint is queried for each detector in
#' \code{detector}, and the per-detector results are combined. Because the
#' server response does not include duty-cycle information, the function cannot
#' filter by \code{duty_cycle} directly. If you require duty-cycle based
#' filtering, retrieve timeline/segment information from the timeline segments
#' endpoints and post-filter the returned file list (this is more advanced and
#' not performed automatically by this helper).
#'
#' @examples
#' \dontrun{
#' # Discover a run + detectors, then get files
#' runs <- list_obsrun()
#' dets <- list_detector("O1")
#' files <- get_segments("O1", dets, GPSstart = 1126051217, GPSend = 1126051217 + 86400)
#' head(files)
#' }
#'
#' @seealso \code{\link{list_obsrun}}, \code{\link{list_detector}},
#'   \code{\link{download_segments}}
#' @references \url{https://gwosc.org/api/}
#' @export
get_segments <- function(obsrun, detector, GPSstart, GPSend,
                         sampling.freq = 4096, dur = 4096,
                         file.format = "hdf5",
                         duty.cycle.lwr = 95) {
    if (missing(GPSend)) GPSend <- GPSstart + dur

    # map sampling.freq (Hz) -> sample-rate kHz expected by API (4 or 16)
    sample_khz <- as.integer(round(sampling.freq / 1000))
    if (!sample_khz %in% c(4L, 16L)) {
        stop("sampling.freq must correspond to 4 kHz (4096) or 16 kHz (16384) -- got: ", sampling.freq)
    }

    # Choose which URL field to use from the API result based on file.format
    url_field <- switch(tolower(file.format),
        "hdf5" = "hdf5_url",
        "gwf"  = "gwf_url",
        "txt"  = "gwf_url", # no separate txt URL in many responses; leave as gwf_url fallback
        stop("Unsupported file.format: ", file.format)
    )

    all_results <- list()

    for (det in detector) {
        message("> Querying GWOSC strain-files for ", det, " (", obsrun, ") ...")
        base <- "https://gwosc.org/api/v2/strain-files"

        # build query; include duration and file-format to narrow results on server side
        q <- list(
            start = as.integer(GPSstart),
            stop = as.integer(GPSend),
            detector = det,
            `sample-rate` = sample_khz,
            duration = as.integer(dur),
            `file-format` = tolower(file.format)
        )

        res <- httr::GET(base, query = q, httr::user_agent("R (gw osc client)"))
        if (httr::http_error(res)) {
            warning("HTTP error when querying strain-files for ", det, ": ", httr::status_code(res))
            next
        }

        txt <- httr::content(res, as = "text", encoding = "UTF-8")
        js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE),
            error = function(e) {
                warning("Failed to parse JSON for detector ", det, ": ", e$message)
                return(NULL)
            }
        )
        if (is.null(js)) next
        if (!"results" %in% names(js) || length(js$results) == 0) {
            message("|> no strain-files found for ", det, " in the given interval")
            next
        }

        results <- js$results

        # results may be a data.frame already or a list; safely coerce to data.frame
        if (is.data.frame(results)) {
            df <- as.data.frame(results, stringsAsFactors = FALSE)
        } else if (is.list(results)) {
            # convert list-of-records to data.frame (dplyr::bind_rows handles NULL entries gracefully)
            df <- tryCatch(dplyr::bind_rows(results), error = function(e) data.frame())
        } else {
            df <- data.frame()
        }

        if (nrow(df) == 0) next

        # pick the URL for the requested file format; if missing, try reasonable fallbacks
        if (!(url_field %in% colnames(df))) {
            # Try common fields
            possible <- c("hdf5_url", "gwf_url", "download_url")
            found <- intersect(possible, colnames(df))
            if (length(found) > 0) {
                url_field <- found[1]
            } else {
                warning("No usable download URL field found for detector ", det)
                next
            }
        }

        # normalize columns for return
        df2 <- data.frame(
            GPSstart = as.integer(df$gps_start),
            utc_start = if ("utc_start" %in% colnames(df)) as.character(df$utc_start) else NA_character_,
            detector = as.character(df$detector),
            sample_rate_kHz = if ("sample_rate_kHz" %in% colnames(df)) as.integer(df$sample_rate_kHz) else sample_khz,
            url = as.character(df[[url_field]]),
            detail_url = if ("detail_url" %in% colnames(df)) as.character(df$detail_url) else NA_character_,
            stringsAsFactors = FALSE
        )

        # add duration column (requested duration)
        df2$duration <- as.integer(dur)

        all_results[[det]] <- df2
    }

    if (length(all_results) == 0) stop("No segments/files found for the requested detectors/time range.")

    out_df <- dplyr::bind_rows(all_results)

    # The old code allowed duty_cycle filtering. strain-files responses do NOT include duty_cycle.
    # So: we cannot filter on duty_cycle here. Warn the user if they asked for a duty cutoff.
    if (!missing(duty.cycle.lwr) && !is.null(duty.cycle.lwr) && duty.cycle.lwr > 0) {
        warning(
            "Note: 'duty.cycle.lwr' cannot be applied with the strain-files endpoint (no duty_cycle field).",
            " If you need duty-cycle filtering, call the timeline segments API and post-filter."
        )
    }

    # final message about estimated size (approx 124 MB per 4096s segment — keep as before)
    est.size <- ifelse(124 * nrow(out_df) > 1024,
        paste0(trunc(124 * nrow(out_df) / 1024), " GB"),
        paste0(124 * nrow(out_df), " MB")
    )
    message("|> Found ", nrow(out_df), " files (est. size ~ ", est.size, ")")

    # sort by GPSstart
    out_df <- dplyr::arrange(out_df, GPSstart)

    return(out_df)
}

#' Download files listed in a GWOSC segment-data frame
#'
#' Download the files described by the \code{url} column of \code{seg.df} and
#' save them into a local directory. This helper is intentionally simple: it
#' takes the \code{url} values returned by \code{get_segments()} and preserves
#' the original file basenames when saving.
#'
#' @param seg.df A data.frame as returned by \code{get_segments()} that
#'   contains a \code{url} column. The function will attempt to download each
#'   \code{url} into \code{path}.
#' @param path Character. Directory to save downloaded files. The directory is
#'   created recursively if it does not exist.
#' @param background Logical (default \code{FALSE}). If \code{TRUE} and the
#'   \code{job} package is available, downloads will be launched as a
#'   background job. If \code{job} is not installed the function falls back to
#'   foreground downloads with a warning.
#' @param background.wait Integer (default 30). When using \code{background =
#'   TRUE}, the function polls for the presence of the first file every
#'   \code{background.wait} seconds and returns once the first file exists.
#' @param timeout Integer (default 300). Download timeout in seconds; the
#'   function temporarily sets \code{options(timeout = ...)} while downloading.
#'
#' @return A character vector with full paths to the files that were (or will
#'   be) written. If background mode is used the files may still be downloading
#'   when this function returns.
#'
#' @examples
#' \dontrun{
#' files <- get_segments("O1", "H1", 1126051217, 1126051217 + 4096)
#' outpaths <- download_segments(files, path = "data/O1_H1/")
#' print(outpaths)
#' }
#'
#' @seealso \code{\link{get_segments}}
#' @export
download_segments <- function(
    seg.df, path,
    background = FALSE,
    background.wait = 30,
    timeout = 300) {
    if (!curl::has_internet()) stop("No internet connection")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)

    original.timeout <- getOption("timeout")
    options(timeout = max(timeout, original.timeout))

    filenames <- file.path(path, basename(seg.df$url))

    download.expr <- expr({
        for (i in seq_along(seg.df$url)) {
            download.file(
                url = seg.df$url[i],
                destfile = filenames[i],
                quiet = FALSE
            )
        }
    })

    message("> Downloading files to: ", normalizePath(path))
    if (background) {
        if (!requireNamespace("job", quietly = TRUE)) {
            warning("'job' package not available; falling back to foreground download")
            eval(download.expr)
        } else {
            job::job({
                eval(download.expr)
            })
            while (!file.exists(filenames[1])) {
                message("|> Waiting ", background.wait, "s for first file...")
                Sys.sleep(background.wait)
            }
        }
    } else {
        eval(download.expr)
    }

    options(timeout = original.timeout)
    return(filenames)
}

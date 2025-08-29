#' List GW event nicknames from GWOSC (API v2)
#'
#' Query the GWOSC API v2 `/api/v2/events` endpoint and return a character
#' vector of all known event names (e.g. "GW150914", "GW151226", ...).
#'
#' The function automatically follows pagination (the `next` link in the API
#' response) until all pages are retrieved. It attempts a few Accept header
#' strategies to be robust against GWOSC API variations.
#'
#' @param verbose Logical scalar. If TRUE, print progress messages. Default FALSE.
#' @return Character vector of event names.
#' @examples
#' \dontrun{
#' events <- list_gwosc_event()
#' head(events)
#' }
#' @export
list_gwosc_event <- function(verbose = FALSE) {
    if (!requireNamespace("httr", quietly = TRUE)) stop("Please install the 'httr' package.")
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install the 'jsonlite' package.")

    base_url <- "https://gwosc.org/api/v2/events"
    # start with page=1 (server defaults okay), but explicitly follow 'next' links
    url <- paste0(base_url, "?per_page=100") # request 100 per page to reduce round-trips

    names_out <- character(0L)

    # helper to GET with a few Accept strategies
    fetch_text_with_accept <- function(u) {
        # try Accept: application/json, then application/vnd.api+json, then no Accept
        tries <- list(
            list(accept = "application/json"),
            list(accept = "application/vnd.api+json"),
            list(accept = NULL)
        )
        last_err <- NULL
        for (t in tries) {
            if (!is.null(t$accept)) {
                res <- tryCatch(
                    httr::GET(u, httr::user_agent("R (gwosc client)"), httr::accept(t$accept)),
                    error = function(e) e
                )
            } else {
                res <- tryCatch(
                    httr::GET(u, httr::user_agent("R (gwosc client)")),
                    error = function(e) e
                )
            }
            if (inherits(res, "error")) {
                last_err <- res
                next
            }
            # if server responded with 406 or other error, consider next try
            if (httr::http_error(res)) {
                last_err <- res
                next
            }
            # got something; return as text
            txt <- httr::content(res, as = "text", encoding = "UTF-8")
            return(list(ok = TRUE, text = txt, status = httr::status_code(res)))
        }
        # all failed
        stop("Failed to GET ", u, ": ", if (inherits(last_err, "error")) last_err$message else paste0("HTTP ", httr::status_code(last_err)))
    }

    page_num <- 1L
    while (!is.null(url) && nzchar(url)) {
        if (verbose) message(sprintf("[page %d] GET %s", page_num, url))
        got <- tryCatch(fetch_text_with_accept(url), error = function(e) e)
        if (inherits(got, "error")) stop("Failed to fetch events: ", got$message)

        txt <- got$text
        # some endpoints may occasionally return HTML on errors; detect and fail early
        if (grepl("^\\s*<", txt)) {
            stop("GWOSC returned HTML instead of JSON when fetching: ", url)
        }

        js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) stop("Failed to parse JSON from GWOSC: ", e$message))

        # locate results array; robust to 'results' or 'data'
        results <- NULL
        if ("results" %in% names(js)) {
            results <- js$results
        } else if ("data" %in% names(js)) {
            results <- js$data
        } else if (is.list(js) && length(js) > 0 && any(vapply(js, function(x) is.list(x) && !is.null(x$name), logical(1)))) {
            # maybe top-level list of objects with name fields
            results <- js
        } else {
            stop("Unexpected JSON structure from GWOSC events endpoint (no 'results'/'data').")
        }

        # extract names
        if (is.data.frame(results)) {
            if ("name" %in% colnames(results)) {
                names_page <- as.character(results$name)
            } else {
                # if rows are data.frame but no name column, try first column
                names_page <- as.character(results[[1]])
            }
        } else if (is.list(results)) {
            # list of records -> try to pluck 'name' from each
            names_page <- vapply(results, function(rec) {
                if (is.null(rec)) {
                    return(NA_character_)
                }
                if (is.list(rec) && !is.null(rec$name)) {
                    return(as.character(rec$name))
                }
                if (is.atomic(rec) && length(rec) >= 1) {
                    return(as.character(rec[[1]]))
                }
                NA_character_
            }, FUN.VALUE = character(1), USE.NAMES = FALSE)
            names_page <- names_page[!is.na(names_page)]
        } else {
            names_page <- character(0)
        }

        if (length(names_page) > 0) {
            names_out <- c(names_out, names_page)
        }

        # follow pagination: prefer 'next' field if present, else use page info
        next_url <- NULL
        if ("next" %in% names(js) && !is.null(js[["next"]]) && nzchar(js[["next"]])) {
            next_url <- js[["next"]]
        } else if (!is.null(js$results) && is.list(js$results) && !is.null(js$results_per_page) && !is.null(js$page_number) && !is.null(js$num_pages)) {
            # fallback: construct next page if page_number < num_pages
            if (!is.null(js$page_number) && !is.null(js$num_pages) && js$page_number < js$num_pages) {
                page_num_local <- as.integer(js$page_number) + 1L
                # replace or append per_page/page params
                next_url <- paste0(base_url, "?per_page=100&page=", page_num_local)
            }
        }
        # if no next found, break
        if (is.null(next_url) || !nzchar(next_url)) break
        url <- next_url
        page_num <- page_num + 1L
    } # end pagination loop

    # unique, preserve order
    if (length(names_out) == 0) {
        return(character(0))
    }
    unique_names <- names_out[!duplicated(names_out)]
    return(unique_names)
}

#' List all available GWOSC parameter names
#'
#' Returns a character vector of all valid parameter names
#' that can be used in \code{\link{get_gwosc_param}}.
#'
#' @return A character vector of parameter names.
#' @examples
#' list_gwosc_param()
#' @seealso \code{\link{get_gwosc_param}}
#' @export
list_gwosc_param <- function() {
    c(
        "GPS",
        "mass_1_source", "mass_1_source_lower", "mass_1_source_upper", "mass_1_source_unit",
        "mass_2_source", "mass_2_source_lower", "mass_2_source_upper", "mass_2_source_unit",
        "chi_eff", "chi_eff_lower", "chi_eff_upper", "chi_eff_unit",
        "total_mass_source", "total_mass_source_lower", "total_mass_source_upper", "total_mass_source_unit",
        "chirp_mass_source", "chirp_mass_source_lower", "chirp_mass_source_upper", "chirp_mass_source_unit",
        "chirp_mass", "chirp_mass_lower", "chirp_mass_upper", "chirp_mass_unit",
        "redshift", "redshift_lower", "redshift_upper", "redshift_unit",
        "far", "far_unit",
        "p_astro", "p_astro_unit",
        "final_mass_source", "final_mass_source_lower", "final_mass_source_upper", "final_mass_source_unit",
        "network_matched_filter_snr",
        "network_matched_filter_snr_lower", "network_matched_filter_snr_upper", "network_matched_filter_snr_unit",
        "luminosity_distance", "luminosity_distance_lower", "luminosity_distance_upper", "luminosity_distance_unit"
    )
}

#' Get GWOSC parameters for a specific GW event
#'
#' This function queries the GWOSC API v2 to fetch the latest version of
#' gravitational-wave event metadata and its preferred physical parameters.
#' It returns either all available parameters or a user-specified parameter.
#'
#' The function automatically:
#' \itemize{
#'   \item Selects the latest event version.
#'   \item Extracts GPS time from event-version details or parameter lists.
#'   \item Uses only preferred (default) parameter values provided by GWOSC.
#'   \item Returns absolute values for lower/upper errors.
#' }
#'
#' @param name Character. Event name (e.g. `"GW150914"`, `"GW190521"`).
#' @param param Character. Either `"all"` (default) to return all parameters,
#'   or one of the valid parameter names from \code{list_gwosc_param()}.
#'
#' @return A \code{data.frame} with one row named by the event.
#'   \itemize{
#'     \item If \code{param = "all"}, all allowed parameters are returned.
#'     \item If \code{param} is a single column name, only that parameter is returned.
#'   }
#'
#' @examples
#' \dontrun{
#' # List all available parameter names
#' list_gwosc_param()
#'
#' # Fetch all parameters for GW150914
#' get_gwosc_param("GW150914", "all")
#'
#' # Fetch only the chirp mass source for GW150914
#' get_gwosc_param("GW150914", "chirp_mass_source")
#' }
#'
#' @seealso \code{\link{list_gwosc_param}}
#' @export
get_gwosc_param <- function(name, param = "all") {
    stopifnot(is.character(name), length(name) == 1L)
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.")

    # Allowed parameters
    allowed <- list_gwosc_param()
    if (!(length(param) == 1L && is.character(param))) stop("`param` must be a single character value.")
    if (!identical(param, "all") && !param %in% allowed) {
        stop("`param` must be 'all' or one of: ", paste(allowed, collapse = ", "))
    }

    # Helper functions
    get_json <- function(url) jsonlite::fromJSON(url, simplifyVector = TRUE)
    enc <- function(x) utils::URLencode(x, reserved = TRUE)
    base <- "https://gwosc.org/api/v2"
    as_num <- function(x) {
        if (is.null(x) || length(x) == 0) {
            return(NA_real_)
        }
        suppressWarnings(as.numeric(x)[1L])
    }

    # 1) Resolve the latest event version
    ev <- tryCatch(get_json(sprintf("%s/events/%s", base, enc(name))), error = function(e) NULL)
    if (is.null(ev) || is.null(ev$versions) || length(ev$versions) == 0) {
        out_cols <- if (identical(param, "all")) allowed else param
        df0 <- as.data.frame(setNames(replicate(length(out_cols), logical(0)), out_cols))
        rownames(df0) <- name
        return(df0)
    }
    vdf <- as.data.frame(ev$versions)
    vmax <- max(as.integer(vdf$version), na.rm = TRUE)

    # 2) Try to extract GPS from event-version details
    evv <- tryCatch(
        get_json(sprintf("%s/event-versions/%s-v%d?format=api", base, enc(name), vmax)),
        error = function(e) NULL
    )
    GPS <- NA_real_
    if (!is.null(evv)) {
        if (!is.null(evv$gps)) GPS <- as_num(evv$gps)
        if (is.na(GPS) && !is.null(evv$gps_time)) GPS <- as_num(evv$gps_time)
    }

    # 3) Default parameters (preferred values only)
    dpar <- tryCatch(
        get_json(sprintf("%s/event-versions/%s-v%d/default-parameters", base, enc(name), vmax)),
        error = function(e) NULL
    )
    pref <- if (!is.null(dpar) && !is.null(dpar$results)) as.data.frame(dpar$results, stringsAsFactors = FALSE) else NULL

    # 4) If GPS is still NA, try to extract from /parameters (preferred pipelines)
    if (is.na(GPS)) {
        parlist <- tryCatch(
            get_json(sprintf("%s/event-versions/%s-v%d/parameters", base, enc(name), vmax)),
            error = function(e) NULL
        )
        if (!is.null(parlist) && !is.null(parlist$results) && length(parlist$results) > 0) {
            res <- parlist$results
            if ("is_preferred" %in% names(res)) res <- res[res$is_preferred %in% TRUE, , drop = FALSE]
            if (nrow(res) > 0 && "parameters" %in% names(res)) {
                time_keys <- c("geocent_time", "t_geocent", "tc", "time", "gps", "gps_time")
                for (i in seq_len(nrow(res))) {
                    pi <- res$parameters[[i]]
                    if (is.null(pi)) next
                    pdf <- as.data.frame(pi, stringsAsFactors = FALSE)
                    hit <- which(pdf$name %in% time_keys)[1L]
                    if (!is.na(hit)) {
                        GPS <- as_num(pdf$best[hit])
                        if (!is.na(GPS)) break
                    }
                }
            }
        }
    }

    # Helper to pull best/err/unit for a parameter
    get_triplet <- function(base_name) {
        if (is.null(pref) || nrow(pref) == 0) {
            return(list(val = NA_real_, lo = NA_real_, up = NA_real_, unit = ""))
        }
        row <- pref[pref$name == base_name, , drop = FALSE]
        if (nrow(row) == 0) {
            return(list(val = NA_real_, lo = NA_real_, up = NA_real_, unit = ""))
        }
        val <- as_num(row$best[1L])
        lo <- abs(as_num(row$lower_error[1L]))
        up <- abs(as_num(row$upper_error[1L]))
        unit <- ifelse(is.null(row$unit[1L]) || is.na(row$unit[1L]), "", as.character(row$unit[1L]))
        list(val = val, lo = lo, up = up, unit = unit)
    }

    # 5) Build one-row output
    out <- setNames(vector("list", length(allowed)), allowed)
    for (nm in allowed) out[[nm]] <- NA_real_
    out[["GPS"]] <- GPS

    bases <- c(
        "mass_1_source", "mass_2_source", "chi_eff", "total_mass_source",
        "chirp_mass_source", "chirp_mass", "redshift", "far", "p_astro",
        "final_mass_source", "network_matched_filter_snr", "luminosity_distance"
    )
    for (b in bases) {
        t <- get_triplet(b)
        out[[b]] <- t$val
        lo_nm <- paste0(b, "_lower")
        up_nm <- paste0(b, "_upper")
        unit_nm <- paste0(b, "_unit")
        if (lo_nm %in% allowed) out[[lo_nm]] <- t$lo
        if (up_nm %in% allowed) out[[up_nm]] <- t$up
        if (unit_nm %in% allowed) out[[unit_nm]] <- t$unit
    }

    df <- as.data.frame(out, check.names = FALSE, optional = TRUE)
    rownames(df) <- name

    if (identical(param, "all")) {
        return(df)
    } else {
        if (!param %in% names(df)) df[[param]] <- NA_real_
        one <- df[, param, drop = FALSE]
        rownames(one) <- name
        return(one)
    }
}

#' Download dataset/strain-file for a GWOSC event (robust v2 logic)
#'
#' For a given GW event name (e.g. "GW150914") and detector (e.g. "H1"),
#' try several GWOSC v2 endpoints in a sensible order to locate a matching
#' strain-file URL and download it.
#'
#' The function attempts (in order):
#'  1. /api/v2/events/<EVENT>/strain-files
#'  2. /api/v2/events/<EVENT> -> choose latest version -> /event-versions/<EVENT>-vN/strain-files
#'  3. /api/v2/event-versions/<EVENT>/dataset  -> use dataset$strain_files_url
#'
#' It filters candidates by detector and file.format and chooses the best match
#' (nearest gps_start when available). If load=TRUE and the file is HDF5, the
#' function returns a `ts` object (and optionally removes the file).
#'
#' @param event_name Character. GWOSC event name, e.g. "GW150914".
#' @param det Character. Detector name, e.g. "H1".
#' @param path Character. Directory to save file (created if missing).
#' @param file.format Character. "hdf5" (default), "gwf", or "txt".
#' @param timeout Integer. Download timeout seconds (default 300).
#' @param load Logical. If TRUE and hdf5, load file as `ts` and return it.
#' @param remove Logical. If load=TRUE and remove=TRUE, delete file after loading.
#' @param verbose Logical. Print progress messages.
#' @return If load=FALSE, normalized path to downloaded file. If load=TRUE, a `ts` object.
#' @export
download_event <- function(event_name,
                           det,
                           path = ".",
                           file.format = "hdf5",
                           timeout = 300,
                           load = FALSE,
                           remove = FALSE,
                           verbose = TRUE) {
    if (!curl::has_internet()) stop("No internet connection")
    if (missing(event_name) || missing(det)) stop("event_name and det must be provided")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)

    # small helper: GET JSON defensively
    fetch_json <- function(url, verbose_local = FALSE) {
        if (verbose_local) message("  -> GET ", url)
        res <- httr::GET(url, httr::user_agent("R (gwosc client)"))
        if (httr::http_error(res)) {
            return(list(ok = FALSE, status = httr::status_code(res), text = httr::content(res, as = "text", encoding = "UTF-8")))
        }
        txt <- httr::content(res, as = "text", encoding = "UTF-8")
        if (grepl("^\\s*<", txt)) {
            return(list(ok = FALSE, status = res$status, text = txt))
        }
        js <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
        if (is.null(js)) {
            return(list(ok = FALSE, status = res$status, text = txt))
        }
        list(ok = TRUE, status = res$status, json = js)
    }

    # try endpoints in order and record attempted URLs for diagnostics
    tried_urls <- character(0)
    sf_results <- NULL

    # 1) Try event-level strain-files
    ev_sf_url <- sprintf("https://gwosc.org/api/v2/events/%s/strain-files", utils::URLencode(event_name, reserved = TRUE))
    tried_urls <- c(tried_urls, ev_sf_url)
    if (verbose) message("> Trying event-level strain-files: ", ev_sf_url)
    r1 <- fetch_json(ev_sf_url, verbose_local = verbose)
    if (isTRUE(r1$ok) && !is.null(r1$json)) {
        # results may be top-level or under $results
        if ("results" %in% names(r1$json)) sf_results <- r1$json$results else sf_results <- r1$json
    } else {
        if (verbose) message("|> event-level request failed (status=", r1$status, "). Will try versions/dataset.")
    }

    # 2) If none, try event -> versions -> event-version strain-files
    if (is.null(sf_results) || (is.list(sf_results) && length(sf_results) == 0)) {
        ev_url <- sprintf("https://gwosc.org/api/v2/events/%s", utils::URLencode(event_name, reserved = TRUE))
        tried_urls <- c(tried_urls, ev_url)
        if (verbose) message("> Resolving event versions: ", ev_url)
        r2 <- fetch_json(ev_url, verbose_local = verbose)
        if (!isTRUE(r2$ok) || is.null(r2$json)) {
            if (verbose) message("|> Failed to fetch event metadata (status=", r2$status, ").")
        } else {
            versions <- r2$json$versions
            if (!is.null(versions) && length(versions) > 0) {
                # extract numeric version where possible
                vnums <- vapply(versions, function(v) {
                    if (!is.null(v$version)) {
                        return(as.integer(v$version))
                    }
                    NA_integer_
                }, integer(1))
                chosen_idx <- if (all(is.na(vnums))) 1L else which.max(vnums)
                chosen_ver <- versions[[chosen_idx]]
                # try direct strain_files_url on chosen version if present
                if (!is.null(chosen_ver$strain_files_url) && nzchar(as.character(chosen_ver$strain_files_url))) {
                    sf_url <- as.character(chosen_ver$strain_files_url)
                    tried_urls <- c(tried_urls, sf_url)
                    if (verbose) message("> Trying version-provided strain_files_url: ", sf_url)
                    r3 <- fetch_json(sf_url, verbose_local = verbose)
                    if (isTRUE(r3$ok) && !is.null(r3$json)) {
                        sf_results <- if ("results" %in% names(r3$json)) r3$json$results else r3$json
                    }
                }
                # else try detail_url + "/strain-files" or constructed event-versions url
                if (is.null(sf_results)) {
                    detail_url <- if (!is.null(chosen_ver$detail_url) && nzchar(as.character(chosen_ver$detail_url))) {
                        as.character(chosen_ver$detail_url)
                    } else if (!is.null(chosen_ver$version)) {
                        sprintf("https://gwosc.org/api/v2/event-versions/%s-v%d", utils::URLencode(event_name, reserved = TRUE), as.integer(chosen_ver$version))
                    } else {
                        NULL
                    }

                    if (!is.null(detail_url)) {
                        sf_url2 <- paste0(detail_url, "/strain-files")
                        tried_urls <- c(tried_urls, sf_url2)
                        if (verbose) message("> Trying event-version strain-files: ", sf_url2)
                        r4 <- fetch_json(sf_url2, verbose_local = verbose)
                        if (isTRUE(r4$ok) && !is.null(r4$json)) {
                            sf_results <- if ("results" %in% names(r4$json)) r4$json$results else r4$json
                        }
                    }
                }
            } else {
                if (verbose) message("|> No versions listed for event.")
            }
        }
    }

    # 3) If still none, try dataset endpoint (event-version dataset)
    if (is.null(sf_results) || (is.list(sf_results) && length(sf_results) == 0)) {
        # try /api/v2/event-versions/<EVENT>/dataset  (note: user code used this)
        ds_url <- sprintf("https://gwosc.org/api/v2/event-versions/%s/dataset", utils::URLencode(event_name, reserved = TRUE))
        tried_urls <- c(tried_urls, ds_url)
        if (verbose) message("> Trying event-version dataset URL: ", ds_url)
        r5 <- fetch_json(ds_url, verbose_local = verbose)
        if (isTRUE(r5$ok) && !is.null(r5$json)) {
            # dataset may contain 'strain_files_url'
            if (!is.null(r5$json$strain_files_url) && nzchar(as.character(r5$json$strain_files_url))) {
                sf_url3 <- as.character(r5$json$strain_files_url)
                tried_urls <- c(tried_urls, sf_url3)
                if (verbose) message("> Trying dataset's strain_files_url: ", sf_url3)
                r6 <- fetch_json(sf_url3, verbose_local = verbose)
                if (isTRUE(r6$ok) && !is.null(r6$json)) {
                    sf_results <- if ("results" %in% names(r6$json)) r6$json$results else r6$json
                }
            } else {
                if (verbose) message("|> dataset JSON did not include 'strain_files_url'.")
            }
        } else {
            if (verbose) message("|> dataset endpoint failed (status=", r5$status, ").")
        }
    }

    # If still no results, error with diagnostics
    if (is.null(sf_results) || length(sf_results) == 0) {
        stop("No strain-files entries found for event '", event_name, "'. Tried URLs:\n", paste(tried_urls, collapse = "\n"))
    }

    # coerce to data.frame if possible (sf_results may already be data.frame)
    sf_df <- NULL
    if (is.data.frame(sf_results)) {
        sf_df <- as.data.frame(sf_results, stringsAsFactors = FALSE)
    } else if (is.list(sf_results)) {
        sf_df <- tryCatch(dplyr::bind_rows(sf_results), error = function(e) data.frame())
    } else {
        stop("Unexpected shape for strain-files response.")
    }

    if (nrow(sf_df) == 0) stop("No strain-file records after coercion for event ", event_name)

    # normalize common column names
    if (!"gps_start" %in% colnames(sf_df) && "start" %in% colnames(sf_df)) sf_df$gps_start <- sf_df$start
    if (!"detector" %in% colnames(sf_df) && "detectors" %in% colnames(sf_df)) {
        # detectors may be array; coerce to single detector string if needed
        sf_df$detector <- sapply(sf_df$detectors, function(x) {
            if (is.null(x)) {
                return(NA_character_)
            }
            if (is.atomic(x)) {
                return(as.character(x))
            }
            if (is.list(x) && length(x) == 1) {
                return(as.character(x[[1]]))
            }
            paste(as.character(unlist(x)), collapse = ",")
        })
    }

    # find url field per file.format
    url_field <- switch(tolower(file.format),
        "hdf5" = "hdf5_url",
        "gwf"  = "gwf_url",
        "txt"  = "gwf_url",
        stop("Unsupported file.format: ", file.format)
    )
    possible_urls <- c("hdf5_url", "gwf_url", "download_url", "url")
    if (!(url_field %in% colnames(sf_df))) {
        found <- intersect(possible_urls, colnames(sf_df))
        if (length(found) > 0) url_field <- found[1] else stop("No download URL field found in strain-files records.")
    }

    # filter by detector
    cand <- sf_df
    if (!is.null(det)) {
        cand <- cand[as.character(cand$detector) == as.character(det), , drop = FALSE]
    }

    if (nrow(cand) == 0) stop("No strain-file entries for detector=", det, " for event ", event_name)

    # prefer candidates matching duration & sample-rate if such columns exist
    # normalize sample_rate column names
    sample_cols <- intersect(c("sample_rate_kHz", "sample_rate_khz", "sample_rate", "sampleRate"), colnames(cand))
    duration_cols <- intersect(c("duration", "file_duration"), colnames(cand))

    # if multiple candidates, choose best by (1) exact duration match (if available), (2) nearest gps_start to dataset gps_start (if present)
    chosen_idx <- 1L
    if (nrow(cand) > 1) {
        # filter by duration if available
        if (length(duration_cols) > 0) {
            dur_vals <- as.integer(cand[[duration_cols[1]]])
            target_dur <- if (!is.null(r5) && isTRUE(r5$ok) && !is.null(r5$json$duration)) as.integer(r5$json$duration) else NA_integer_
            # but user didn't pass dur param; we prefer any exact duration if available (event dataset usually has duration)
            exact_idx <- which(!is.na(dur_vals) & !is.na(target_dur) & dur_vals == target_dur)
            if (length(exact_idx) > 0) {
                chosen_idx <- exact_idx[1]
            } else {
                # else prefer the first; will refine with gps proximity if possible
                chosen_idx <- 1L
            }
        }
        # refine by gps_start proximity if event dataset gps_start available
        if ("gps_start" %in% colnames(cand)) {
            # try to take event dataset gps_start from earlier dataset JSON if present
            ds_gps <- NULL
            # try to extract ds_gps from r5$json (dataset) or r2$json (event)
            if (exists("r5") && is.list(r5) && isTRUE(r5$ok) && !is.null(r5$json$gps_start)) ds_gps <- as.integer(r5$json$gps_start)
            if (exists("r1") && is.list(r1) && isTRUE(r1$ok) && is.null(ds_gps)) {
                # maybe event-level had gps? skip
            }
            # if ds_gps not available, try to use first candidate gps as tie-breaker
            if (!is.null(ds_gps) && !is.na(ds_gps)) {
                gps_vals <- as.integer(cand$gps_start)
                diffs <- abs(gps_vals - ds_gps)
                chosen_idx <- which.min(diffs)
            } else {
                # fallback: pick the candidate with smallest absolute difference to its own median (stable pick)
                gps_vals <- as.integer(cand$gps_start)
                na_idx <- is.na(gps_vals)
                if (all(na_idx)) {
                    chosen_idx <- chosen_idx
                } else {
                    chosen_idx <- which.min(abs(gps_vals - median(gps_vals, na.rm = TRUE)))
                }
            }
        }
    }

    chosen_row <- cand[chosen_idx, , drop = FALSE]

    file_url <- as.character(chosen_row[[url_field]])
    if (is.null(file_url) || !nzchar(file_url)) stop("Chosen strain-file record has no usable URL field (tried field: ", url_field, ")")

    destfile <- file.path(path, basename(file_url))
    if (verbose) {
        message("> Downloading: ", file_url)
        message("  -> to: ", destfile)
    }

    original.timeout <- getOption("timeout")
    options(timeout = max(timeout, original.timeout))
    utils::download.file(url = file_url, destfile = destfile, quiet = FALSE)
    options(timeout = original.timeout)

    if (load) {
        if (tolower(file.format) != "hdf5") stop("'load=TRUE' only supported for hdf5 files")
        tmp <- hdf5r::h5file(filename = destfile, mode = "r")
        tstart <- tmp[["meta"]]$open(name = "GPSstart")$read()
        Data <- tmp[["strain"]]$open(name = "Strain")$read()
        tmp$close_all()
        freq <- 4096
        if (length(sample_cols) > 0) {
            # sample_cols might be kHz: convert if necessary
            s <- chosen_row[[sample_cols[1]]]
            if (!is.null(s) && !is.na(s)) {
                s_num <- as.numeric(s)
                # if numeric and small (4/16) treat as kHz; if larger treat as Hz
                if (s_num %in% c(4, 16)) freq <- as.integer(s_num * 1000) else if (s_num > 1000) freq <- as.integer(s_num)
            }
        }
        Data <- ts(Data, start = tstart, frequency = freq)
        attr(Data, "source.name") <- event_name
        if (remove) {
            file.remove(destfile)
            if (verbose) message("|> removed ", destfile)
        }
        return(Data)
    }

    return(normalizePath(destfile))
}

# Segments----

#' List available observation runs from the GWOSC API
#'
#' Retrieve the canonical names of observation runs available on GWOSC (for
#' example `"O1"`, `"O2"`, `"O3a"`, ...). This is the recommended first call
#' for interactive usage: use the returned run names with \code{list_detector()}
#' and \code{get_segment()} so you don't have to guess valid run identifiers.
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
#' @seealso \code{\link{list_detector}}, \code{\link{get_segment}}
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
#' passing invalid detector names to \code{get_segment()}.
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
#' @seealso \code{\link{list_obsrun}}, \code{\link{get_segment}}
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

#' List timelines available for a given run
#'
#' Return timelines (e.g. "H1_DATA", "H1_BURST_CAT2") available for a run.
#' @param obsrun character run id, e.g. "O1"
#' @param verbose logical for debugging
#' @return character vector of timeline names
list_timelines <- function(obsrun, verbose = FALSE) {
    url <- sprintf("https://gwosc.org/api/v2/runs/%s/timelines", obsrun)
    res <- httr::GET(url, httr::user_agent("R (gwosc client)"))
    if (httr::http_error(res)) stop("HTTP error: ", httr::status_code(res))

    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

    if (verbose) {
        message("Top-level fields: ", paste(names(json), collapse = ", "))
        str(json, max.level = 2)
    }

    # results may be data.frame or list
    if (!"results" %in% names(json)) stop("Unexpected response: no 'results' field")
    results <- json$results

    if (is.data.frame(results)) {
        if (!"name" %in% colnames(results)) stop("Unexpected 'results' structure: no 'name' column")
        timelines <- as.character(results$name)
    } else if (is.list(results)) {
        timelines <- vapply(results, function(x) {
            if (!is.null(x$name)) {
                return(as.character(x$name))
            }
            stop("Unexpected result entry missing 'name'")
        }, FUN.VALUE = character(1))
    } else {
        stop("Unsupported 'results' type: ", class(results)[1])
    }

    names(timelines) <- NULL
    timelines
}

#' Retrieve file metadata (strain-files) from GWOSC API v2
#'
#' Query GWOSC API v2 /strain-files endpoint and return a tidy data.frame of
#' available files matching the requested time window, detectors, sampling
#' frequency, duration and format.
#'
#' Note: this function only returns file metadata (including download URL).
#' It does NOT perform any duty-cycle or timeline-based filtering.
#'
#' @param obsrun Character. Observation run name (e.g. "O1").
#' @param detector Character vector. One or more detectors (e.g. c("H1","L1")).
#' @param GPSstart Integer. Start GPS time.
#' @param GPSend Integer. End GPS time. If omitted, defaults to GPSstart + dur.
#' @param sampling.freq Integer (default 4096). Sampling frequency in Hz (only
#'   4096/16384 supported because the API expects sample-rate in kHz).
#' @param dur Integer (default 4096). Requested segment duration (seconds).
#' @param file.format Character (default "hdf5"). Desired file format.
#'
#' @return data.frame with columns: GPSstart, utc_start, detector, sample_rate_kHz,
#'   url, detail_url (when present), duration.
#' @examples
#' \dontrun{
#' fm <- get_filemeta("O1", c("H1", "L1"), 1126051217, 1126051217 + 86400)
#' }
#' @export
get_filemeta <- function(obsrun, detector, GPSstart, GPSend,
                         sampling.freq = 4096, dur = 4096,
                         file.format = "hdf5") {
    if (missing(GPSend)) GPSend <- GPSstart + dur

    sample_khz <- as.integer(round(sampling.freq / 1000))
    if (!sample_khz %in% c(4L, 16L)) {
        stop("sampling.freq must correspond to 4 kHz (4096) or 16 kHz (16384).")
    }

    url_field <- switch(tolower(file.format),
        "hdf5" = "hdf5_url",
        "gwf"  = "gwf_url",
        "txt"  = "gwf_url",
        stop("Unsupported file.format: ", file.format)
    )

    all_results <- list()

    for (det in detector) {
        message("> Querying GWOSC strain-files for ", det, " (", obsrun, ") ...")
        base <- "https://gwosc.org/api/v2/strain-files"
        q <- list(
            start = as.integer(GPSstart),
            stop = as.integer(GPSend),
            detector = det,
            `sample-rate` = sample_khz,
            duration = as.integer(dur),
            `file-format` = tolower(file.format)
        )

        res <- httr::GET(base, query = q, httr::user_agent("R (gwosc client)"))
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

        if (is.data.frame(results)) {
            df <- as.data.frame(results, stringsAsFactors = FALSE)
        } else if (is.list(results)) {
            df <- tryCatch(dplyr::bind_rows(results), error = function(e) data.frame())
        } else {
            df <- data.frame()
        }
        if (nrow(df) == 0) next

        if (!(url_field %in% colnames(df))) {
            possible <- c("hdf5_url", "gwf_url", "download_url")
            found <- intersect(possible, colnames(df))
            if (length(found) > 0) {
                url_field <- found[1]
            } else {
                warning("No usable download URL field found for detector ", det)
                next
            }
        }

        df2 <- data.frame(
            GPSstart = as.integer(df$gps_start),
            utc_start = if ("utc_start" %in% colnames(df)) as.character(df$utc_start) else NA_character_,
            detector = as.character(df$detector),
            sample_rate_kHz = if ("sample_rate_kHz" %in% colnames(df)) as.integer(df$sample_rate_kHz) else sample_khz,
            url = as.character(df[[url_field]]),
            detail_url = if ("detail_url" %in% colnames(df)) as.character(df$detail_url) else NA_character_,
            stringsAsFactors = FALSE
        )
        df2$duration <- as.integer(dur)
        all_results[[det]] <- df2
    }

    if (length(all_results) == 0) stop("No files found for the requested detectors/time range.")
    out_df <- dplyr::bind_rows(all_results)
    out_df <- dplyr::arrange(out_df, GPSstart)
    message(
        "|> Found ", nrow(out_df), " files (est. size ~ ",
        ifelse(124 * nrow(out_df) > 1024,
            paste0(trunc(124 * nrow(out_df) / 1024), " GB"),
            paste0(124 * nrow(out_df), " MB")
        ), ")"
    )
    return(out_df)
}

#' Retrieve timeline segments for a run/timeline (may include duty_cycle)
#'
#' Query GWOSC timeline segments:
#' /api/v2/runs/<obsrun>/timelines/<timeline>/segments
#'
#' @param obsrun Character run id (e.g. "O1").
#' @param timeline Character timeline id (e.g. "H1_DATA" or "H1_BURST_CAT2").
#' @param GPSstart Integer start GPS.
#' @param GPSend Integer stop GPS.
#' @param compact Logical If TRUE request compact form when supported.
#' @param verbose Logical for debugging.
#' @return data.frame with columns start, stop, and duty_cycle (if present).
#' @examples
#' \dontrun{
#' segs <- get_timeline("O1", "H1_DATA", 1126051217, 1126051217 + 86400)
#' }
#' @export
get_timeline <- function(obsrun, timeline, GPSstart, GPSend,
                         compact = FALSE, verbose = FALSE) {
    base <- sprintf("https://gwosc.org/api/v2/runs/%s/timelines/%s/segments", obsrun, timeline)
    q <- list(start = as.integer(GPSstart), stop = as.integer(GPSend))
    if (compact) q$compact <- "true"
    res <- httr::GET(base, query = q, httr::user_agent("R (gwosc client)"))
    if (httr::http_error(res)) stop("HTTP error: ", httr::status_code(res))
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    if (verbose) {
        message("Segments JSON fields: ", paste(names(json), collapse = ", "))
        str(json, max.level = 1)
    }

    resobj <- if ("results" %in% names(json)) json$results else json

    if (is.list(resobj) && length(resobj) > 0 && is.atomic(resobj[[1]])) {
        mat <- do.call(rbind, lapply(resobj, function(x) as.integer(x[1:2])))
        df <- data.frame(start = as.integer(mat[, 1]), stop = as.integer(mat[, 2]), stringsAsFactors = FALSE)
        return(df)
    }

    if (is.data.frame(resobj)) {
        df <- as.data.frame(resobj, stringsAsFactors = FALSE)
    } else if (is.list(resobj)) {
        df <- tryCatch(dplyr::bind_rows(resobj), error = function(e) data.frame())
    } else {
        stop("Unsupported segments response type: ", class(resobj)[1])
    }

    possible_start_names <- c("start", "gps_start", "segment_start")
    possible_stop_names <- c("stop", "gps_end", "segment_end")
    start_name <- intersect(possible_start_names, colnames(df))
    stop_name <- intersect(possible_stop_names, colnames(df))

    if (length(start_name) == 1 && length(stop_name) == 1) {
        df2 <- data.frame(
            start = as.integer(df[[start_name]]),
            stop = as.integer(df[[stop_name]]),
            stringsAsFactors = FALSE
        )
    } else if (all(c("start", "stop") %in% colnames(df))) {
        df2 <- data.frame(
            start = as.integer(df$start),
            stop = as.integer(df$stop),
            stringsAsFactors = FALSE
        )
    } else if (ncol(df) >= 2) {
        df2 <- data.frame(start = as.integer(df[[1]]), stop = as.integer(df[[2]]), stringsAsFactors = FALSE)
    } else {
        stop("Cannot find start/stop fields in timeline segments response")
    }

    duty_names <- intersect(c("duty_cycle", "duty", "duty_cycle_percent", "dutyCycle", "duty_cycle_frac"), colnames(df))
    if (length(duty_names) >= 1) {
        df2$duty_cycle <- as.numeric(df[[duty_names[1]]])
    } else {
        df2$duty_cycle <- NA_real_
    }

    df2
}


#' Filter a files data.frame by timeline segment duty cycle
#'
#' files_df: output of get_segment() containing at least GPSstart, duration (seconds), detector
#' segs_df: output of get_timeline_segments() containing start, stop, duty_cycle (or NA)
#' duty.cycle.lwr: threshold in same units as segs_df$duty_cycle (e.g. percent 95)
#' require_full_coverage: if TRUE require the file interval [GPSstart, GPSstart+duration) to be fully inside a segment
filter_files_by_dutycycle <- function(files_df, segs_df, duty.cycle.lwr = 95, require_full_coverage = TRUE) {
    if (!all(c("GPSstart", "duration", "detector", "url") %in% colnames(files_df))) {
        stop("files_df must contain columns: GPSstart, duration, detector, url")
    }
    if (!all(c("start", "stop", "duty_cycle") %in% colnames(segs_df))) {
        # allow missing duty_cycle (will be NA) but must have start/stop
        if (!all(c("start", "stop") %in% colnames(segs_df))) stop("segs_df must contain start and stop columns")
        if (!"duty_cycle" %in% colnames(segs_df)) segs_df$duty_cycle <- NA_real_
    }

    # join by detector: segs_df may correspond to single detector; if multi-detector, ensure segs_df has 'detector' column
    if (!"detector" %in% colnames(segs_df)) {
        # assume segs_df corresponds to one detector -> replicate detector from files_df when filtering per detector
        # We'll handle per-detector matching in a loop
        filtered_list <- lapply(unique(files_df$detector), function(det) {
            files_sub <- files_df[files_df$detector == det, , drop = FALSE]
            segs_sub <- segs_df
            # assume segs_df was fetched for 'det' or same for all; if not, caller should supply detector column
            files_keep <- logical(nrow(files_sub))
            for (i in seq_len(nrow(files_sub))) {
                fstart <- as.integer(files_sub$GPSstart[i])
                fend <- as.integer(files_sub$GPSstart[i] + files_sub$duration[i])
                # find candidate segments
                if (require_full_coverage) {
                    cand <- segs_sub$start <= fstart & segs_sub$stop >= fend
                } else {
                    # overlap if segment covers start OR any part overlaps
                    cand <- (segs_sub$start <= fstart & segs_sub$stop >= fstart) |
                        (segs_sub$start < fend & segs_sub$stop >= fend) |
                        (segs_sub$start >= fstart & segs_sub$stop <= fend)
                }
                if (any(cand)) {
                    # pick max duty_cycle among covering segments
                    dvals <- segs_sub$duty_cycle[cand]
                    # If duty_cycle is NA (not provided), treat as fail (i.e., not meeting threshold)
                    if (all(is.na(dvals))) {
                        files_keep[i] <- FALSE
                    } else {
                        files_keep[i] <- max(dvals, na.rm = TRUE) >= duty.cycle.lwr
                    }
                } else {
                    files_keep[i] <- FALSE
                }
            }
            files_sub[files_keep, , drop = FALSE]
        })
        filtered <- do.call(rbind, filtered_list)
    } else {
        # segs_df has detector column -> perform efficient join-like check
        library(dplyr)
        filtered_rows <- lapply(split(files_df, files_df$detector), function(files_sub) {
            det <- unique(files_sub$detector)
            segs_sub <- segs_df[segs_df$detector == det, , drop = FALSE]
            if (nrow(segs_sub) == 0) {
                return(NULL)
            }
            keep_idx <- logical(nrow(files_sub))
            for (i in seq_len(nrow(files_sub))) {
                fstart <- as.integer(files_sub$GPSstart[i])
                fend <- as.integer(files_sub$GPSstart[i] + files_sub$duration[i])
                if (require_full_coverage) {
                    cand <- segs_sub$start <= fstart & segs_sub$stop >= fend
                } else {
                    cand <- (segs_sub$start <= fstart & segs_sub$stop >= fstart) |
                        (segs_sub$start < fend & segs_sub$stop >= fend) |
                        (segs_sub$start >= fstart & segs_sub$stop <= fend)
                }
                if (any(cand)) {
                    dvals <- segs_sub$duty_cycle[cand]
                    if (all(is.na(dvals))) {
                        keep_idx[i] <- FALSE
                    } else {
                        keep_idx[i] <- max(dvals, na.rm = TRUE) >= duty.cycle.lwr
                    }
                } else {
                    keep_idx[i] <- FALSE
                }
            }
            files_sub[keep_idx, , drop = FALSE]
        })
        filtered <- do.call(rbind, filtered_rows)
    }

    # if no files passed filter, return empty data.frame with same columns
    if (is.null(filtered) || nrow(filtered) == 0) {
        return(files_df[0, , drop = FALSE])
    }
    rownames(filtered) <- NULL
    filtered
}

#' Get files (strain-file metadata) optionally filtered by timeline duty-cycle
#'
#' High-level helper that retrieves file metadata (get_filemeta()) and, only
#' when duty.cycle.lwr is provided (non-NULL), applies timeline-based
#' duty-cycle filtering using get_timeline() and filter_files_by_dutycycle().
#'
#' @param obsrun Character run id (e.g. "O1").
#' @param detector Character vector of detectors.
#' @param GPSstart Integer start GPS.
#' @param GPSend Integer stop GPS. If omitted, defaults to GPSstart + dur.
#' @param sampling.freq Integer (default 4096).
#' @param dur Integer (default 4096).
#' @param file.format Character (default "hdf5").
#' @param duty.cycle.lwr Numeric or NULL. If NULL (default) no duty filtering is applied.
#' @param require_full_coverage Logical passed to filter_files_by_dutycycle.
#' @param timeline_name Optional timeline name (single string) or NULL; if NULL a sensible default paste0(det, "_DATA") is used per detector.
#' @param timeline_verbose Logical for timeline debug.
#'
#' @return data.frame of file metadata (filtered if duty.cycle.lwr provided).
#' @examples
#' \dontrun{
#' # No duty filtering:
#' files <- get_segment("O1", c("H1", "L1"), 1126051217, 1126051217 + 86400)
#'
#' # With duty filtering (95%): timeline default is "<DET>_DATA"
#' good <- get_segment("O1", c("H1", "L1"), 1126051217, 1126051217 + 86400,
#'     duty.cycle.lwr = 95
#' )
#' }
#' @export
get_segment <- function(obsrun, detector, GPSstart, GPSend = NULL,
                        sampling.freq = 4096, dur = 4096, file.format = "hdf5",
                        duty.cycle.lwr = NULL, require_full_coverage = TRUE,
                        timeline_name = NULL, timeline_verbose = FALSE) {
    if (missing(GPSend) || is.null(GPSend)) GPSend <- GPSstart + dur

    files_df <- get_filemeta(
        obsrun = obsrun, detector = detector,
        GPSstart = GPSstart, GPSend = GPSend,
        sampling.freq = sampling.freq, dur = dur,
        file.format = file.format
    )

    # If user didn't request duty filtering, return raw file list
    if (is.null(duty.cycle.lwr)) {
        return(files_df)
    }

    # Otherwise perform timeline retrieval + filter
    segs_all <- list()
    for (det in unique(files_df$detector)) {
        tl <- timeline_name
        if (is.null(tl)) tl <- paste0(det, "_DATA")
        segs <- tryCatch(
            {
                get_timeline(
                    obsrun = obsrun, timeline = tl, GPSstart = GPSstart, GPSend = GPSend,
                    compact = FALSE, verbose = timeline_verbose
                )
            },
            error = function(e) {
                warning("Failed to fetch timeline '", tl, "' for detector ", det, ": ", e$message)
                return(NULL)
            }
        )
        if (!is.null(segs) && nrow(segs) > 0) {
            if (!"detector" %in% colnames(segs)) segs$detector <- det
            segs_all[[det]] <- segs
        } else {
            warning("No timeline segments for timeline '", tl, "' detector ", det)
        }
    }

    if (length(segs_all) == 0) {
        warning("No timeline segments retrieved; returning unfiltered file list.")
        return(files_df)
    }

    segs_df <- dplyr::bind_rows(segs_all)

    files_filtered <- filter_files_by_dutycycle(files_df, segs_df,
        duty.cycle.lwr = duty.cycle.lwr,
        require_full_coverage = require_full_coverage
    )
    return(files_filtered)
}


#' Download files listed in a GWOSC segment-data frame
#'
#' Download the files described by the \code{url} column of \code{file.df} and
#' save them into a local directory. This helper is intentionally simple: it
#' takes the \code{url} values returned by \code{get_segment()} and preserves
#' the original file basenames when saving.
#'
#' @param file.df A data.frame as returned by \code{get_segment()} that
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
#' files <- get_segment("O1", "H1", 1126051217, 1126051217 + 4096)
#' outpaths <- download_segment(files, path = "data/O1_H1/")
#' print(outpaths)
#' }
#'
#' @seealso \code{\link{get_segment}}
#' @export
download_segment <- function(file.df, path,
                             background = FALSE,
                             background.wait = 30,
                             timeout = 300) {
    if (!curl::has_internet()) stop("No internet connection")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)

    original.timeout <- getOption("timeout")
    options(timeout = max(timeout, original.timeout))

    filenames <- file.path(path, basename(file.df$url))

    download.expr <- expr({
        for (i in seq_along(file.df$url)) {
            download.file(
                url = file.df$url[i],
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

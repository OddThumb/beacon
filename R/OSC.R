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
#' \dontrun{
#' list_gwosc_param()
#' }
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

#' Get GWOSC parameters for one or more GW events
#'
#' This function queries the GWOSC API v2 to fetch, for each requested event,
#' the latest-event-version GPS time and the preferred (default) physical
#' parameters. It returns either all allowed parameters or a user-specified
#' subset of parameter columns.
#'
#' The function automatically:
#' \itemize{
#'   \item Selects the latest event version per event name.
#'   \item Extracts GPS time from event-version details or, if missing, from the
#'         preferred pipeline parameters (e.g., geocent_time).
#'   \item Uses only preferred (default) parameter values provided by GWOSC.
#'   \item Returns absolute values for lower/upper errors.
#' }
#'
#' @param name Character vector. One or more event names (e.g., \code{c("GW150914","GW151012")}).
#' @param param Character. Either \code{"all"} (default) for all parameters, or a
#'   character vector of column names from \code{list_gwosc_param()} to select.
#'   The output preserves the order of \code{param}.
#'
#' @return A \code{data.frame} whose rows correspond to the requested events
#'   (in the same order as \code{name}) and whose columns are either all allowed
#'   parameters or the requested subset. Row names are set to event names.
#'
#' @examples
#' \dontrun{
#' # All parameters for multiple events
#' get_gwosc_param(c("GW150914", "GW151012"), "all")
#'
#' # A subset of parameters (order preserved)
#' get_gwosc_param("GW150914", c("luminosity_distance", "GPS"))
#'
#' # Single parameter for multiple events
#' get_gwosc_param(c("GW150914", "GW151012"), "chirp_mass_source")
#' }
#'
#' @seealso \code{\link{list_gwosc_param}}
#' @export
get_gwosc_param <- function(name, param = "all") {
    stopifnot(is.character(name), length(name) >= 1L)
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.")

    # Allowed parameter names (physical params + GPS)
    allowed <- list_gwosc_param()

    # Validate `param`: can be "all" or a character vector subset of allowed
    if (!is.character(param) || length(param) < 1L) {
        stop("`param` must be 'all' or a character vector of column names.")
    }
    if (!identical(param, "all")) {
        unknown <- setdiff(param, allowed)
        if (length(unknown) > 0L) {
            stop(
                "Unknown parameter(s): ", paste(unknown, collapse = ", "),
                ". Use list_gwosc_param() to see valid names."
            )
        }
    }

    # --- Helpers (HTTP/JSON and coercions) ---
    get_json <- function(url) jsonlite::fromJSON(url, simplifyVector = TRUE)
    enc <- function(x) utils::URLencode(x, reserved = TRUE)
    base <- "https://gwosc.org/api/v2"
    as_num <- function(x) {
        if (is.null(x) || length(x) == 0) {
            return(NA_real_)
        }
        suppressWarnings(as.numeric(x)[1L])
    }

    # --- Core worker: fetch one event and return a one-row data.frame ---
    .get_one <- function(one_name) {
        # Initialize an empty row with all allowed columns (filled with NA)
        make_empty_row <- function() {
            lst <- setNames(vector("list", length(allowed)), allowed)
            for (nm in allowed) lst[[nm]] <- NA_real_
            df <- as.data.frame(lst, check.names = FALSE, optional = TRUE)
            rownames(df) <- one_name
            df
        }

        # 1) Resolve the latest version for this event
        ev <- tryCatch(get_json(sprintf("%s/events/%s", base, enc(one_name))), error = function(e) NULL)
        if (is.null(ev) || is.null(ev$versions) || length(ev$versions) == 0) {
            return(make_empty_row())
        }
        vdf <- as.data.frame(ev$versions)
        vmax <- max(as.integer(vdf$version), na.rm = TRUE)

        # 2) GPS from event-version details (primary)
        evv <- tryCatch(
            get_json(sprintf("%s/event-versions/%s-v%d?format=api", base, enc(one_name), vmax)),
            error = function(e) NULL
        )
        GPS <- NA_real_
        if (!is.null(evv)) {
            if (!is.null(evv$gps)) GPS <- as_num(evv$gps)
            if (is.na(GPS) && !is.null(evv$gps_time)) GPS <- as_num(evv$gps_time)
        }

        # 3) Preferred/default parameters (used for physical params)
        dpar <- tryCatch(
            get_json(sprintf("%s/event-versions/%s-v%d/default-parameters", base, enc(one_name), vmax)),
            error = function(e) NULL
        )
        pref <- if (!is.null(dpar) && !is.null(dpar$results)) as.data.frame(dpar$results, stringsAsFactors = FALSE) else NULL

        # 4) If GPS still NA, try preferred pipelines from /parameters (time-like names)
        if (is.na(GPS)) {
            parlist <- tryCatch(
                get_json(sprintf("%s/event-versions/%s-v%d/parameters", base, enc(one_name), vmax)),
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

        # Helper to pull best/err/unit for a parameter from preferred list
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

        # 5) Build one-row data.frame for this event
        row <- setNames(vector("list", length(allowed)), allowed)
        for (nm in allowed) row[[nm]] <- NA_real_
        row[["GPS"]] <- GPS

        bases <- c(
            "mass_1_source", "mass_2_source", "chi_eff", "total_mass_source",
            "chirp_mass_source", "chirp_mass", "redshift", "far", "p_astro",
            "final_mass_source", "network_matched_filter_snr", "luminosity_distance"
        )
        for (b in bases) {
            t <- get_triplet(b)
            row[[b]] <- t$val
            lo_nm <- paste0(b, "_lower")
            up_nm <- paste0(b, "_upper")
            unit_nm <- paste0(b, "_unit")
            if (lo_nm %in% allowed) row[[lo_nm]] <- t$lo
            if (up_nm %in% allowed) row[[up_nm]] <- t$up
            if (unit_nm %in% allowed) row[[unit_nm]] <- t$unit
        }

        df <- as.data.frame(row, check.names = FALSE, optional = TRUE)
        rownames(df) <- one_name
        df
    }

    # --- Vectorized over `name` ---
    rows <- lapply(name, .get_one)
    out <- do.call(rbind, rows)

    # Apply `param` selection at the end to preserve row order and column order
    if (identical(param, "all")) {
        return(out)
    } else {
        # Ensure all requested columns exist; if not, create NA columns
        missing_cols <- setdiff(param, names(out))
        if (length(missing_cols) > 0L) {
            for (mc in missing_cols) out[[mc]] <- NA_real_
        }
        # Return only requested columns, in the requested order
        return(out[, param, drop = FALSE])
    }
}

#' Download dataset/strain-file for a GWOSC event (v2, sampling.freq-aware)
#'
#' For a given GW event name (e.g. "GW150914") and detector (e.g. "H1"),
#' this function tries several GWOSC v2 endpoints in a sensible order to find
#' a matching strain-file URL and download it. Among multiple candidates, it
#' prioritizes the one whose sample rate is closest to \code{sampling.freq}.
#'
#' The function attempts (in order):
#'  1. /api/v2/events/<EVENT>/strain-files
#'  2. /api/v2/events/<EVENT> -> latest version -> /event-versions/<EVENT>-vN/strain-files
#'  3. /api/v2/event-versions/<EVENT>/dataset -> use \code{dataset$strain_files_url}
#'
#' If \code{load=TRUE} and the file is HDF5, the function returns a \code{ts}
#' object by delegating to \code{read_H5(file, sampling.freq, dq.level)}.
#'
#' @param event_name Character. GWOSC event name, e.g. "GW150914".
#' @param det Character. Detector name, e.g. "H1".
#' @param path Character. Directory to save file (created if missing).
#' @param file.format Character. "hdf5" (default), "gwf", or "txt".
#' @param sampling.freq Numeric scalar (Hz). Desired sampling frequency used to
#'   prioritize candidates. Default \code{4096}.
#' @param timeout Integer. Download timeout seconds (default 300).
#' @param load Logical. If TRUE and HDF5, load via \code{read_H5()} and return a \code{ts}.
#' @param remove Logical. If \code{load=TRUE} and \code{remove=TRUE}, delete file after loading.
#' @param verbose Logical. Print progress messages.
#'
#' @return If \code{load=FALSE}, normalized path to downloaded file (character).
#'         If \code{load=TRUE} and HDF5, a \code{ts} object from \code{read_H5()}.
#' @export
download_event <- function(event_name,
                           det,
                           path = ".",
                           file.format = "hdf5",
                           sampling.freq = 4096,
                           timeout = 300,
                           load = FALSE,
                           remove = FALSE,
                           verbose = TRUE) {
    if (!curl::has_internet()) stop("No internet connection")
    if (missing(event_name) || missing(det)) stop("event_name and det must be provided")
    if (!is.numeric(sampling.freq) || length(sampling.freq) != 1L || is.na(sampling.freq) || sampling.freq <= 0) {
        stop("`sampling.freq` must be a positive numeric scalar (Hz).")
    }
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)

    # [ Helpers ]
    # Defensive JSON fetcher
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

    # Try endpoints in order
    tried_urls <- character(0)
    sf_results <- NULL

    # 1) Event-level strain-files
    ev_sf_url <- sprintf("https://gwosc.org/api/v2/events/%s/strain-files", utils::URLencode(event_name, reserved = TRUE))
    tried_urls <- c(tried_urls, ev_sf_url)
    if (verbose) message("> Trying event-level strain-files: ", ev_sf_url)
    r1 <- fetch_json(ev_sf_url, verbose_local = verbose)
    if (isTRUE(r1$ok) && !is.null(r1$json)) {
        sf_results <- if ("results" %in% names(r1$json)) r1$json$results else r1$json
    } else {
        if (verbose) message("|> event-level request failed (status=", r1$status, "). Will try versions/dataset.")
    }

    # 2) Event -> versions -> chosen version -> strain-files
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
                vnums <- vapply(versions, function(v) if (!is.null(v$version)) as.integer(v$version) else NA_integer_, integer(1))
                chosen_idx <- if (all(is.na(vnums))) 1L else which.max(vnums)
                chosen_ver <- versions[[chosen_idx]]

                if (!is.null(chosen_ver$strain_files_url) && nzchar(as.character(chosen_ver$strain_files_url))) {
                    sf_url <- as.character(chosen_ver$strain_files_url)
                    tried_urls <- c(tried_urls, sf_url)
                    if (verbose) message("> Trying version-provided strain_files_url: ", sf_url)
                    r3 <- fetch_json(sf_url, verbose_local = verbose)
                    if (isTRUE(r3$ok) && !is.null(r3$json)) {
                        sf_results <- if ("results" %in% names(r3$json)) r3$json$results else r3$json
                    }
                }

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

    # 3) Event-version dataset -> strain_files_url
    if (is.null(sf_results) || (is.list(sf_results) && length(sf_results) == 0)) {
        ds_url <- sprintf("https://gwosc.org/api/v2/event-versions/%s/dataset", utils::URLencode(event_name, reserved = TRUE))
        tried_urls <- c(tried_urls, ds_url)
        if (verbose) message("> Trying event-version dataset URL: ", ds_url)
        r5 <- fetch_json(ds_url, verbose_local = verbose)
        if (isTRUE(r5$ok) && !is.null(r5$json)) {
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

    # No results at all
    if (is.null(sf_results) || length(sf_results) == 0) {
        stop("No strain-files entries found for event '", event_name, "'. Tried URLs:\n", paste(tried_urls, collapse = "\n"))
    }

    # Coerce to data.frame & normalize columns
    if (is.data.frame(sf_results)) {
        sf_df <- as.data.frame(sf_results, stringsAsFactors = FALSE)
    } else if (is.list(sf_results)) {
        sf_df <- tryCatch(dplyr::bind_rows(sf_results), error = function(e) data.frame())
    } else {
        stop("Unexpected shape for strain-files response.")
    }
    if (nrow(sf_df) == 0) stop("No strain-file records after coercion for event ", event_name)

    if (!"gps_start" %in% colnames(sf_df) && "start" %in% colnames(sf_df)) sf_df$gps_start <- sf_df$start
    if (!"detector" %in% colnames(sf_df) && "detectors" %in% colnames(sf_df)) {
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

    # Derive sample_rate_hz (prefer kHz fields if present)
    sr_hz <- rep(NA_real_, nrow(sf_df))
    if ("sample_rate_kHz" %in% names(sf_df)) sr_hz <- as.numeric(sf_df$sample_rate_kHz) * 1000
    if ("sample_rate_khz" %in% names(sf_df)) sr_hz <- ifelse(is.na(sr_hz), as.numeric(sf_df$sample_rate_khz) * 1000, sr_hz)
    if ("sample_rate" %in% names(sf_df)) sr_hz <- ifelse(is.na(sr_hz), suppressWarnings(as.numeric(sf_df$sample_rate)), sr_hz)
    if ("sampleRate" %in% names(sf_df)) sr_hz <- ifelse(is.na(sr_hz), suppressWarnings(as.numeric(sf_df$sampleRate)), sr_hz)
    sf_df$sample_rate_hz <- sr_hz

    # Determine URL field
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

    # Filter/prioritize by detector and sampling.freq

    cand <- sf_df
    if (!is.null(det)) {
        cand <- cand[as.character(cand$detector) == as.character(det), , drop = FALSE]
    }
    if (nrow(cand) == 0) stop("No strain-file entries for detector=", det, " for event ", event_name)

    # Prefer closest sample rate to sampling.freq (if sample rates known)
    if (any(!is.na(cand$sample_rate_hz))) {
        known <- !is.na(cand$sample_rate_hz)
        diffs <- abs(cand$sample_rate_hz[known] - sampling.freq)
        nearest <- cand$sample_rate_hz[known][which.min(diffs)]
        keep <- cand$sample_rate_hz == nearest
        cand <- cand[ifelse(is.na(keep), FALSE, keep), , drop = FALSE]
    }
    if (nrow(cand) == 0) stop("No strain-file entries after sampling.freq prioritization for event ", event_name)

    # If multiple, refine by gps_start proximity (stable pick)
    chosen_idx <- 1L
    if (nrow(cand) > 1 && "gps_start" %in% colnames(cand)) {
        gps_vals <- suppressWarnings(as.integer(cand$gps_start))
        if (all(!is.na(gps_vals))) chosen_idx <- which.min(abs(gps_vals - median(gps_vals)))
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

    # Load via read_H5 if requested
    if (load) {
        if (tolower(file.format) != "hdf5") stop("'load=TRUE' only supported for hdf5 files")
        res <- read_H5(destfile, sampling.freq = sampling.freq, dq.level = "all")
        if (remove) {
            file.remove(destfile)
            if (verbose) message("|> removed ", destfile)
        }
        return(res)
    }

    normalizePath(destfile)
}

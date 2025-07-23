# MISCELLANEOUS ----

#' Create WD recursively with alternative option
#'
#' @param root A character. A root directory (full path).
#' @param sub A character. A sub-directory (only name).
#' @param subsub A character. A sub-sub-directory (only name).
#' @param alternative A logical (default: FALSE). If directory is already exists, it puts suffix, e.g. _1, _2 ...
#' @param set A logical (default: TRUE). Whether automatically set the working directory after creating it.
#' @return A character. A final working directory path, if 'set=TRUE' (for any use for later).
#' @export
create_wd <- function(root, sub, subsub, alternative = F, set = T) {
    dir <- paste(root, sub, subsub, sep = '/')

    if (!dir.exists(dir)) {
        dir.create(dir, recursive = T)
        message(dir, " is created")
    } else if (dir.exists(dir) & alternative) {
        i <- 1
        cond <- T
        alter.dir <- dir
        while (cond) {
            message(
                "'",
                alter.dir,
                "' already exists\n Try with ",
                paste(dir, i, sep = '_')
            )
            alter.dir <- paste(dir, i, sep = '_')
            cond <- dir.exists(alter.dir)
            if (!cond) {
                dir.create(alter.dir, recursive = T)
                message("\n> ", alter.dir, " is created")
            }
            i <- i + 1
        }
        dir <- alter.dir
    } else {
        warning("'", dir, "' already exists")
    }

    if (set) {
        setwd(dir)
        message("> ", dir, " is now working directory")
        return(dir)
    }
}

#' Copy attributes of 'which's from 'ref' to 'target'
#'
#' @export
copy_attr <- function(target, ref, which) {
    for (att in which) {
        attr(target, att) <- attr(ref, att)
    }
    target
}

#' Wrapper of unique(diff(x))
#'
#' @export
uniqdif <- function(x, tol = 1e-8) {
    dx <- diff(x)
    ref <- dx[1]
    unique_dx <- unique(dx[abs(dx - ref) > tol])
    if (length(unique_dx) == 0) {
        return(ref)
    } else {
        warning("Non-uniform spacing detected.")
        return(unique(dx))
    }
}

#' Simple initial value
#'
#' @param x A vector.
#' @param n A numeric (default: 1). A number of head.
#' @return Numeric(s). n initial times.
#' @export
vi <- function(x, n = 1) {
    head(x, n)
}

#' Simple final value
#'
#' @param x A vector.
#' @param n A numeric (default: 1). A number of tail.
#' @return Numeric(s). n final times.
#' @export
vf <- function(x, n = 1) {
    tail(x, n)
}

#' Simple value range
#'
#' @param x A vector.
#' @return A vector of length 2 which is equivalent to `c(ti(ts), tf(ts))`.
#' @export
vr <- function(x) {
    c(vi(x), vf(x))
}

#' Simple value length from first data point to last.
#'
#' @param ts A time series (`ts`) object.
#' @return A vector of length 2 which is equivalent to `c(ti(ts), tf(ts))`.
#' @export
vl <- function(x) {
    diff(vr(x))
}


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

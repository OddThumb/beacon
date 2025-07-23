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

#' Paste WD to file name
#'
#' @param file A character. File name.
#' @param prefix A character. '{prefix}_file' will be the final file name.
#' @param dir A character (default: getwd()). Working directory will be pasted.
#' @return A character.
#' @export
paste_wd <- function(file, prefix = NULL, dir = getwd()) {
    if (is.null(prefix)) {
        paste(dir, '/', file, sep = '')
    } else {
        paste(dir, '/', paste(prefix, "_", file, sep = ''), sep = '')
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
    checked <- devtools::check(beacon.path)

    if (length(checked$errors) == 0) {
        message("3) Re-installing 'beacon'...")
        devtools::install(beacon.path)
    } else {
        cat(checked$errors)
        stop("\nPlease check output")
    }
}

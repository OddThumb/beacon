#' Create Working Directory (WD) Recursively with Optional Suffix
#'
#' Create a nested directory from root/sub/subsub. If the directory already exists
#' and `alternative = TRUE`, a new directory will be created with an appended numeric suffix.
#'
#' @param root A character. Root directory (absolute path).
#' @param sub A character. Sub-directory name (not a path).
#' @param subsub A character. Sub-sub-directory name.
#' @param alternative A logical (default: FALSE). If TRUE, appends numeric suffix to avoid overwrite.
#' @param set A logical (default: TRUE). If TRUE, sets the created directory as working directory.
#'
#' @return A character string of the final working directory path (only if `set = TRUE`).
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

#' Combine Working Directory Path with File Name
#'
#' Concatenate a working directory with a file name and optional prefix.
#'
#' @param file A character. File name (with or without extension).
#' @param prefix A character (optional). If provided, result becomes "{prefix}_{file}".
#' @param dir A character. Working directory path (default: current working directory).
#'
#' @return A character string representing the full file path.
#' @export
paste_wd <- function(file, prefix = NULL, dir = getwd()) {
    if (is.null(prefix)) {
        paste(dir, '/', file, sep = '')
    } else {
        paste(dir, '/', paste(prefix, "_", file, sep = ''), sep = '')
    }
}

#' Copy Attributes from Reference Object
#'
#' Copy specified attributes from a reference object to a target object.
#'
#' @param target An object to which attributes will be added.
#' @param ref A reference object from which attributes will be copied.
#' @param which A character vector specifying the names of attributes to copy.
#'
#' @return The modified `target` object with selected attributes copied from `ref`.
#' @export
copy_attr <- function(target, ref, which) {
    for (att in which) {
        attr(target, att) <- attr(ref, att)
    }
    target
}

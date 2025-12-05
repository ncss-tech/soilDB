#' Get soilDB User Directory Path
#'
#' @description Wrapper around `tools::R_user_dir()` for soilDB-specific file
#'   storage in a platform-specific, user-level directory. Requires R >= 4.0.
#'
#'   These directories can be used for storing assets used as input to other
#'   soilDB functions, or for caching results to allow for offline use or fewer
#'   repeated requests to remote sources.
#'
#'   Use argument `remove=TRUE` to delete files stored in the specified
#'   directories.
#'   
#' @details If the directory path does not exist it is created (recursively)
#'   when `create=TRUE`. Output paths are normalized using
#'   `normalizePath()` and the specified path separator (`fsep`).
#'
#' @param which _character_. One of: `"data"`, `"config"`, or `"cache"`. Passed
#'   to `tools::R_user_dir()`.
#' @param ... _character_. Sub-directories. Additional arguments are passed to
#'   `file.path()`
#' @param create _logical_. When `TRUE` (default) the directory path is created
#'   (recursively).
#' @param remove _logical_. When `TRUE` the contents of the folder(s) are
#'   deleted (recursively). Default: `FALSE`.
#' @param fsep _character_. File path separator. Passed to `file.path()` and
#'   `normalizePath()`. Default: `.Platform$file.sep`
#' @param mustWork _logical_. Throw error if path cannot be normalized; passed
#'   to `normalizePath()`. Default `NA` generates a warning.
#'
#' @return _character_. File paths within specified soilDB user directory. When
#'   `remove=TRUE` result is `NULL`.
#' 
#' @export
#' @author Andrew Gene Brown
#' @examples
#'
#' soilDB_user_dir("data", c("dataset1", "dataset2"), "source", create = FALSE)
#' 
soilDB_user_dir <- function(which = c("data", "config", "cache"),
                            ...,
                            create = TRUE,
                            remove = FALSE,
                            fsep = .Platform$file.sep,
                            mustWork = NA) {
  which <- match.arg(which, c("data", "config", "cache"))
  
  dir <- tools::R_user_dir("soilDB", which)
  
  data_dir <- file.path(dir, ..., fsep = fsep)
  if (isTRUE(remove)) {
    sapply(data_dir, unlink, recursive = TRUE)
    return(NULL)
  }
  if (isTRUE(create)) {
    sapply(data_dir,
           dir.create,
           showWarnings = FALSE,
           recursive = TRUE)
  }
  normalizePath(data_dir, winslash = fsep, mustWork = mustWork)
}

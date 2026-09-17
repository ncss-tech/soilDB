#' List cached Web Soil Survey ZIP archives
#'
#' List cached Web Soil Survey ZIP archives stored by `downloadSSURGO()`.
#' The same cache policy module is used by `downloadSSURGO()` to place and pick ZIPs.
#'
#' @param areasymbols _character_. Optional areasymbol filter.
#' @param fiscal_year _character_ or _integer_. Optional fiscal year filter such as `"FY26"`.
#' @param db _character_. One or more of `"SSURGO"` or `"STATSGO"`. Defaults to both.
#' @param pattern _character_. Optional regular expression used to filter cached ZIP filenames.
#' @param cache_dir _character_. Optional cache root. Defaults to the soilDB WSS cache.
#'
#' @return A `data.frame` of cached ZIP metadata.
#' @export
list_WSS_cache <- function(areasymbols = NULL,
                           fiscal_year = NULL,
                           db = c("SSURGO", "STATSGO"),
                           pattern = NULL,
                           cache_dir = NULL) {
  db <- match.arg(toupper(db), c("SSURGO", "STATSGO"), several.ok = TRUE)

  res <- .wss_cache_entries(cache_dir = cache_dir, create = FALSE)
  res <- .wss_cache_select(
    res,
    areasymbols = areasymbols,
    fiscal_year = fiscal_year,
    db = db,
    pattern = pattern
  )

  rownames(res) <- NULL
  return(res)
}

#' Clean cached Web Soil Survey ZIP archives
#'
#' Clean cached Web Soil Survey ZIP archives stored by `downloadSSURGO()`.
#' By default, this will clear the entire cache unless filtered by arguments.
#'
#' @param areasymbols _character_. Optional areasymbol filter.
#' @param fiscal_year _character_ or _integer_. Optional fiscal year filter such as `"FY26"`.
#' @param db _character_. One or more of `"SSURGO"` or `"STATSGO"`. Defaults to both.
#' @param pattern _character_. Optional regular expression used to filter cached ZIP filenames.
#' @param latest_only _logical_. If `TRUE`, preserves the most recent ZIP for each area/fiscal year and deletes only outdated/orphaned ones. Default: `FALSE`.
#' @param cache_dir _character_. Optional cache root. Defaults to the soilDB WSS cache.
#'
#' @return The removed file paths are returned invisibly.
#' @export
clear_WSS_cache <- function(areasymbols = NULL,
                            fiscal_year = NULL,
                            db = c("SSURGO", "STATSGO"),
                            pattern = NULL,
                            latest_only = FALSE,
                            cache_dir = NULL) {
  db <- match.arg(toupper(db), c("SSURGO", "STATSGO"), several.ok = TRUE)

  res <- .wss_cache_entries(cache_dir = cache_dir, create = TRUE)
  
  candidates <- .wss_cache_select(
    res,
    areasymbols = areasymbols,
    fiscal_year = fiscal_year,
    db = db,
    pattern = pattern,
    latest_only = FALSE
  )
  
  if (isTRUE(latest_only)) {
    latest <- .wss_cache_select(candidates, latest_only = TRUE, db = db)
    removed <- setdiff(candidates$file, latest$file)
  } else {
    removed <- unique(candidates$file)
  }

  if (length(removed) > 0) {
    unlink(removed)
  }
  invisible(removed)
}

.wss_cache_root <- function(cache_dir = NULL, create = TRUE) {
  if (is.null(cache_dir)) {
    cache_dir <- getOption("soilDB.WSS.cache_dir")
  }

  if (is.null(cache_dir)) {
    cache_dir <- soilDB_user_dir("cache", "WSS", create = create)
  } else if (isTRUE(create)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  normalizePath(cache_dir, winslash = .Platform$file.sep, mustWork = FALSE)
}

.wss_cache_download_urls <- function(urls,
                                     cache_root,
                                     cache_mode = TRUE,
                                     force = FALSE,
                                     quiet = FALSE) {
  if (length(urls) == 0) {
    return(character(0))
  }

  destfiles <- vapply(
    urls,
    .wss_cache_destfile,
    character(1),
    cache_root = cache_root,
    cache_mode = cache_mode
  )

  for (i in seq_along(urls)) {
    destfile <- destfiles[i]
    res <- .wss_cache_download_one(
      url = urls[i],
      destfile = destfile,
      force = force,
      quiet = quiet
    )
    if (!is.null(res)) {
      destfiles[i] <- res
    }
  }

  destfiles[file.exists(destfiles)]
}

.wss_cache_entries <- function(cache_dir = NULL, create = FALSE) {
  cache_dir <- .wss_cache_root(cache_dir, create = create)

  if (!dir.exists(cache_dir)) {
    return(data.frame())
  }

  files <- list.files(cache_dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    return(data.frame())
  }

  res <- do.call(rbind, lapply(files, .parse_wss_cache_file))
  if (is.null(res)) {
    return(data.frame())
  }

  rownames(res) <- NULL
  res
}

.wss_cache_download_one <- function(url, destfile, force = FALSE, quiet = FALSE) {
  if (!dir.exists(dirname(destfile))) {
    dir.create(dirname(destfile), recursive = TRUE)
  }

  if (file.exists(destfile) && !isTRUE(force)) {
    return(destfile)
  }

  if (isTRUE(force) && file.exists(destfile)) {
    unlink(destfile, recursive = TRUE, force = TRUE)
  }

  tmp_destfile <- destfile
  if (isTRUE(force) || file.exists(destfile)) {
    tmp_destfile <- tempfile(pattern = "wss_", tmpdir = dirname(destfile), fileext = ".zip")
  }

  download_ok <- try(
    curl::curl_download(
      url,
      destfile = tmp_destfile,
      quiet = quiet,
      mode = "wb",
      handle = .soilDB_curl_handle()
    ),
    silent = quiet
  )

  if (inherits(download_ok, "try-error")) {
    if (!identical(tmp_destfile, destfile) && file.exists(tmp_destfile)) {
      unlink(tmp_destfile)
    }
    if (isTRUE(force)) {
      stop("Forced re-download of SSURGO ZIP files failed: ", destfile, call. = FALSE)
    }
    return(NULL)
  }

  if (!identical(tmp_destfile, destfile) && file.exists(tmp_destfile)) {
    if (file.exists(destfile)) {
      file.remove(destfile)
    }
    if (!file.rename(tmp_destfile, destfile)) {
      if (file.exists(tmp_destfile)) {
        unlink(tmp_destfile)
      }
      stop("Failed to move downloaded ZIP into place: ", destfile, call. = FALSE)
    }
  }

  destfile
}

.wss_cache_select <- function(entries,
                              areasymbols = NULL,
                              fiscal_year = NULL,
                              db = c("SSURGO", "STATSGO"),
                              pattern = NULL,
                              include_template = NULL,
                              latest_only = FALSE) {
  if (is.null(entries) || nrow(entries) == 0) {
    return(entries)
  }

  db <- match.arg(toupper(db), c("SSURGO", "STATSGO"), several.ok = TRUE)
  res <- entries[entries$db %in% db, , drop = FALSE]

  if (!is.null(areasymbols)) {
    res <- res[toupper(res$areasymbol) %in% toupper(areasymbols), , drop = FALSE]
  }

  if (!is.null(fiscal_year)) {
    fy <- .normalize_wss_fiscal_year(fiscal_year)
    res <- res[res$fiscal_year == fy, , drop = FALSE]
  }

  if (!is.null(pattern)) {
    res <- res[grepl(pattern, res$basename), , drop = FALSE]
  }

  if (!is.null(include_template) && db == "SSURGO" && "template" %in% names(res)) {
    res <- res[res$template == isTRUE(include_template), , drop = FALSE]
  }

  if (latest_only && nrow(res) > 0) {
    res <- res[order(res$areasymbol, res$template, -as.numeric(res$saverest), res$basename), , drop = FALSE]
    keep <- !duplicated(interaction(res$areasymbol, res$template, drop = TRUE, lex.order = TRUE))
    res <- res[keep, , drop = FALSE]
  } else if (nrow(res) > 0) {
    res <- res[order(res$fiscal_year, res$areasymbol, res$saverest, res$basename), , drop = FALSE]
  }

  res
}

.wss_cache_destfile <- function(url, cache_root, cache_mode = TRUE) {
  bn <- .wss_url_filename(url)

  if (!isTRUE(cache_mode)) {
    return(file.path(cache_root, bn))
  }

  parsed <- .parse_wss_filename(bn)
  if (is.null(parsed) || is.na(parsed$saverest)) {
    stop("Unable to derive a WSS cache path from URL: ", bn, call. = FALSE)
  }

  file.path(cache_root, parsed$fiscal_year, bn)
}

.wss_url_filename <- function(url) {
  m <- regexpr("wss_(?:SSA|gsmsoil).*\\.zip", url, perl = TRUE, ignore.case = TRUE)
  if (m[1] < 0) {
    return(sub("^.*[\\\\/]", "", url))
  }
  regmatches(url, m)[[1]]
}

.normalize_wss_fiscal_year <- function(fiscal_year) {
  if (is.null(fiscal_year) || length(fiscal_year) == 0) {
    return(character(0))
  }
  fy_str <- trimws(as.character(fiscal_year))
  fy_str <- sub("^FY", "", fy_str, ignore.case = TRUE)
  yr <- suppressWarnings(as.integer(fy_str))
  yr <- yr %% 100L
  sprintf("FY%02d", yr)
}

.wss_fiscal_year <- function(x) {
  x <- as.Date(x)
  yr <- as.integer(format(x, "%Y"))
  mo <- as.integer(format(x, "%m"))
  fy <- ifelse(mo >= 10L, yr + 1L, yr)
  sprintf("FY%02d", fy %% 100L)
}

.parse_wss_filename <- function(bn) {
  if (grepl("^wss_gsmsoil_", bn, ignore.case = TRUE)) {
    m <- regexec("^wss_gsmsoil_(.+)_\\[(.+)\\]\\.zip$", bn, ignore.case = TRUE)
    vals <- regmatches(bn, m)[[1]]
    if (length(vals) != 3) {
      return(NULL)
    }
    areasymbol <- vals[2]
    saverest_str <- vals[3]
    db <- "STATSGO"
    template <- FALSE
  } else if (grepl("^wss_SSA_", bn, ignore.case = TRUE)) {
    m <- regexec("^wss_SSA_(.+?)(?:_soildb_([A-Z]{2}|NPS)_2003)?_\\[(.+)\\]\\.zip$", bn, ignore.case = TRUE)
    vals <- regmatches(bn, m)[[1]]
    if (length(vals) != 4) {
      return(NULL)
    }
    areasymbol <- vals[2]
    saverest_str <- vals[4]
    db <- "SSURGO"
    template <- !is.na(vals[3]) && nzchar(vals[3])
  } else {
    return(NULL)
  }

  saverest <- as.Date(saverest_str, format = "%m/%d/%Y %H:%M:%S")
  if (is.na(saverest)) {
    saverest <- as.Date(saverest_str)
  }
  
  list(
    basename = bn,
    db = db,
    areasymbol = toupper(areasymbol),
    saverest = saverest,
    fiscal_year = .wss_fiscal_year(saverest),
    template = template
  )
}

.parse_wss_cache_file <- function(file) {
  bn <- basename(file)
  parsed <- .parse_wss_filename(bn)
  
  if (is.null(parsed)) {
    return(NULL)
  }

  data.frame(
    file = normalizePath(file, winslash = .Platform$file.sep, mustWork = FALSE),
    basename = parsed$basename,
    db = parsed$db,
    areasymbol = parsed$areasymbol,
    saverest = parsed$saverest,
    fiscal_year = parsed$fiscal_year,
    template = parsed$template,
    stringsAsFactors = FALSE
  )
}

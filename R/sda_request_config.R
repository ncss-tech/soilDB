.sda_state <- new.env(parent = emptyenv())

.SDA_DEFAULT_URL <- "https://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest"
.SDA_OFFICIAL_HOST <- "sdmdataaccess.sc.egov.usda.gov"
.SDA_RESERVED_HEADERS <- c("content-type", "content-length", "host", "transfer-encoding")

#' @noRd
#' @keywords internal
.sda_parse_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(trimws(url))) {
    stop("`soilDB.sda.tabular.post.rest.url` must be a single non-empty character string.", call. = FALSE)
  }
  m <- regexec("^(https?)://(\\[[^]]+\\]|[^/:?#]+)(?::([0-9]+))?(/.*)?$", url,
               ignore.case = TRUE, perl = TRUE)
  parts <- regmatches(url, m)[[1]]
  if (length(parts) == 0L || grepl("@", parts[3], fixed = TRUE)) {
    stop("`soilDB.sda.tabular.post.rest.url` must be a valid HTTP or HTTPS URL.", call. = FALSE)
  }
  host <- tolower(parts[3])
  if (startsWith(host, "[") && endsWith(host, "]")) {
    host <- substring(host, 2L, nchar(host) - 1L)
  }
  list(scheme = tolower(parts[2]), host = host)
}

#' @noRd
#' @keywords internal
.sda_request_config <- function(default_ua = NULL) {
  if (is.null(default_ua)) {
    default_ua <- paste0(
      "soilDB/", utils::packageVersion("soilDB"),
      " (", R.version$platform, ")"
    )
  }

  raw_url <- getOption("soilDB.sda.tabular.post.rest.url", default = .SDA_DEFAULT_URL)
  if (is.null(raw_url)) raw_url <- .SDA_DEFAULT_URL
  parsed <- .sda_parse_url(raw_url)

  raw_hdrs <- getOption("soilDB.sda.headers", default = NULL)
  if (is.function(raw_hdrs)) raw_hdrs <- raw_hdrs()

  hdr_vec <- character(0)
  if (!is.null(raw_hdrs)) {
    if (!is.character(raw_hdrs) && !is.list(raw_hdrs)) {
      stop("`soilDB.sda.headers` must be a named character vector, named list of single strings, or a 0-argument function returning one.", call. = FALSE)
    }
    if (length(raw_hdrs) > 0L) {
      nms <- names(raw_hdrs)
      if (is.null(nms) || any(is.na(nms)) || any(!nzchar(trimws(nms)))) {
        stop("All elements of `soilDB.sda.headers` must have non-empty names.", call. = FALSE)
      }
      nms_trimmed <- trimws(nms)
      if (anyDuplicated(tolower(nms_trimmed)) > 0L) {
        stop("Header names in `soilDB.sda.headers` must be unique (case-insensitive).", call. = FALSE)
      }
      for (i in seq_along(raw_hdrs)) {
        val <- raw_hdrs[[i]]
        if (!is.character(val) || length(val) != 1L || is.na(val)) {
          stop(
            sprintf("Header '%s' in `soilDB.sda.headers` must have a length-1, non-NA character value.", nms_trimmed[i]),
            call. = FALSE
          )
        }
        if (grepl("[\r\n]", nms_trimmed[i]) || grepl("[\r\n]", val)) {
          stop("Header names and values in `soilDB.sda.headers` must not contain line breaks.", call. = FALSE)
        }
      }
      hdr_vec <- stats::setNames(as.character(unlist(raw_hdrs, use.names = FALSE)), nms_trimmed)
    }
  }

  raw_tok <- getOption("soilDB.sda.token", default = NULL)
  if (is.function(raw_tok)) raw_tok <- raw_tok()
  if (is.null(raw_tok)) {
    env_tok <- Sys.getenv("SOILDB_SDA_TOKEN", unset = "")
    if (nzchar(env_tok)) raw_tok <- env_tok
  } else if (!is.character(raw_tok) || length(raw_tok) != 1L || is.na(raw_tok) || !nzchar(raw_tok)) {
    stop("`soilDB.sda.token` must resolve to a single non-empty, non-NA character string.", call. = FALSE)
  }
  if (!is.null(raw_tok) && grepl("[\r\n]", raw_tok)) {
    stop("`soilDB.sda.token` must not contain line breaks.", call. = FALSE)
  }

  if (length(hdr_vec) > 0L) {
    is_res <- tolower(names(hdr_vec)) %in% .SDA_RESERVED_HEADERS
    if (any(is_res)) {
      dropped <- names(hdr_vec)[is_res]
      warning(
        sprintf("Dropping reserved header(s) from `soilDB.sda.headers`: %s", paste(dropped, collapse = ", ")),
        call. = FALSE
      )
      hdr_vec <- hdr_vec[!is_res]
    }
  }

  if (!is.null(raw_tok)) {
    has_auth <- any(tolower(names(hdr_vec)) == "authorization")
    if (has_auth) {
      if (!isTRUE(.sda_state$warned_token_ignored)) {
        message("Both `soilDB.sda.headers` ('Authorization') and a bearer token were provided; using explicit 'Authorization' header and ignoring token.")
        .sda_state$warned_token_ignored <- TRUE
      }
    } else {
      hdr_vec["Authorization"] <- paste("Bearer", raw_tok)
    }
  }

  final_ua <- default_ua
  if (length(hdr_vec) > 0L) {
    is_official <- identical(parsed$host, .SDA_OFFICIAL_HOST)
    if (is_official) {
      if (!isTRUE(.sda_state$warned_official_host)) {
        warning(
          sprintf(
            "Custom SDA headers/tokens are ignored when using the official SDA host (%s). Skipped header(s): %s",
            .SDA_OFFICIAL_HOST, paste(names(hdr_vec), collapse = ", ")
          ),
          call. = FALSE
        )
        .sda_state$warned_official_host <- TRUE
      }
      hdr_vec <- character(0)
    } else {
      is_loopback <- parsed$host %in% c("localhost", "127.0.0.1", "::1")
      if (identical(parsed$scheme, "http") && !is_loopback) {
        stop(
          sprintf(
            "Refusing to send custom SDA headers/tokens over plain HTTP to non-loopback host '%s'. Use HTTPS or a loopback address (localhost, 127.0.0.1, ::1).",
            parsed$host
          ),
          call. = FALSE
        )
      }
    }
  }

  if (length(hdr_vec) > 0L) {
    ua_idx <- which(tolower(names(hdr_vec)) == "user-agent")
    if (length(ua_idx) > 0L) {
      custom_ua <- trimws(hdr_vec[[ua_idx[1]]])
      if (nzchar(custom_ua)) final_ua <- paste(default_ua, custom_ua)
      hdr_vec <- hdr_vec[-ua_idx]
    }
  }

  list(url = raw_url, useragent = final_ua, headers = hdr_vec)
}

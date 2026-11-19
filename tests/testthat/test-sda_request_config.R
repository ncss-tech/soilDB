with_sda_config_options <- function(options_list, code) {
  old_options <- options(names(options_list))
  on.exit(options(old_options), add = TRUE)
  do.call(options, options_list)
  force(code)
}

with_sda_test_token <- function(token, code) {
  old_token <- Sys.getenv("SOILDB_SDA_TOKEN", unset = NA_character_)
  on.exit(
    if (is.na(old_token)) Sys.unsetenv("SOILDB_SDA_TOKEN") else Sys.setenv(SOILDB_SDA_TOKEN = old_token),
    add = TRUE
  )
  if (is.null(token)) {
    Sys.unsetenv("SOILDB_SDA_TOKEN")
  } else {
    Sys.setenv(SOILDB_SDA_TOKEN = token)
  }
  force(code)
}

reset_sda_config_warnings <- function() {
  state <- get(".sda_state", envir = asNamespace("soilDB"))
  state$warned_token_ignored <- FALSE
  state$warned_official_host <- FALSE
}

test_that("SDA request configuration defaults to legacy URL and User-Agent", {
  with_sda_test_token(NULL, {
    with_sda_config_options(
      list(
        soilDB.sda.tabular.post.rest.url = NULL,
        soilDB.sda.headers = NULL,
        soilDB.sda.token = NULL
      ),
      {
        default_ua <- paste0(
          "libcurl/", curl::curl_version()$version,
          " r-curl/", as.character(utils::packageVersion("curl")),
          " soilDB/", as.character(utils::packageVersion("soilDB")), " (SDA_query)"
        )
        cfg <- .sda_request_config(default_ua)
        expect_identical(cfg$url, .SDA_DEFAULT_URL)
        expect_identical(cfg$useragent, default_ua)
        expect_length(cfg$headers, 0)
      }
    )
  })
})

test_that("SDA endpoint URLs must be single HTTP or HTTPS URLs", {
  expect_identical(
    .sda_parse_url("HTTPS://example.test:8443/path"),
    list(scheme = "https", host = "example.test")
  )
  expect_identical(
    .sda_parse_url("http://[::1]:8000/post"),
    list(scheme = "http", host = "::1")
  )
  for (url in list(NULL, "", NA_character_, c("https://a.test", "https://b.test"),
                   "ftp://example.test", "https://user@example.test")) {
    expect_error(.sda_parse_url(url))
  }
})

test_that("SDA headers accept named vectors, lists, and functions", {
  with_sda_test_token(NULL, {
    for (headers in list(
      c("X-Test" = "value"),
      list("X-Test" = "value"),
      function() c("X-Test" = "value")
    )) {
      cfg <- with_sda_config_options(
        list(
          soilDB.sda.tabular.post.rest.url = "https://stand-in.example/post",
          soilDB.sda.headers = headers,
          soilDB.sda.token = NULL
        ),
        .sda_request_config("legacy UA")
      )
      expect_identical(cfg$headers, c("X-Test" = "value"))
    }
  })
})

test_that("SDA headers reject invalid names, values, and line breaks", {
  with_sda_test_token(NULL, {
    invalid_headers <- list(
      "value",
      c("X-Test" = "one", "x-test" = "two"),
      c(" " = "value"),
      list("X-Test" = 1),
      c("X-Test" = "value\r\nInjected: yes")
    )
    for (headers in invalid_headers) {
      expect_error(with_sda_config_options(
        list(
          soilDB.sda.tabular.post.rest.url = "https://stand-in.example/post",
          soilDB.sda.headers = headers,
          soilDB.sda.token = NULL
        ),
        .sda_request_config("legacy UA")
      ))
    }
    expect_error(with_sda_config_options(
      list(soilDB.sda.token = "secret\r\nInjected: yes"),
      .sda_request_config("legacy UA")
    ), "line breaks")
  })
})

test_that("reserved SDA headers are dropped without exposing values", {
  with_sda_test_token(NULL, {
    warning_text <- NULL
    cfg <- withCallingHandlers(
      with_sda_config_options(
        list(
          soilDB.sda.tabular.post.rest.url = "https://stand-in.example/post",
          soilDB.sda.headers = c("Content-Type" = "secret-value", "X-Test" = "ok"),
          soilDB.sda.token = NULL
        ),
        .sda_request_config("legacy UA")
      ),
      warning = function(w) {
        warning_text <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    expect_match(warning_text, "Content-Type")
    expect_false(grepl("secret-value", warning_text, fixed = TRUE))
    expect_identical(cfg$headers, c("X-Test" = "ok"))
  })
})

test_that("SDA bearer tokens resolve from options, functions, and environment", {
  with_sda_test_token("environment-token", {
    cfg <- with_sda_config_options(
      list(
        soilDB.sda.tabular.post.rest.url = "https://stand-in.example/post",
        soilDB.sda.headers = NULL,
        soilDB.sda.token = NULL
      ),
      .sda_request_config("legacy UA")
    )
    expect_identical(unname(cfg$headers[["Authorization"]]), "Bearer environment-token")

    cfg <- with_sda_config_options(
      list(
        soilDB.sda.tabular.post.rest.url = "https://stand-in.example/post",
        soilDB.sda.headers = NULL,
        soilDB.sda.token = function() "option-token"
      ),
      .sda_request_config("legacy UA")
    )
    expect_identical(unname(cfg$headers[["Authorization"]]), "Bearer option-token")
  })
})

test_that("explicit Authorization headers take precedence over bearer tokens", {
  reset_sda_config_warnings()
  messages <- character()
  with_sda_test_token(NULL, {
    cfg <- withCallingHandlers(
      with_sda_config_options(
        list(
          soilDB.sda.tabular.post.rest.url = "https://stand-in.example/post",
          soilDB.sda.headers = c(Authorization = "Custom auth"),
          soilDB.sda.token = "sensitive-token"
        ),
        .sda_request_config("legacy UA")
      ),
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    expect_identical(unname(cfg$headers[["Authorization"]]), "Custom auth")
    expect_true(any(grepl("ignoring token", messages, fixed = TRUE)))
    expect_false(any(grepl("sensitive-token", messages, fixed = TRUE)))
  })
})

test_that("custom credentials are suppressed for the official SDA host", {
  reset_sda_config_warnings()
  with_sda_test_token(NULL, {
    warning_text <- NULL
    cfg <- withCallingHandlers(
      with_sda_config_options(
        list(
          soilDB.sda.tabular.post.rest.url = "https://SDMDATAACCESS.SC.EGOV.USDA.GOV/post.rest",
          soilDB.sda.headers = c("X-Secret" = "secret-value"),
          soilDB.sda.token = "bearer-secret"
        ),
        .sda_request_config("legacy UA")
      ),
      warning = function(w) {
        warning_text <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    expect_length(cfg$headers, 0)
    expect_identical(cfg$useragent, "legacy UA")
    expect_match(warning_text, "X-Secret")
    expect_false(grepl("secret-value|bearer-secret", warning_text))
  })
})

test_that("SDA credentials cannot be sent over non-loopback HTTP", {
  with_sda_test_token(NULL, {
    expect_error(with_sda_config_options(
      list(
        soilDB.sda.tabular.post.rest.url = "http://stand-in.example/post",
        soilDB.sda.headers = NULL,
        soilDB.sda.token = "do-not-send"
      ),
      .sda_request_config("legacy UA")
    ), "Refusing to send")

    for (url in c("http://localhost:8000/post", "http://127.0.0.1/post", "http://[::1]/post")) {
      cfg <- with_sda_config_options(
        list(
          soilDB.sda.tabular.post.rest.url = url,
          soilDB.sda.headers = NULL,
          soilDB.sda.token = "local-token"
        ),
        .sda_request_config("legacy UA")
      )
      expect_identical(unname(cfg$headers[["Authorization"]]), "Bearer local-token")
    }
  })
})

test_that("custom User-Agent is appended to the legacy User-Agent", {
  with_sda_test_token(NULL, {
    cfg <- with_sda_config_options(
      list(
        soilDB.sda.tabular.post.rest.url = "https://stand-in.example/post",
        soilDB.sda.headers = c("User-Agent" = "stand-in client", "X-Test" = "value"),
        soilDB.sda.token = NULL
      ),
      .sda_request_config("legacy UA")
    )
    expect_identical(cfg$useragent, "legacy UA stand-in client")
    expect_identical(cfg$headers, c("X-Test" = "value"))
  })
})

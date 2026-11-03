test_that(".write_table_with_log creates and appends correctly with chunking", {

  skip_on_cran()

  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  tmp <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
    try(unlink(tmp), silent = TRUE)
  })

  df_full <- data.frame(x = 1:50, y = letters[1:50], stringsAsFactors = FALSE)
  df_part1 <- df_full[1:10, , drop = FALSE]
  df_part2 <- df_full[11:50, , drop = FALSE]

  rec1 <- .write_table_with_log(conn = con, name = "t_test", value = df_part1, overwrite = TRUE, append = FALSE, file = NULL, quiet = TRUE)
  expect_true(is.list(rec1))
  expect_equal(rec1$table, "t_test")
  expect_equal(rec1$rows, nrow(df_part1))

  oldenv <- Sys.getenv("R_SOILDB_SSURGO_ROW_CHUNK_SIZE")
  Sys.setenv(R_SOILDB_SSURGO_ROW_CHUNK_SIZE = "15")
  on.exit(Sys.setenv(R_SOILDB_SSURGO_ROW_CHUNK_SIZE = oldenv))

  rec2 <- .write_table_with_log(conn = con, name = "t_test", value = df_part2, overwrite = FALSE, append = TRUE, file = NULL, quiet = TRUE)
  expect_true(is.list(rec2))
  expect_equal(rec2$table, "t_test")
  expect_equal(rec2$rows, nrow(df_part2))

  db_df <- DBI::dbReadTable(con, "t_test")

  expect_equal(db_df, df_full)
})

test_that("areasymbol filtering avoids oversized regular expressions", {
  files <- c(
    file.path("exports", "CA067", "tabular", "mapunit.txt"),
    file.path("exports", "spatial", "soilmu_a_ca077.shp"),
    file.path("exports", "spatial", "soilmu_a_ca0670.shp"),
    file.path("exports", "spatial", "soilmu_a_ca9990.txt")
  )
  requested <- c(
    sprintf("CA%03d", 1:999),
    "CA067",
    "CA077",
    "CA999"
  )

  matches <- .match_ssurgo_areasymbols(files, requested)

  expect_true(matches$match[1])
  expect_true(matches$match[2])
  expect_false(matches$match[3])
  expect_false(matches$match[4])
  expect_setequal(matches$found, c("CA067", "CA077"))
})

test_that("areasymbol matching discovers symbols in non-data paths", {
  files <- c(
    file.path("exports", "CA067", "tabular", "mapunit.txt"),
    file.path("exports", "spatial", "soilmu_a_ca077.shp"),
    file.path("exports", "CA067", "wss_SSA_CA067_[2026-01-01].zip"),
    file.path("exports", "README.txt.bak"),
    file.path("exports", "notes.csv")
  )

  result <- .match_ssurgo_areasymbols(files, c("CA067", "CA077"))
  expect_true(result$match[3])
  expect_setequal(result$found, c("CA067", "CA077"))
})

test_that("createSSURGO exdir inventory messages and errors", {
  exdir <- tempfile("empty-ssurgo-")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)

  expect_message(
    expect_error(
      createSSURGO(filename = tempfile(fileext = ".sqlite"),
                   exdir = exdir, quiet = FALSE),
      "appears to be empty"
    ),
    "Scanning SSURGO extraction directory"
  )
})

test_that("createSSURGO does not append after failed table initialization", {

  skip_on_cran()
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  exdir <- tempfile("ssurgo-init-fail-")
  dir.create(file.path(exdir, "tabular"), recursive = TRUE)
  on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)

  f1 <- file.path(exdir, "tabular", "mapunit_part1.txt")
  f2 <- file.path(exdir, "tabular", "mapunit_part2.txt")
  writeLines("1", f1)
  writeLines("2", f2)

  mstabcol_file <- file.path(exdir, "tabular", "mstabcol.txt")
  writeLines("mapunit|1|mukey|Map Unit Key||String||10", mstabcol_file)

  msidxdet_file <- file.path(exdir, "tabular", "msidxdet.txt")
  writeLines("mapunit|IX_dummy|junk|junk", msidxdet_file)

  out_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(out_db, force = TRUE), add = TRUE)

  append_calls <- logical(0)
  write_call_n <- 0L

  testthat::local_mocked_bindings(
    .inventory_ssurgo_files = function(...) {
      list(
        f.shp = character(0),
        shp.grp = NULL,
        f.shp.sc = character(0),
        include_spatial = FALSE,
        f.txt.grp = list(mapunit = c(f1, f2)),
        txt.first = character(0),
        include_tabular = TRUE,
        mstabn = character(0),
        mstabcn = mstabcol_file,
        msidxdn = msidxdet_file,
        mstab = NULL,
        mstab_lut = c(mapunit = "mapunit")
      )
    },
    .read_delim_tabular = function(...) {
      data.frame(mukey = "123", stringsAsFactors = FALSE)
    },
    .write_table_with_log = function(conn, name, value, overwrite = FALSE,
                                     append = FALSE, file = NULL, quiet = TRUE) {
      append_calls <<- c(append_calls, append)
      write_call_n <<- write_call_n + 1L

      if (write_call_n == 1L) {
        DBI::dbWriteTable(conn, name, value[0, , drop = FALSE], append = FALSE, overwrite = FALSE)
        stop("mock init failure", call. = FALSE)
      }

      list(table = name, ssa = NA_character_, rows = nrow(value), elapsed = 0)
    },
    .package = "soilDB"
  )

  expect_no_error(
    createSSURGO(
      filename = out_db,
      exdir = exdir,
      include_spatial = FALSE,
      include_tabular = TRUE,
      quiet = FALSE
    )
  )

  expect_identical(write_call_n, 2L)
  expect_identical(append_calls, c(FALSE, FALSE))
  expect_false(append_calls[2])
})

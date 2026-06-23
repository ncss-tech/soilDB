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

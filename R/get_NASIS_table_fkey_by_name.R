#' Get the foreign key by table name
#'
#' @param tables character vector of table names
#'
#' @return The foreign key column name for the specified table name
#'
#' @examples
#' \dontrun{
#' .get_NASIS_table_fkey_by_name(c("site","phorizon_View_1","not_a_table"))
#' }
.get_NASIS_table_fkey_by_name <- function(tables) {
  
  NASIS_table_column_keys <- NULL
  load(system.file("data/NASIS_table_column_keys.rda", package="soilDB"))
  
  # create fkey lookup table
  fkeys <- NASIS_table_column_keys$fkey
  names(fkeys) <- NASIS_table_column_keys$table
  
  # the same foreign keys are used in the selected set
  tables_search <- gsub("_View_1", "", tables)
  res <- fkeys[match(tables_search, names(fkeys))]
  res <- unlist(lapply(res, function(x) if(length(x) == 0) { return(NA) } else { return(x) }), recursive = FALSE)
  names(res) <- tables
  return(res)
}

#' Get the key by table name
#'
#' @param tables character vector of table names
#'
#' @return The foreign key column name for the specified table name
#'
#' @examples
#' \dontrun{
#' .get_NASIS_table_pkey_by_name(c("site","phorizon_View_1","not_a_table"))
#' }
.get_NASIS_table_pkey_by_name <- function(tables) {
  
  NASIS_table_column_keys <- NULL
  load(system.file("data/NASIS_table_column_keys.rda", package="soilDB"))
  
  # create fkey lookup table
  pkeys <- NASIS_table_column_keys$pkey
  names(pkeys) <- NASIS_table_column_keys$table
  
  # the same primary keys are used in the selected set
  tables_search <- gsub("_View_1", "", tables)
  res <- pkeys[match(tables_search, names(pkeys))]
  res <- unlist(lapply(res, function(x) if(length(x) == 0) { return(NA) } else { return(x) }), recursive = FALSE)
  names(res) <- tables
  return(res)
}

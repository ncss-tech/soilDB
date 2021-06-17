#' Get a NASIS table key by type and table name
#'
#' @param tables character vector of table names
#' @param keycol One of: "fkey" the foreign key; "pkeyref" the primary key referenced by the foreign key, or "pkey" the primary key.
#'
#' @return The key column name for the specified table name
#' @aliases get_NASIS_fkey_by_name get_NASIS_pkeyref_by_name get_NASIS_pkey_by_name
#' @examples
#' \dontrun{
#' get_NASIS_table_key_by_name(c("site","phorizon_View_1","not_a_table"))
#' }
get_NASIS_table_key_by_name <- function(tables, 
                                         keycol = c("all", "fkey", "pkeyref", "pkey")) {
  
  NASIS_table_column_keys <- NULL
  load(system.file("data/NASIS_table_column_keys.rda", package="soilDB"))
  
  tables_search <- gsub("_View_1", "", tables)
  keycol <- match.arg(keycol, c("all", "fkey", "pkeyref", "pkey"))
  if(keycol == "all") {
    idx.order <- order(match(tables_search, NASIS_table_column_keys$table))
    
    res <- merge(data.frame(table = tables_search),
                            NASIS_table_column_keys,
                            all.x = TRUE, sort = FALSE)[idx.order,]
    # map back to input labels
    res$table <- tables
    
  } else {
    # create fkey lookup table
    keys <- NASIS_table_column_keys[[keycol]]
    names(keys) <- NASIS_table_column_keys$table
    
    # the same primary keys are used in the selected set
    res <- keys[match(tables_search, names(keys))]
    res <- unlist(lapply(res, function(x) {
                                if(length(x) == 0) { 
                                  return(NA) 
                                } else { 
                                  return(x) 
                                }
                              }))
    # map back to input labels
    names(res) <- tables
  }
  return(res)
}

#' @export
get_NASIS_fkey_by_name <- function(tables, keycol = "fkey") {
  get_NASIS_table_key_by_name(tables, keycol = keycol)
}

#' @export
get_NASIS_pkeyref_by_name <- function(tables, keycol = "pkeyref") {
  get_NASIS_table_key_by_name(tables, keycol = keycol)
}

#' @export
get_NASIS_pkey_by_name <- function(tables, keycol = "pkey") {
  get_NASIS_table_key_by_name(tables, keycol = keycol)
}

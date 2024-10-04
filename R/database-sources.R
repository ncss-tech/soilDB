#' NASIS Local Database
#'
#' This is a guide on using databases that follow the NASIS schema. Most of the time users are querying an instance of the Microsoft SQL Server NASIS local transactional database running on their computer. It is possible to create file-based "snapshots" of a local instance of the NASIS database using SQLite. See `[createStaticNASIS()]` for details. These file-based snapshots, or other custom connections, can generally be specified to NASIS-related functions via the `dsn` argument.
#' 
#' # Working With Coded Values and Decoding
#' 
#' Some values (choice lists) in NASIS are conventionally stored using numeric codes. The codes are defined by "domain" and allow for both "names" and "labels" as well as other descriptive information to be provided for each choice list element. See `get_NASIS_column_metadata()` for details.
#' 
#' Many soilDB functions call the function `uncode()` internally to handle conversion to human-readable values using official NASIS domains. If writing queries directly against the database source, such as a connection created with `NASIS()` or query run with `dbQueryNASIS()`, you call `uncode()` on the _data.frame_ result of your query. Conversion of internal values to choice list names is based on domains associated with result column names.
#' 
#' When using a custom SQLite database, sometimes values in the database are delivered pre-decoded to make the database more directly usable. An example of this would be the Kellogg Soil Survey Laboratory morphologic database, the NASIS data corresponding to the laboratory analyses available through the \link[=fetchLDM]{Lab Data Mart (LDM)}. 
#' 
#' To avoid issues with offsets between internal storage value and external readable value (for data such as farmland classification or Munsell color value and chroma), you should not call `uncode()` multiple times. Also, you can disable the "decoding" behavior made internally in soilDB functions by setting `options(soilDB.NASIS.skip_uncode = TRUE)`.
#' 
#' 
#' @name NASISLocalDatabase
NULL

# new tools for querying lab data from SDA Lab Data Mart
#

#' Query data from Kellogg Soil Survey Laboratory Data Mart via Soil Data Access
#'
#' LDM model diagram: \url{https://jneme910.github.io/Lab_Data_Mart_Documentation/Documents/SDA_KSSL_Data_model.html}
#'
#' @param x a vector of values to find in column specified by `what`
#' @param dsn data source name; either a path to a SQLite database, an open DBIConnection or (default) `NULL` (to use `soilDB::SDA_query`)
#' @param what a single column name from tables: `lab_combine_nasis_ncss`, `lab_webmap`, `lab_site`, `lab_pedon` or `lab_area`
#' @param bycol a single column name from `lab_layer` used for processing chunks; default: `"pedon_key"`
#' @param tables a vector of table names; Default is `"lab_physical_properties"`, `"lab_chemical_properties"`, `"lab_calculations_including_estimates_and_default_values"`, and `"lab_rosetta_Key"`. May also include one or more of:  `"lab_mineralogy_glass_count"`, `"lab_major_and_trace_elements_and_oxides"`, `"lab_xray_and_thermal"` but it will be necessary to select appropriate `prep_code` and `analyzed_size_frac` for your analysis (see _Details_).
#' @param chunk.size number of pedons per chunk (for queries that may exceed `maxJsonLength`)
#' @param ntries number of tries (times to halve `chunk.size`) before returning `NULL`; default `3`
#' @param layer_type Default: `"horizon"`, `"layer"`, and `"reporting layer"`
#' @param prep_code Default: `"S"` and `""`. May also include one or more of: `"F"`, `"HM"`, `"HM_SK"` `"GP"`, `"M"`, `"N"`, or `"S"`
#' @param analyzed_size_frac Default: `"<2 mm"` and `""`. May also include one or more of: `"<2 mm"`, `"0.02-0.05 mm"`, `"0.05-0.1 mm"`, `"1-2 mm"`, `"0.5-1 mm"`, `"0.25-0.5 mm"`, `"0.05-2 mm"`, `"0.02-2 mm"`, `"0.1-0.25 mm"`, `"<0.002 mm"`
#'
#' @details If the `chunk.size` parameter is set too large and the Soil Data Access request fails, the algorithm will re-try the query with a smaller (halved) `chunk.size` argument. This will be attempted up to 3 times before returning `NULL`
#'
#' Currently the `lab_area` tables are joined only for the "Soil Survey Area" related records.
#'
#' When requesting data from `"lab_major_and_trace_elements_and_oxides"`, `"lab_mineralogy_glass_count"`, or `"lab_xray_and_thermal"` multiple preparation codes (`prep_code`) or size fractions (`analyzed_size_frac`) are possible. The default behavior of `fetchLDM()` is to attempt to return a topologically valid (minimal overlaps) _SoilProfileCollection_. This is achieved by setting `prep_code="S"` ("sieved") and `analyzed_size_frac="<2 mm"`. You may specify alternate or additional preparation codes or fractions as needed, but note that this may cause "duplication" of some layers where measurements were made with different preparation or on fractionated samples
#'
#' @return a `SoilProfileCollection` for a successful query, a `try-error` if no site/pedon locations can be found or `NULL` for an empty `lab_layer` (within sites/pedons) result
#' @export
#'
#' @examples
#' \donttest{
#'
#' if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'
#'   # fetch by ssa_key
#'   fetchLDM(8297, what = "ssa_key")
#'
#'   # get physical properties for points correlated as taxonomic subgroup "Typic Argialbolls"
#'   fetchLDM(x= "Typic Argialbolls", what = "corr_taxsubgrp", tables = "lab_physical_properties")
#'
#'   # fetch by area_code (must be a soil survey area code for now; and be querying against SDA (dsn=NULL)
#'   #                     SSA is the lowest-level area that is always populated)
#'   fetchLDM("CA630", what = "area_code")
#' }
#'
#' }
#' @importFrom aqp `depths<-` `site<-`
#' @importFrom data.table rbindlist
fetchLDM <- function(x,
           dsn = NULL,
           what = "lab_combine_nasis_ncss.pedlabsampnum",
           bycol = "pedon_key",
           tables = c(
             "lab_physical_properties",
             "lab_chemical_properties",
             "lab_calculations_including_estimates_and_default_values",
             # "lab_major_and_trace_elements_and_oxides", # remove alternate prep/fractionated data from default set of tables
             # "lab_mineralogy_glass_count",
             # "lab_xray_and_thermal",
             "lab_rosetta_Key"),
           chunk.size = 1000,
           ntries = 3,
           layer_type = c("horizon","layer","reporting layer"),
           prep_code = c("S", ""), # , `"F"`, `"HM"`, `"HM_SK"` `"GP"`, `"M"`, `"N"`, or `"S"`
           analyzed_size_frac = c("<2 mm", "")#  optional: "0.02-0.05 mm", "0.05-0.1 mm", "1-2 mm", "0.5-1 mm", "0.25-0.5 mm", "0.05-2 mm", "0.02-2 mm", "0.1-0.25 mm", "<0.002 mm"
           ) {

  # set up data source connection if needed

  if (inherits(dsn, 'DBIConnection')) {
    # allow any existing DBI connection to be passed via dsn argument
    con <- dsn
  } else if(!is.null(dsn) && is.character(dsn)) {
    # if it is a path to data source, try to connect with RSQLite
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dsn)
  } else {
    # otherwise we are using SDA_query
    con <- NULL
  }

  what <- match.arg(what, choices = c("pedon_key", "site_key", "pedlabsampnum", "pedoniid", "upedonid",
                                      "labdatadescflag", "priority", "priority2", "samp_name", "samp_class_type",
                                      "samp_classdate", "samp_classification_name", "samp_taxorder",
                                      "samp_taxsuborder", "samp_taxgrtgroup", "samp_taxsubgrp", "samp_taxpartsize",
                                      "samp_taxpartsizemod", "samp_taxceactcl", "samp_taxreaction",
                                      "samp_taxtempcl", "samp_taxmoistscl", "samp_taxtempregime", "samp_taxminalogy",
                                      "samp_taxother", "samp_osdtypelocflag", "corr_name", "corr_class_type",
                                      "corr_classdate", "corr_classification_name", "corr_taxorder",
                                      "corr_taxsuborder", "corr_taxgrtgroup", "corr_taxsubgrp", "corr_taxpartsize",
                                      "corr_taxpartsizemod", "corr_taxceactcl", "corr_taxreaction",
                                      "corr_taxtempcl", "corr_taxmoistscl", "corr_taxtempregime", "corr_taxminalogy",
                                      "corr_taxother", "corr_osdtypelocflag", "SSL_name", "SSL_class_type",
                                      "SSL_classdate", "SSL_classification_name", "SSL_taxorder", "SSL_taxsuborder",
                                      "SSL_taxgrtgroup", "SSL_taxsubgrp", "SSL_taxpartsize", "SSL_taxpartsizemod",
                                      "SSL_taxceactcl", "SSL_taxreaction", "SSL_taxtempcl", "SSL_taxmoistscl",
                                      "SSL_taxtempregime", "SSL_taxminalogy", "SSL_taxother", "SSL_osdtypelocflag",
                                      "siteiid", "usiteid", "site_obsdate", "latitude_decimal_degrees",
                                      "longitude_decimal_degrees", "country_key", "state_key", "county_key",
                                      "mlra_key", "ssa_key", "npark_key", "nforest_key", "note", "samp_taxfamhahatmatcl",
                                      "corr_taxfamhahatmatcl", "SSL_taxfamhahatmatcl", "pedobjupdate",
                                      "siteobjupdate", "area_key", "area_type", "area_sub_type", "parent_area_key",
                                      "parent_org_key", "area_code", "area_name", "area_abbrev", "area_desc"))
  
  # TODO: set up arbitrary area queries by putting area table into groups:
  #       country, state, county, mlra, ssa, npark, nforest

  # get site/pedon/area information
  site_query <- sprintf(
    "SELECT * FROM lab_combine_nasis_ncss
              LEFT JOIN lab_webmap ON
                  lab_combine_nasis_ncss.pedon_key = lab_webmap.pedon_key
              LEFT JOIN lab_site ON
                  lab_combine_nasis_ncss.site_key = lab_site.site_key
              LEFT JOIN lab_pedon ON
                  lab_combine_nasis_ncss.site_key = lab_pedon.site_key
            WHERE LOWER(%s) IN %s",
    what, format_SQL_in_statement(tolower(x)))

  if(inherits(con, 'DBIConnection')) {
    # query con using (modified) site_query
    sites <- try(DBI::dbGetQuery(con,
                                 gsub("\\blab_|\\blab_combine_", "",
                                      gsub("lab_(site|pedon)", "nasis_\\1", site_query))))
  } else {
    # the lab_area table allows for overlap with many different area types
    # for now we only offer the "ssa" (soil survey area) area_type 
    site_query_ssaarea <- gsub("WHERE LOWER", 
                       "LEFT JOIN lab_area ON
                       lab_combine_nasis_ncss.ssa_key = lab_area.area_key
                       WHERE LOWER", site_query)
    sites <- SDA_query(site_query_ssaarea)
  }

  if (!inherits(sites, 'try-error')) {

    # TODO: this shouldn't be needed
    sites <- sites[,unique(colnames(sites))]

    # get data for lab layers within pedon_key returned
    hz <- .get_lab_layer_by_pedon_key(x = sites[[bycol]],
                                      con = con,
                                      bycol = bycol,
                                      tables = tables,
                                      layer_type = layer_type,
                                      prep_code = prep_code,
                                      analyzed_size_frac = analyzed_size_frac)

    .do_chunk <- function(con, size) {
      chunk.idx <- makeChunks(sites[[bycol]], size)
      as.data.frame(data.table::rbindlist(lapply(unique(chunk.idx),
                                                 function(i) {
                                                   keys <- sites[[bycol]][chunk.idx == i]
                                                   res <- .get_lab_layer_by_pedon_key(x = keys,
                                                                                      con = con,
                                                                                      bycol = bycol,
                                                                                      tables = tables,
                                                                                      prep_code = prep_code,
                                                                                      analyzed_size_frac = analyzed_size_frac)
                                                   if (inherits(res, 'try-error'))
                                                     return(NULL)
                                                   res
                                                 })))
    }

    ntry <- 0
    while ((inherits(hz, 'try-error') || is.null(hz)) && ntry < ntries) {
      hz <- .do_chunk(con, size = chunk.size)
      # repeat as long as there is a try error/NULL, halving chunk.size with each iteration
      chunk.size <- pmax(floor(chunk.size / 2), 1)
      ntry <- ntry + 1
    }

    # close connection (if we opened it)
    if (is.character(dsn) && inherits(con, "DBIConnection")) {
      DBI::dbDisconnect(con)
    }

    if (!is.null(hz) && nrow(hz) > 0) {

      # local SQLite: sometimes prep_code differs for bulk density with no difference in value
      #               e.g. pedon_key 10010
      # prep_code is also repeated across multiple tables
      # hz$prep_code <- NULL

      # site_key used in multiple tables
      hz$site_key <- NULL

      # build SoilProfileCollection
      depths(hz) <- pedon_key ~ hzn_top + hzn_bot
      site(hz) <- sites

      return(hz)
    } else {
      message("no horizon data in result!")
      return(NULL)
    }

  } else {
    # return try-error
    return(sites)
  }
}

.get_lab_layer_by_pedon_key <- function(x,
                                        con = NULL,
                                        bycol = "pedon_key",
                                        tables = c(
                                          "lab_physical_properties",
                                          "lab_mineralogy_glass_count",
                                          "lab_chemical_properties",
                                          "lab_major_and_trace_elements_and_oxides",
                                          "lab_xray_and_thermal",
                                          "lab_calculations_including_estimates_and_default_values",
                                          "lab_rosetta_Key"
                                        ),
                                        layer_type = c("horizon", "layer", "reporting layer"),
                                        prep_code = c("GP", "M", "N", "S", "HM"),
                                        analyzed_size_frac = c("0.02-0.05 mm", "0.05-0.1 mm", "1-2 mm", 
                                                               "0.5-1 mm", "0.25-0.5 mm", "0.05-2 mm", 
                                                               "<2 mm", "0.02-2 mm", "0.1-0.25 mm", 
                                                               "<0.002 mm")) {

  
  flattables <- c(
    "lab_physical_properties",
    "lab_chemical_properties",
    "lab_calculations_including_estimates_and_default_values",
    "lab_major_and_trace_elements_and_oxides",
    "lab_rosetta_Key"
  )

  fractables <- c("lab_mineralogy_glass_count",
                  "lab_xray_and_thermal")

  tables <- match.arg(tables, c(flattables, fractables), several.ok = TRUE)

  fractablejoincriteria <- list(
      "lab_mineralogy_glass_count" = "LEFT JOIN lab_mineralogy_glass_count ON
                                      lab_layer.labsampnum = lab_mineralogy_glass_count.labsampnum",

      "lab_xray_and_thermal" = "LEFT JOIN lab_xray_and_thermal ON
                                lab_layer.labsampnum = lab_xray_and_thermal.labsampnum"
    )

  tablejoincriteria <- list(
      "lab_physical_properties" = "LEFT JOIN lab_physical_properties ON
                                   lab_layer.labsampnum = lab_physical_properties.labsampnum",

      "lab_chemical_properties" = "LEFT JOIN lab_chemical_properties ON
                                   lab_layer.labsampnum = lab_chemical_properties.labsampnum",
      
      "lab_major_and_trace_elements_and_oxides" = "LEFT JOIN lab_major_and_trace_elements_and_oxides ON
                                                   lab_layer.labsampnum = lab_major_and_trace_elements_and_oxides.labsampnum",
      
      "lab_calculations_including_estimates_and_default_values" = "LEFT JOIN lab_calculations_including_estimates_and_default_values ON
       lab_layer.labsampnum = lab_calculations_including_estimates_and_default_values.labsampnum",

      "lab_rosetta_Key" = "LEFT JOIN lab_rosetta_Key ON
                           lab_layer.layer_key = lab_rosetta_Key.layer_key"
    )
  
  layer_type <- match.arg(layer_type, c("horizon", "layer", "reporting layer"), several.ok = TRUE)
  
  layer_query <-  sprintf(
    "SELECT * FROM lab_layer %s WHERE lab_layer.layer_type IN %s AND %s IN %s AND %s",
    paste0(sapply(flattables[flattables %in% tables], function(a) tablejoincriteria[[a]]), collapse = "\n"),
    format_SQL_in_statement(layer_type),
    bycol,
    format_SQL_in_statement(x),
    paste0(paste0(sapply(flattables[flattables %in% tables[tables != "lab_rosetta_Key"]], 
                         function(b) paste0("IsNull(",b,".prep_code, '')")), 
                  " IN ", format_SQL_in_statement(prep_code)), collapse= " AND "))

  if (any(tables %in% fractables)) {
    layer_fraction_query <-  sprintf(
      "SELECT * FROM lab_layer %s WHERE lab_layer.layer_type IN %s AND %s IN %s AND %s AND %s",
      paste0(sapply(fractables[fractables %in% tables], function(a) fractablejoincriteria[[a]]), collapse = "\n"),
      format_SQL_in_statement(layer_type),
      bycol,
      format_SQL_in_statement(x),
      paste0(paste0(sapply(fractables[fractables %in% tables], 
                           function(b) paste0("IsNull(",b,".prep_code, '')")), 
                    " IN ", format_SQL_in_statement(prep_code)), collapse= " AND "),
      paste0(paste0(sapply(fractables[fractables %in% tables], 
                         function(b) paste0("IsNull(",b,".analyzed_size_frac, '')")), 
                  " IN ", format_SQL_in_statement(analyzed_size_frac)), collapse= " AND "))
  } else {
    layer_fraction_query <- NULL
  }

  if(inherits(con, 'DBIConnection')) {
    # query con using (modified) layer_query
    return(try(DBI::dbGetQuery(con, gsub("\\blab_|\\blab_combine_|_properties|_Key|_including_estimates_and_default_values|_and|mineralogy_|_count", "", gsub("major_and_trace_elements_and_oxides","geochemical",layer_query)))))
  }

  layerdata <- suppressWarnings(SDA_query(layer_query))
  
  if (inherits(layerdata, 'try-error'))
    return(layerdata)
  
  # TODO: this shouldn't be needed
  layerdata <- layerdata[,unique(colnames(layerdata))]

  if(!is.null(layer_fraction_query)) {
    layerfracdata <- suppressWarnings(SDA_query(layer_fraction_query))
    layerfracdata <- layerfracdata[,unique(colnames(layerfracdata))]
    if (!inherits(layerdata, 'try-error') && !inherits(layerfracdata, 'try-error')) {

      if (nrow(layerfracdata) == 0)
        message(sprintf('no fractionated samples found for selected prep_code (%s) and analyzed_size_frac (%s)',
                paste0(prep_code, collapse = ", "), paste0(analyzed_size_frac, collapse=", ")))

      layerdata <- merge(layerdata, layerfracdata[,c("labsampnum",
                                                     colnames(layerfracdata)[!colnames(layerfracdata) %in% colnames(layerdata)])],
                         by = "labsampnum", all.x = TRUE, incomparables = NA)
    }
  }
  layerdata$prep_code[is.na(layerdata$prep_code)] <- ""
  layerdata
}

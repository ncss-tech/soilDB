# new tools for querying lab data from SDA Lab Data Mart
#

#' Query data from Kellogg Soil Survey Laboratory Data Mart via Soil Data Access or local SQLite snapshot
#'
#' This function provides access to the Kellogg Soil Survey Laboratory Data Mart via Soil Data Access or a local SQLite snapshot. See details and examples for additional usage instructions.
#' 
#'
#' @param x A vector of values to find in column specified by `what`, default `NULL` uses no constraints on `what`
#' @param what A single column name from tables: `lab_combine_nasis_ncss`, `lab_webmap`, `lab_site`, `lab_pedon` or `lab_area`. Common choices include `pedlabsampnum` (Laboratory Pedon ID), `upedonid` (User Pedon ID), `corr_name` ('Correlated' Taxon Name), `samp_name` ('Sampled As' Taxon Name), or `area_code` (area symbol for specified `lab_area` records, see `area_type`).
#' @param bycol A single column name from `lab_layer` used for processing chunks; default: `"pedon_key"`
#' @param tables A vector of table names; Default is `"lab_physical_properties"`, `"lab_chemical_properties"`, `"lab_calculations_including_estimates_and_default_values"`, and `"lab_rosetta_Key"`. May also include one or more of: `"lab_mir"`, `"lab_mineralogy_glass_count"`, `"lab_major_and_trace_elements_and_oxides"`, `"lab_xray_and_thermal"` but it will be necessary to select appropriate `prep_code` and `analyzed_size_frac` for your analysis (see _Details_).
#' @param WHERE character. A custom SQL WHERE clause, which overrides `x`, `what`, and `bycol`, such as `CASE WHEN corr_name IS NOT NULL THEN LOWER(corr_name) ELSE LOWER(samp_name) END = 'musick'`
#' @param chunk.size Number of pedons per chunk (for queries that may exceed `maxJsonLength`)
#' @param ntries Number of tries (times to halve `chunk.size`) before returning `NULL`; default `3`
#' @param layer_type Default: `"horizon"`, `"layer"`, and `"reporting layer"`
#' @param area_type Default: `"ssa"` (Soil Survey Area). Other options include (choose one): `"country"`, `"state"`, `"county"`, `"mlra"` (Major Land Resource Area), `"nforest"` (National Forest), `"npark"` (National Park)
#' @param prep_code Default: `"S"` and `""`. May also include one or more of: `"F"`, `"HM"`, `"HM_SK"` `"GP"`, `"M"`, `"N"`, or `"S"`
#' @param analyzed_size_frac Default: `"<2 mm"` and `""`. May also include one or more of: `"<0.002 mm"`, `"0.02-0.05 mm"`, `"0.05-0.1 mm"`, `"0.1-0.25 mm"`, `"0.25-0.5 mm"`, `"0.5-1 mm"`, `"1-2 mm"`, `"0.02-2 mm"`, `"0.05-2 mm"`
#' @param dsn Data source name; either a path to a SQLite database, an open DBIConnection or (default) `NULL` (to use `soilDB::SDA_query`)
#'
#' @details 
#' You can download SQLite or GeoPackage snapshots here: \url{https://ncsslabdatamart.sc.egov.usda.gov/database_download.aspx}. Specify the `dsn` argument to use a local copy of the lab data rather than Soil Data Access web service.
#'
#' Lab Data Mart model diagram: \url{https://jneme910.github.io/Lab_Data_Mart_Documentation/Documents/SDA_KSSL_Data_model.html}
#' If the `chunk.size` parameter is set too large and the Soil Data Access request fails, the algorithm will re-try the query with a smaller (halved) `chunk.size` argument. This will be attempted up to 3 times before returning `NULL`
#'
#' The default behavior joins the `lab_area` tables only for the "Soil Survey Area" related records. You can specify alternative area records for use in `x`, `what` or `WHERE` arguments by setting `area_type` to a different value.
#'
#' When requesting data from `"lab_major_and_trace_elements_and_oxides"`, `"lab_mineralogy_glass_count"`, or `"lab_xray_and_thermal"` multiple preparation codes (`prep_code`) or size fractions (`analyzed_size_frac`) are possible. The default behavior of `fetchLDM()` is to attempt to return a topologically valid (minimal overlaps) _SoilProfileCollection_. This is achieved by setting `prep_code="S"` ("sieved") and `analyzed_size_frac="<2 mm"`. You may specify alternate or additional preparation codes or fractions as needed, but note that this may cause "duplication" of some layers where measurements were made with different preparation or on fractionated samples
#'
#' @return a `SoilProfileCollection` for a successful query, a `try-error` if no site/pedon locations can be found or `NULL` for an empty `lab_layer` (within sites/pedons) result
#' @export
#' @examplesIf curl::has_internet() && requireNamespace("httr", quietly = TRUE)
#' \dontrun{
#'   
#'   # fetch by Soil Survey Area area symbol (area_code using default "ssa" area_type)
#'   res <- fetchLDM("CA630", what = "area_code")
#'   
#'   # fetch by Major Land Resource area symbol (area_code using "mlra" area_type)
#'   res <- fetchLDM("22A", what = "area_code", area_type = "mlra")
#'   
#'   # fetch by multiple case-insensitive taxon name
#'   # (correlated or sampled as Musick or Holland series)
#'   res <- fetchLDM(WHERE = "(CASE WHEN corr_name IS NOT NULL 
#'                                 THEN LOWER(corr_name) 
#'                                 ELSE LOWER(samp_name) 
#'                             END) IN ('musick', 'holland')")
#'
#'   # physical properties of soils correlated as taxonomic subgroup "Typic Argialbolls"
#'   res <- fetchLDM(x = "Typic Argialbolls", 
#'                   what = "corr_taxsubgrp", 
#'                   tables = "lab_physical_properties")
#'
#' }
fetchLDM <- function(x = NULL,
           what = "pedlabsampnum",
           bycol = "pedon_key",
           tables = c(
             "lab_physical_properties",
             "lab_chemical_properties",
             "lab_calculations_including_estimates_and_default_values",
             # # remove alternate prep/fractionated data from default set of tables
             # "lab_major_and_trace_elements_and_oxides", 
             # "lab_mineralogy_glass_count",
             # "lab_xray_and_thermal",
             "lab_rosetta_Key"
             # "lab_mir"
             ),
           WHERE = NULL,
           chunk.size = 1000,
           ntries = 3,
           layer_type = c("horizon", "layer", "reporting layer"), 
           area_type = c("ssa", "country", "state", "county", "mlra", "nforest", "npark"),
           prep_code = c("S", ""), # , `"F"`, `"HM"`, `"HM_SK"` `"GP"`, `"M"`, `"N"`, or `"S"`
           analyzed_size_frac = c("<2 mm", ""),#  optional: "<0.002 mm", "0.02-0.05 mm", "0.05-0.1 mm", "0.1-0.25 mm", "0.25-0.5 mm", "0.5-1 mm", "1-2 mm", "0.02-2 mm", "0.05-2 mm"
           dsn = NULL) {

  # set up data source connection if needed

  if (inherits(dsn, 'DBIConnection')) {
    # allow any existing DBI connection to be passed via dsn argument
    con <- dsn
  } else if (!is.null(dsn) && is.character(dsn)) {
    # if it is a path to data source, try to connect with RSQLite
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dsn)
  } else {
    # otherwise we are using SDA_query
    con <- NULL
  }
  
  area_type <- match.arg(tolower(area_type[1]),  c("ssa", "country", "state", "county", "mlra", "nforest", "npark"))
  
  lab_combine_nasis_ncss <- c("pedon_key", "site_key", "pedlabsampnum", "pedoniid", "upedonid", 
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
                              "siteobjupdate")
  lab_area <- c("area_key", "area_type", "area_sub_type", "parent_area_key", 
                "parent_org_key", "area_code", "area_name", "area_abbrev", "area_desc")
  lab_webmap <- c("wmiid", "Series", "User_pedon_ID", "pedon_Key", "peiid", "Soil_Classification")
  lab_site <- c("site_key", "user_site_id")
  lab_pedon <- c("pedon_key", "pedlabsampnum", "observation_date")
  
  if (!is.null(x)) {
    if (what %in% lab_combine_nasis_ncss)
      what <- paste0("lab_combine_nasis_ncss.", what)
    if (what %in% lab_area)
      what <- paste0("lab_area.", what)
    if (what %in% lab_webmap)
      what <- paste0("lab_webmap.", what)
    if (what %in% lab_site)
      what <- paste0("lab_site.", what)
    if (what %in% lab_pedon)
      what <- paste0("lab_pedon.", what)
    what <- match.arg(what, choices = c(paste0("lab_combine_nasis_ncss.", lab_combine_nasis_ncss), 
                                        paste0("lab_area.", lab_area),
                                        paste0("lab_webmap.", lab_webmap),
                                        paste0("lab_site.", lab_site),
                                        paste0("lab_pedon.", lab_pedon)))
  }

  if (!is.null(x) && (missing(WHERE) || is.null(WHERE))) {
    WHERE <- sprintf("LOWER(%s) IN %s", what, format_SQL_in_statement(tolower(x)))
  } 
  
  # get site/pedon/area information
  site_query <- paste0(
    "SELECT * FROM lab_combine_nasis_ncss
              LEFT JOIN lab_webmap ON
                  lab_combine_nasis_ncss.pedon_key = lab_webmap.pedon_key
              LEFT JOIN lab_site ON
                  lab_combine_nasis_ncss.site_key = lab_site.site_key
              LEFT JOIN lab_pedon ON
                  lab_combine_nasis_ncss.site_key = lab_pedon.site_key ", 
            ifelse(is.null(x) && is.null(WHERE), "", paste("WHERE", WHERE)))
  
  
  # the lab_area table allows for overlap with many different area types, only join for specified area_type
  site_query_byarea <- gsub(
    "WHERE",
    sprintf(
      "LEFT JOIN lab_area ON
      lab_combine_nasis_ncss.%s_key = lab_area.area_key
      WHERE", area_type
    ), site_query)
  
  if (inherits(con, 'DBIConnection')) {
    sites <- try(DBI::dbGetQuery(con, site_query_byarea))
  } else {
    sites <- suppressMessages(SDA_query(site_query_byarea))
  }

  if (!inherits(sites, 'try-error') && !is.null(sites)) {

    # TODO: this shouldn't be needed
    sites <- sites[, unique(colnames(sites))]

    if (is.null(chunk.size) || nrow(sites) < chunk.size) {
      # get data for lab layers within pedon_key returned
      hz <- .get_lab_layer_by_pedon_key(x = sites[[bycol]],
                                        con = con,
                                        bycol = bycol,
                                        tables = tables,
                                        layer_type = layer_type,
                                        prep_code = prep_code,
                                        analyzed_size_frac = analyzed_size_frac)
      if (inherits(hz, 'try-error') && grepl("Layer data query failed", hz[1])) {
        return(hz)
      }
    } else {
      hz <- try(stop("Failed to access Web Service or query returned no results", call. = FALSE), silent = TRUE)
    }
    
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
      if (is.null(chunk.size)) {
        stop("query failed and chunk.size argument is NULL", call. = FALSE)
      }
      hz <- .do_chunk(con, chunk.size)
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
      
      # hacks to deal with problems in the various databases
      # hz <- unique(hz[,unique(colnames(hz))]) 
      
      # build SoilProfileCollection
      depths(hz) <- pedon_key ~ hzn_top + hzn_bot
      hzn <- aqp::horizonNames(hz)
      hzn <- hzn[hzn != idname(hz)]
      site(hz) <- sites[, !colnames(sites) %in% hzn]
      hzdesgnname(hz) <- 'hzn_desgn'

      return(hz)
    } else {
      message("no horizon data in result!")
      return(NULL)
    }

  } else {
    # return try-error
    return(sites)
  }
  NULL
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
                                          "lab_rosetta_Key",
                                          "lab_mir"
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
    "lab_rosetta_Key",
    "lab_mir"
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
      "lab_xray_and_thermal" = "LEFT JOIN lab_xray_and_thermal ON
                                lab_layer.labsampnum = lab_xray_and_thermal.labsampnum",
      "lab_calculations_including_estimates_and_default_values" = "LEFT JOIN lab_calculations_including_estimates_and_default_values ON
       lab_layer.labsampnum = lab_calculations_including_estimates_and_default_values.labsampnum",

      "lab_rosetta_Key" = "LEFT JOIN lab_rosetta_Key ON
                           lab_layer.layer_key = lab_rosetta_Key.layer_key",
      "lab_mir" = "LEFT JOIN lab_mir ON lab_layer.layer_key = lab_mir.layer_key 
                   LEFT JOIN lab_mir_wavelength ON lab_mir.d_wavelength_array_id = lab_mir_wavelength.d_wavelength_array_id"
    )
  
  layer_type <- match.arg(layer_type, c("horizon", "layer", "reporting layer"), several.ok = TRUE)
  
  if (any(tables %in% flattables)) {
    nt <- flattables[flattables %in% tables[!tables %in% c("lab_rosetta_Key", "lab_mir")]]
    layer_query <-  sprintf(
      "SELECT * FROM lab_layer %s WHERE lab_layer.layer_type IN %s %s %s",
      paste0(sapply(flattables[flattables %in% tables], function(a) tablejoincriteria[[a]]), collapse = "\n"),
      format_SQL_in_statement(layer_type),
      ifelse(is.null(x), "", paste0(" AND ", bycol, " IN ", format_SQL_in_statement(x))),
      ifelse(length(nt) == 0, "", paste0(" AND ", paste0(sapply(nt, function(b) paste0("IsNull(",b,".prep_code, '')")), 
                    " IN ", format_SQL_in_statement(prep_code), collapse = " AND "))))
  } else {
    layer_query <- sprintf(
      "SELECT * FROM lab_layer WHERE lab_layer.layer_type IN %s %s",
      format_SQL_in_statement(layer_type),
      ifelse(is.null(x), "", paste0(" AND ", bycol, " IN ", format_SQL_in_statement(x))))
  } 
  
  if (any(tables %in% fractables)) {
    layer_fraction_query <-  sprintf(
      "SELECT * FROM lab_layer %s WHERE lab_layer.layer_type IN %s AND %s IN %s AND %s AND %s",
      paste0(sapply(fractables[fractables %in% tables], function(a) fractablejoincriteria[[a]]), collapse = "\n"),
      format_SQL_in_statement(layer_type),
      bycol,
      format_SQL_in_statement(x),
      paste0(paste0(sapply(fractables[fractables %in% tables], 
                           function(b) paste0("IsNull(",b,".prep_code, '')")), 
                    " IN ", format_SQL_in_statement(prep_code)), collapse = " AND "),
      paste0(paste0(sapply(fractables[fractables %in% tables], 
                         function(b) paste0("IsNull(",b,".analyzed_size_frac, '')")), 
                  " IN ", format_SQL_in_statement(analyzed_size_frac)), collapse = " AND "))
  } else {
    layer_fraction_query <- NULL
  }

  if (inherits(con, 'DBIConnection')) {
    layerdata <- try(DBI::dbGetQuery(con, gsub("IsNull", "IFNULL", layer_query)))
  } else {
    layerdata <- suppressWarnings(SDA_query(layer_query))
  }

  
  if (inherits(layerdata, 'try-error')) {
    return(try(stop("Layer data query failed", call. = FALSE)))
  }
  
  # TODO: this shouldn't be needed
  layerdata <- layerdata[,unique(colnames(layerdata))]

  if (!is.null(layer_fraction_query)) {
    layerfracdata <- suppressWarnings(SDA_query(layer_fraction_query))
    layerfracdata <- layerfracdata[,unique(colnames(layerfracdata))]
    if (!inherits(layerdata, 'try-error') && !inherits(layerfracdata, 'try-error')) {

      if (nrow(layerfracdata) == 0)
        message(sprintf('no fractionated samples found for selected prep_code (%s) and analyzed_size_frac (%s)',
                paste0(prep_code, collapse = ", "), paste0(analyzed_size_frac, collapse = ", ")))

      layerdata <- merge(layerdata, layerfracdata[,c("labsampnum", colnames(layerfracdata)[!colnames(layerfracdata) %in% colnames(layerdata)])], by = "labsampnum", all.x = TRUE, incomparables = NA)
    }
  }
  if (!is.null(layerdata$prep_code)) {
    layerdata$prep_code[is.na(layerdata$prep_code)] <- ""
  }
  layerdata
}

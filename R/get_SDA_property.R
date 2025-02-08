# Based on ssurgoOnDemand by chad ferguson and jason nemecek
# SDA_properties.R: translation of SDA_Properties.py into soilDB-style R function by andrew brown
# created: 2021/04/03
# last update: 2021/07/07

#' Get map unit properties from Soil Data Access
#'
#' @param property character vector of labels from property dictionary tables (see details) OR physical column names from `component` or `chorizon` table.
#' @param method one of: "Dominant Component (Category)", "Dominant Component (Numeric)", "Weighted Average", "MIN", "MAX", "Dominant Condition", or "None". If "None" is selected, the number of rows returned will depend on whether a component or horizon level property was selected, otherwise the result will be 1:1 with the number of map units.
#' @param areasymbols vector of soil survey area symbols
#' @param mukeys vector of map unit keys
#' @param WHERE character containing SQL WHERE clause specified in terms of fields in `legend` or `mapunit` tables, used in lieu of `mukeys` or `areasymbols`. With aggregation method `"NONE"` the WHERE clause may additionally contain logic for columns from the `component` and `chorizon` table.
#' @param top_depth Default: `0` (centimeters); a numeric value for upper boundary (top depth) used only for method="Weighted Average", "Dominant Component (Numeric)", and "MIN/MAX"
#' @param bottom_depth Default: `200` (centimeters); a numeric value for lower boundary (bottom depth) used only for method="Weighted Average", "Dominant Component (Numeric)", and "MIN/MAX"
#' @param FUN Optional: character representing SQL aggregation function either "MIN" or "MAX" used only for method="min/max"; this argument is calculated internally if you specify `method="MIN"` or `method="MAX"`
#' @param include_minors Include minor components in "Weighted Average" or "MIN/MAX" results? Default: `TRUE`
#' @param miscellaneous_areas Include miscellaneous areas  (non-soil components) in results? Default: `FALSE`. Now works with all `method` types)
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#' @param dsn Path to local SQLite database or a DBIConnection object. If `NULL` (default) use Soil Data Access API via `SDA_query()`.
#' @examplesIf curl::has_internet() && requireNamespace("httr", quietly = TRUE)
#' \donttest{
#'
#'  # get 1/3 bar bulk density [0,25] centimeter depth weighted average from dominant component
#'  get_SDA_property(property = c("dbthirdbar_l","dbthirdbar_r","dbthirdbar_h"),
#'                   method = "Dominant Component (Numeric)",
#'                   areasymbols = "CA630",
#'                   top_depth = 0,
#'                   bottom_depth = 25)
#' }
#'
#' @details
#'
#' The `property` argument refers to one of the property names or columns specified in the tables below. Note that `property` can be specified as either a character vector of labeled properties, such as `"Bulk Density 0.33 bar H2O - Rep Value"`, OR physical column names such as `"dbthirdbar_r"`. To get "low" and "high" values for a particular property, replace the `_r` with `_l` or `_h` in the physical column name; for example `property = c("dbthirdbar_l","dbthirdbar_r","dbthirdbar_h")`. You can view exhaustive lists of component and component horizon level properties in the Soil Data Access ["Tables and Columns Report"](https://sdmdataaccess.sc.egov.usda.gov/documents/TablesAndColumnsReport.pdf).
#'
#'
#'  ## Selected Component-level Properties
#'
#'  |**Property (Component)**                         |**Column**         |
#'  |:------------------------------------------------|:------------------|
#'  |Range Production - Favorable Year                |rsprod_h           |
#'  |Range Production - Normal Year                   |rsprod_r           |
#'  |Range Production - Unfavorable Year              |rsprod_l           |
#'  |Corrosion of Steel                               |corsteel           |
#'  |Corrosion of Concrete                            |corcon             |
#'  |Drainage Class                                   |drainagecl         |
#'  |Hydrologic Group                                 |hydgrp             |
#'  |Taxonomic Class Name                             |taxclname          |
#'  |Taxonomic Order                                  |taxorder           |
#'  |Taxonomic Suborder                               |taxsuborder        |
#'  |Taxonomic Temperature Regime                     |taxtempregime      |
#'  |Wind Erodibility Group                           |weg                |
#'  |Wind Erodibility Index                           |wei                |
#'  |t Factor                                         |tfact              |
#'
#'  ## Selected Horizon-level Properties
#'
#'  |**Property (Horizon)**                           |**Column**         |
#'  |:------------------------------------------------|:------------------|
#'  |0.1 bar H2O - Rep Value                          |wtenthbar_r        |
#'  |0.33 bar H2O - Rep Value                         |wthirdbar_r        |
#'  |15 bar H2O - Rep Value                           |wfifteenbar_r      |
#'  |Available Water Capacity - Rep Value             |awc_r              |
#'  |Bray 1 Phosphate - Rep Value                     |pbray1_r           |
#'  |Bulk Density 0.1 bar H2O - Rep Value             |dbtenthbar_r       |
#'  |Bulk Density 0.33 bar H2O - Rep Value            |dbthirdbar_r       |
#'  |Bulk Density 15 bar H2O - Rep Value              |dbfifteenbar_r     |
#'  |Bulk Density oven dry - Rep Value                |dbovendry_r        |
#'  |CaCO3 Clay - Rep Value                           |claysizedcarb_r    |
#'  |Calcium Carbonate - Rep Value                    |caco3_r            |
#'  |Cation Exchange Capacity - Rep Value             |cec7_r             |
#'  |Coarse Sand - Rep Value                          |sandco_r           |
#'  |Coarse Silt - Rep Value                          |siltco_r           |
#'  |Effective Cation Exchange Capacity - Rep Value   |ecec_r             |
#'  |Electrial Conductivity 1:5 by volume - Rep Value |ec15_r             |
#'  |Electrical Conductivity - Rep Value              |ec_r               |
#'  |Exchangeable Sodium Percentage - Rep Value       |esp_r              |
#'  |Extract Aluminum - Rep Value                     |extral_r           |
#'  |Extractable Acidity - Rep Value                  |extracid_r         |
#'  |Fine Sand - Rep Value                            |sandfine_r         |
#'  |Fine Silt - Rep Value                            |siltfine_r         |
#'  |Free Iron - Rep Value                            |freeiron_r         |
#'  |Gypsum - Rep Value                               |gypsum_r           |
#'  |Kf                                               |kffact             |
#'  |Ki                                               |kifact             |
#'  |Kr                                               |krfact             |
#'  |Kw                                               |kwfact             |
#'  |LEP - Rep Value                                  |lep_r              |
#'  |Liquid Limit - Rep Value                         |ll_r               |
#'  |Medium Sand - Rep Value                          |sandmed_r          |
#'  |Organic Matter - Rep Value                       |om_r               |
#'  |Oxalate Aluminum - Rep Value                     |aloxalate_r        |
#'  |Oxalate Iron - Rep Value                         |feoxalate_r        |
#'  |Oxalate Phosphate - Rep Value                    |poxalate_r         |
#'  |Plasticity Index - Rep Value                     |pi_r               |
#'  |Rock Fragments 3 - 10 inches - Rep Value         |frag3to10_r        |
#'  |Rock Fragments > 10 inches - Rep Value           |fraggt10_r         |
#'  |Rubbed Fiber % - Rep Value                       |fiberrubbedpct_r   |
#'  |Satiated H2O - Rep Value                         |wsatiated_r        |
#'  |Saturated Hydraulic Conductivity - Rep Value     |ksat_r             |
#'  |Sodium Adsorption Ratio - Rep Value              |sar_r              |
#'  |Sum of Bases - Rep Value                         |sumbases_r         |
#'  |Total Clay - Rep Value                           |claytotal_r        |
#'  |Total Phosphate - Rep Value                      |ptotal_r           |
#'  |Total Sand - Rep Value                           |sandtotal_r        |
#'  |Total Silt - Rep Value                           |silttotal_r        |
#'  |Unrubbed Fiber % - Rep Value                     |fiberunrubbedpct_r |
#'  |Very Coarse Sand - Rep Value                     |sandvc_r           |
#'  |Very Fine Sand - Rep Value                       |sandvf_r           |
#'  |Water Soluble Phosphate - Rep Value              |ph2osoluble_r      |
#'  |no. 10 sieve - Rep Value                         |sieveno10_r        |
#'  |no. 200 sieve - Rep Value                        |sieveno200_r       |
#'  |no. 4 sieve - Rep Value                          |sieveno4_r         |
#'  |no. 40 sieve - Rep Value                         |sieveno40_r        |
#'  |pH .01M CaCl2 - Rep Value                        |ph01mcacl2_r       |
#'  |pH 1:1 water - Rep Value                         |ph1to1h2o_r        |
#'  |pH Oxidized - Rep Value                          |phoxidized_r       |
#' @author Jason Nemecek, Chad Ferguson, Andrew Brown
#' @return a data.frame with result
#' @export
get_SDA_property <-
  function(property, # property -- a label or column name from property dictionary
           method = c("Dominant Component (Category)", "Weighted Average",
                      "Min/Max", "Dominant Component (Numeric)", "Dominant Condition",
                      "None"),
           areasymbols = NULL, # vector of areasymbols
           mukeys = NULL, # vector of mukeys
           WHERE = NULL,
           top_depth = 0, # used for method="weighted average", "dominant component (numeric)", "min/max"
           bottom_depth = 200, # used for method="weighted average", "dominant component (numeric)", "min/max"
           FUN = NULL,
           include_minors = FALSE,
           miscellaneous_areas = FALSE,
           query_string = FALSE,
           dsn = NULL)
    {

  q <- .constructPropQuery(method = method,
                           property = property,
                           areasymbols = areasymbols,
                           mukeys = mukeys,
                           WHERE = WHERE,
                           top_depth = top_depth,
                           bottom_depth = bottom_depth,
                           include_minors = include_minors,
                           miscellaneous_areas = miscellaneous_areas,
                           FUN = FUN)

  if (query_string) return(q)

  # execute query
  res <- SDA_query(q, dsn = dsn)
  
  # return if bad
  if (inherits(res, 'try-error')) {
    return(res)
  }

  return(res)
}

.propertyAggMethod <-  function(method) {
  # match to one of the available aggregation methods
  labels <- c("Dominant Component (Category)",
              "Weighted Average",
              "Min/Max",
              "Dominant Component (Numeric)",
              "Dominant Condition",
              "None")
  method <- match.arg(toupper(method), toupper(labels))

  # determine column name prefix/suffix for method
  suffixes <- c('_dom_comp_cat',
                '_wtd_avg',
                '_min_max',
                '_dom_comp_num',
                '_dom_cond', '')
  modifier <- suffixes[match(method, toupper(labels))]

  # return list with method and modifier
  return(list(method = method,
              modifier = modifier))
}

# lookup table of label : representative value
.propertyDictionary <- function() {
  list(
    'Range Production - Favorable Year' = 'rsprod_h',
    'Range Production - Normal Year' = 'rsprod_r',
    'Range Production - Unfavorable Year' = 'rsprod_l',
    '0.1 bar H2O - Rep Value' = 'wtenthbar_r',
    '0.33 bar H2O - Rep Value' = 'wthirdbar_r',
    '15 bar H2O - Rep Value' = 'wfifteenbar_r',
    'Available Water Capacity - Rep Value' = 'awc_r',
    'Bray 1 Phosphate - Rep Value' = 'pbray1_r',
    'Bulk Density 0.1 bar H2O - Rep Value' = 'dbtenthbar_r',
    'Bulk Density 0.33 bar H2O - Rep Value' = 'dbthirdbar_r',
    'Bulk Density 15 bar H2O - Rep Value' = 'dbfifteenbar_r',
    'Bulk Density oven dry - Rep Value' = 'dbovendry_r',
    'CaCO3 Clay - Rep Value' = 'claysizedcarb_r',
    'Calcium Carbonate - Rep Value' = 'caco3_r',
    'Cation Exchange Capacity - Rep Value' = 'cec7_r',
    'Coarse Sand - Rep Value' = 'sandco_r',
    'Coarse Silt - Rep Value' = 'siltco_r',
    'Corrosion of Steel' = 'corsteel',
    'Corrosion of Concrete' = 'corcon',
    'Drainage Class' = 'drainagecl',
    'Effective Cation Exchange Capacity - Rep Value' = 'ecec_r',
    'Electrical Conductivity 1:5 by volume - Rep Value' = 'ec15_r',
    'Electrical Conductivity - Rep Value' = 'ec_r',
    'Exchangeable Sodium Percentage - Rep Value' = 'esp_r',
    'Extract Aluminum - Rep Value' = 'extral_r',
    'Extractable Acidity - Rep Value' = 'extracid_r',
    'Fine Sand - Rep Value' = 'sandfine_r',
    'Fine Silt - Rep Value' = 'siltfine_r',
    'Free Iron - Rep Value' = 'freeiron_r',
    'Gypsum - Rep Value' = 'gypsum_r',
    'Hydrologic Group' = 'hydgrp',
    'Kf' = 'kffact',
    'Ki' = 'kifact',
    'Kr' = 'krfact',
    'Kw' = 'kwfact',
    'LEP - Rep Value' = 'lep_r',
    'Liquid Limit - Rep Value' = 'll_r',
    'Medium Sand - Rep Value' = 'sandmed_r',
    'Organic Matter - Rep Value' = 'om_r',
    'Oxalate Aluminum - Rep Value' = 'aloxalate_r',
    'Oxalate Iron - Rep Value' = 'feoxalate_r',
    'Oxalate Phosphate - Rep Value' = 'poxalate_r',
    'Plasticity Index - Rep Value' = 'pi_r',
    'Rock Fragments 3 - 10 inches - Rep Value' = 'frag3to10_r',
    'Rock Fragments > 10 inches - Rep Value' = 'fraggt10_r',
    'Rubbed Fiber % - Rep Value' = 'fiberrubbedpct_r',
    'Satiated H2O - Rep Value' = 'wsatiated_r',
    'Saturated Hydraulic Conductivity - Rep Value' = 'ksat_r',
    'Sodium Adsorption Ratio - Rep Value' = 'sar_r',
    'Sum of Bases - Rep Value' = 'sumbases_r',
    'Taxonomic Class Name' = 'taxclname',
    'Taxonomic Order' = 'taxorder',
    'Taxonomic Suborder' = 'taxsuborder',
    'Taxonomic Temperature Regime' = 'taxtempregime',
    'Total Clay - Rep Value' = 'claytotal_r',
    'Total Phosphate - Rep Value' = 'ptotal_r',
    'Total Sand - Rep Value' = 'sandtotal_r',
    'Total Silt - Rep Value' = 'silttotal_r',
    'Unrubbed Fiber % - Rep Value' = 'fiberunrubbedpct_r',
    'Very Coarse Sand - Rep Value' = 'sandvc_r',
    'Very Fine Sand - Rep Value' = 'sandvf_r',
    'Water Soluble Phosphate - Rep Value' = 'ph2osoluble_r',
    'Wind Erodibility Group' = 'weg',
    'Wind Erodibility Index' = 'wei',
    'no. 10 sieve - Rep Value' = 'sieveno10_r',
    'no. 200 sieve - Rep Value' = 'sieveno200_r',
    'no. 4 sieve - Rep Value' = 'sieveno4_r',
    'no. 40 sieve - Rep Value' = 'sieveno40_r',
    'pH .01M CaCl2 - Rep Value' = 'ph01mcacl2_r',
    'pH 1:1 water - Rep Value' = 'ph1to1h2o_r',
    'pH Oxidized - Rep Value' = 'phoxidized_r',
    't Factor' = 'tfact'
  )
}

.constructPropQuery <- function(method, property,
                                areasymbols = NULL, mukeys = NULL, WHERE = NULL,
                                top_depth = 0, bottom_depth = 200, FUN = NULL,
                                include_minors = FALSE,
                                miscellaneous_areas = FALSE) {
  # SQL by Jason Nemecek

  if (is.null(mukeys) && is.null(areasymbols) && is.null(WHERE)) {
    stop("Please specify one of the following arguments: mukeys, areasymbols, WHERE", call. = FALSE)
  }
  if (!is.null(mukeys)) {
    WHERE <- paste("mapunit.mukey IN", format_SQL_in_statement(as.integer(mukeys)))
  } else if (!is.null(areasymbols)) {
    WHERE <- paste("legend.areasymbol IN", format_SQL_in_statement(areasymbols))
  }

  if (method != "NONE" &&
      (grepl("component\\.|chorizon\\.", WHERE)[1] ||
       grepl(paste0(.valid_chorizon_columns(), collapse = "|"), WHERE)[1])) {
    stop('WHERE clause containing component or chorizon level fields is only supported when `method = "NONE"`', call. = FALSE)
  }

  # check property, case insensitive, against dictionary
  #  user can also specify columns that aren't in the dictionary using physical column name
  property_up <- toupper(property)
  lut <- .propertyDictionary()
  names(lut) <- toupper(names(lut))
  agg_property <- lut[property_up]

  # check whether properties are in dictionary
  not_in_lut <- sapply(agg_property, function(x) is.null(x) || is.na(x))

  # if they are all not in dictionary, assume user knows what they are doing
  #  this means you can't mix column name input and readable label input in same call
  if (all(not_in_lut)) {

    ## strict: only allow properties from the lookup table
    # names(lut) <- toupper(.propertyDictionary())
    # agg_property <- lut[property]
    # if(any(is.null(agg_property))) stop("property must be a label or column name from SDA property dictionary (.propertyDictonary())", call. = FALSE)

    ## alternate: just assume they are either all component or all horizon column names
    # message('assuming `property` is a vector of component OR horizon-level column names')
    agg_property <- tolower(property)

  } else {

    # remove non-matching if using lookup table labels
    agg_property <- agg_property[!not_in_lut]
  }

  if (!is.character(method))
    stop('argument `method` should be character string containing aggregation method, or `"NONE"` for no aggregation', call. = FALSE)

  method <- toupper(method)

  if (method == "NONE") {
    # dput(colnames(SDA_query("SELECT TOP 1 * FROM chorizon"))) # without cokey
    is_hz <- agg_property %in% .valid_chorizon_columns()
  }

  FUN <- toupper(FUN)

  # check FUN arg for min max method
  if (method == "MIN/MAX") {
    FUN <- match.arg(FUN, c("MIN", "MAX"))
  # handle shorthand min/max for FUN passed as method
  } else if (method == "MAX") {
    method <- "MIN/MAX"
    FUN <- "MAX"
  } else if (method == "MIN") {
    method <- "MIN/MAX"
    FUN <- "MIN"
  }

  # determine label and column prefix/suffix for selected method
  agg_method <- .propertyAggMethod(method)

  # handle top_depth / bottom_depth mis-specified
  if (agg_method$method %in% c("WEIGHTED AVERAGE","DOMINANT COMPONENT (NUMERIC)")) {
    if (!all(c(is.numeric(top_depth), is.numeric(bottom_depth))) ||
         any(c(is.na(top_depth), is.na(bottom_depth)))) {
      stop("`top_depth` and `bottom_depth` must be numeric, non-NA depths in centimeters for method='weighted average' or 'dominant component (numeric)'",
           call. = FALSE)
    }
  }

  # define several helper methods
  .property_dominant_condition_category <- function(property, miscellaneous_areas = FALSE) {
    sprintf("(SELECT TOP 1 %s FROM mapunit AS mu
          INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey %s
          GROUP BY %s, comppct_r ORDER BY SUM(comppct_r) over(partition by %s) DESC) AS %s",
          property,
          ifelse(miscellaneous_areas, ""," AND component.compkind != 'Miscellaneous area'"),
          property, property, property)
  }

  .property_min_max <- function(property, top_depth, bottom_depth, FUN, include_minors = TRUE, miscellaneous_areas = FALSE) {
    sprintf("(SELECT TOP 1 %s(chm1.%s) FROM component AS cm1
             INNER JOIN chorizon AS chm1 ON cm1.cokey = chm1.cokey AND cm1.cokey = component.cokey %s
             WHERE chm1.hzdept_r BETWEEN %s AND %s OR chm1.hzdepb_r BETWEEN %s AND %s) AS %s",
            FUN, property, 
            
            ifelse(miscellaneous_areas, ""," AND component.compkind != 'Miscellaneous area'"),
            top_depth, bottom_depth, top_depth, bottom_depth, property)
  }

  .property_weighted_average <- function(property,
                                         top_depth,
                                         bottom_depth,
                                         WHERE,
                                         dominant = FALSE,
                                         include_minors = FALSE,
                                         miscellaneous_areas = FALSE) {

    n <- 1:length(property)
    stopifnot(n > 0)

    sprintf("SELECT mukey, areasymbol, musym, muname
            INTO #kitchensink
            FROM legend
            INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
            SELECT mapunit.mukey, cokey, comppct_r, compkind, majcompflag,
            SUM (comppct_r) OVER (PARTITION BY mapunit.mukey) AS SUM_COMP_PCT
            INTO #comp_temp
            FROM legend AS legend
            INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
            INNER JOIN component ON component.mukey = mapunit.mukey %s %s %s
            SELECT cokey, compkind, majcompflag, SUM_COMP_PCT,
            %s
            INTO #comp_temp3
            FROM #comp_temp
            SELECT mapunit.mukey, areasymbol, musym, muname, component.cokey AS cokey, chorizon.chkey/1 AS chkey, compname, compkind, hzname, hzdept_r, hzdepb_r, CASE WHEN hzdept_r < %s THEN %s ELSE hzdept_r END AS hzdept_r_ADJ,
            CASE WHEN hzdepb_r > %s THEN %s ELSE hzdepb_r END AS hzdepb_r_ADJ,
            %s, %s,
            comppct_r,
            %s
            INTO #main
            FROM legend
            INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
            INNER JOIN component ON component.mukey = mapunit.mukey %s
            INNER JOIN chorizon ON chorizon.cokey = component.cokey AND hzdepb_r > %s AND hzdept_r <= %s
            WHERE chorizon.hzdept_r IS NOT NULL
            ORDER BY mapunit.mukey, areasymbol, musym, muname, comppct_r DESC, cokey, hzdept_r, hzdepb_r
            %s",
            WHERE,
            WHERE,
            ifelse(include_minors, "", "AND component.majcompflag = 'Yes'"),
            ifelse(miscellaneous_areas, ""," AND component.compkind != 'Miscellaneous area'"),
            ifelse(dominant, paste0("            AND component.cokey = (SELECT TOP 1 c2.cokey FROM component AS c2
                            INNER JOIN mapunit AS mm1 ON c2.mukey = mm1.mukey AND c2.mukey = mapunit.mukey ",
                            ifelse(miscellaneous_areas, ""," AND c2.compkind != 'Miscellaneous area'"),"
                            ORDER BY c2.comppct_r DESC, c2.cokey)"), ""),
            paste0(sprintf("CASE WHEN comppct_r = SUM_COMP_PCT THEN 1 ELSE CAST((#comp_temp.comppct_r) AS decimal(5,2)) / SUM_COMP_PCT END AS WEIGHTED_COMP_PCT%s", n), collapse = ", "),
            top_depth, top_depth,
            bottom_depth, bottom_depth,
            paste0(sprintf("CASE WHEN %s is NULL THEN NULL ELSE CAST (CASE WHEN hzdepb_r > %s THEN %s ELSE hzdepb_r END - CASE WHEN hzdept_r < %s THEN %s ELSE hzdept_r END AS decimal(5,2)) END AS thickness_wt_%s", property, bottom_depth, bottom_depth, top_depth, top_depth, property), collapse = ", \n"),
            paste0(sprintf("CAST (SUM(CAST((CASE WHEN hzdepb_r > %s THEN %s WHEN %s is NULL THEN NULL ELSE hzdepb_r END - CASE WHEN hzdept_r < %s THEN %s WHEN %s is NULL THEN NULL ELSE hzdept_r END) AS decimal(5,2))) OVER (PARTITION BY component.cokey) AS decimal(5,2)) AS sum_thickness_%s", bottom_depth, bottom_depth, property, top_depth, top_depth, property, property), collapse = ", \n"),
            paste0(property, collapse = ", "),
            WHERE,
            ifelse(miscellaneous_areas, ""," AND component.compkind != 'Miscellaneous area'"),
            top_depth, bottom_depth,
            sprintf("SELECT #main.mukey, #main.areasymbol, #main.musym, #main.muname, 
#main.cokey, #main.chkey, #main.compname, #main.compkind, hzname, hzdept_r, hzdepb_r, hzdept_r_ADJ, hzdepb_r_ADJ, %s, %s, comppct_r, SUM_COMP_PCT, %s, %s
                        INTO #comp_temp2
                        FROM #main
                        INNER JOIN #comp_temp3 ON #comp_temp3.cokey = #main.cokey
                        ORDER BY #main.mukey, comppct_r DESC, 
                                 #main.cokey, #main.areasymbol, #main.musym, #main.muname, 
                                 hzdept_r, hzdepb_r
                        SELECT DISTINCT #comp_temp2.mukey, #comp_temp2.cokey, %s
                          INTO #weights
                          FROM #comp_temp2
                          %s
                        SELECT DISTINCT #weights.mukey, %s
                          INTO #weights2
                          FROM #weights
                          GROUP BY #weights.mukey
                        SELECT #comp_temp2.mukey, #comp_temp2.cokey, %s, %s
                          INTO #last_step
                          FROM #comp_temp2
                          INNER JOIN #weights2 ON #weights2.mukey = #comp_temp2.mukey
                          %s
                          GROUP BY #comp_temp2.mukey, #comp_temp2.cokey, %s, %s, %s
                          SELECT #kitchensink.mukey, #last_step.cokey, areasymbol, musym, muname, %s, %s
                            INTO #last_step2
                            FROM #last_step
                            RIGHT OUTER JOIN #kitchensink ON #kitchensink.mukey = #last_step.mukey
                            GROUP BY #kitchensink.areasymbol, #kitchensink.musym, #kitchensink.muname, #kitchensink.mukey, %s, %s, #last_step.cokey
                            ORDER BY #kitchensink.mukey, #kitchensink.areasymbol, #kitchensink.musym, #kitchensink.muname
                            SELECT #last_step2.mukey, #last_step2.areasymbol, #last_step2.musym, #last_step2.muname, %s
                              FROM #last_step2
                              LEFT OUTER JOIN #last_step ON #last_step.mukey = #last_step2.mukey
                                  GROUP BY #last_step2.areasymbol, #last_step2.musym, #last_step2.muname, #last_step2.mukey, %s
                                  ORDER BY #last_step2.mukey, #last_step2.areasymbol, #last_step2.musym, #last_step2.muname, %s",
      paste0(sprintf("(CASE WHEN ISNULL(sum_thickness_%s, 0) = 0 THEN 0 ELSE WEIGHTED_COMP_PCT%s END) AS CORRECT_COMP_PCT%s", property, n, n), collapse = ", "),
      paste0(sprintf("ISNULL(thickness_wt_%s, 0) AS thickness_wt_%s, sum_thickness_%s", property, property, property), collapse = ", "),
      paste0(property, collapse = ", "),
      paste0(sprintf("((thickness_wt_%s / (CASE WHEN sum_thickness_%s = 0 THEN 1 ELSE sum_thickness_%s END)) * %s) AS DEPTH_WEIGHTED_AVERAGE%s",
               property, property, property, property, n), collapse = ", "),
      paste0(sprintf("CORRECT_COMP_PCT%s", n), collapse = ", "),
      paste0("WHERE ", paste0(sprintf("DEPTH_WEIGHTED_AVERAGE%s IS NOT NULL", n), collapse = " OR ")),
      paste0(sprintf("SUM(CORRECT_COMP_PCT%s) AS RATED_PCT%s", n, n), collapse = ", "),
      paste0(sprintf("#weights2.RATED_PCT%s", n), collapse = ", "),
      paste0(sprintf("SUM(CORRECT_COMP_PCT%s * DEPTH_WEIGHTED_AVERAGE%s) AS COMP_WEIGHTED_AVERAGE%s", n, n, n), collapse = ", "),
      paste0("WHERE ", paste0(sprintf("DEPTH_WEIGHTED_AVERAGE%s IS NOT NULL", n), collapse = " OR ")),
      paste0(sprintf("CORRECT_COMP_PCT%s", n), collapse = ", "),
      paste0(sprintf("#weights2.RATED_PCT%s", n), collapse = ", "),
      paste0(sprintf("DEPTH_WEIGHTED_AVERAGE%s", n), collapse = ", "),
      paste0(sprintf("#last_step.RATED_PCT%s", n), collapse = ", "),
      paste0(sprintf("CAST (SUM((CASE WHEN RATED_PCT%s = 0 THEN NULL ELSE COMP_WEIGHTED_AVERAGE%s END) / (CASE WHEN RATED_PCT%s = 0 THEN 1 ELSE RATED_PCT%s END)) OVER (PARTITION BY #kitchensink.mukey) AS decimal(10,2)) AS %s",
                     n, n, n, n, property), collapse = ", "),
      paste0(sprintf("#last_step.RATED_PCT%s", n), collapse = ", "),
      paste0(sprintf("COMP_WEIGHTED_AVERAGE%s", n), collapse = ", "),
      paste0(sprintf("#last_step2.%s", property), collapse = ", "),
      paste0(sprintf("#last_step2.%s", property), collapse = ", "),
      paste0(sprintf("#last_step2.%s", property), collapse = ", ")))
  }

  .property_dominant_component_numeric <- function(property, top_depth, bottom_depth, WHERE, miscellaneous_areas = FALSE) {
    # dominant component numeric is a more specific case of weighted average
    .property_weighted_average(property, top_depth, bottom_depth, WHERE, dominant = TRUE, include_minors = TRUE, miscellaneous_areas = miscellaneous_areas)
  }

  # create query based on method
  switch(toupper(agg_method$method),
    # dominant component (category)
    "DOMINANT COMPONENT (CATEGORY)" =
    sprintf("SELECT mapunit.mukey, areasymbol, musym, muname, %s
             FROM legend
             INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
             INNER JOIN component ON component.mukey = mapunit.mukey AND
                                     component.cokey = (SELECT TOP 1 c1.cokey FROM component AS c1
                                                INNER JOIN mapunit AS mu ON component.mukey = mu.mukey AND
                                                                            c1.mukey = mapunit.mukey %s
                                                ORDER BY c1.comppct_r DESC, c1.cokey)",

            paste0(sapply(agg_property, function(x) sprintf("%s AS %s", x, x)), collapse = ", "),
            WHERE,
            ifelse(miscellaneous_areas, ""," AND c1.compkind != 'Miscellaneous area'")),

    # weighted average (.weighted_average handles vector agg_property)
    "WEIGHTED AVERAGE" = .property_weighted_average(agg_property, top_depth, bottom_depth, WHERE, include_minors = include_minors, miscellaneous_areas = miscellaneous_areas),
    "MIN/MAX" =
      sprintf("SELECT mapunit.mukey, areasymbol, musym, muname, %s
               INTO #funagg
                    FROM legend
                    INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
                    LEFT JOIN component ON component.mukey = mapunit.mukey %s %s
               SELECT mukey, areasymbol, musym, muname,  %s FROM #funagg
               GROUP BY mukey, areasymbol, musym, muname",
              paste0(sapply(agg_property, function(x) .property_min_max(x, top_depth, bottom_depth, FUN = FUN, miscellaneous_areas = miscellaneous_areas)), collapse = ", "),
              WHERE,
              ifelse(include_minors, ""," AND component.majcompflag = 'Yes'"),
              ifelse(miscellaneous_areas, ""," AND component.compkind != 'Miscellaneous area'"),
              paste0(paste0(FUN, "(", agg_property, ") AS ", agg_property), collapse = ",")),

    # dominant component (numeric) (.dominant_component_numeric handles vector agg_property)
    "DOMINANT COMPONENT (NUMERIC)" = .property_dominant_component_numeric(agg_property, top_depth, bottom_depth, WHERE, miscellaneous_areas),

    # dominant condition
    "DOMINANT CONDITION" =
    sprintf("SELECT mapunit.mukey, areasymbol, musym, muname, %s
             FROM legend
              INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND %s
              INNER JOIN component ON component.mukey = mapunit.mukey AND
                                           component.cokey = (SELECT TOP 1 c1.cokey FROM component AS c1
                                                      INNER JOIN mapunit AS mu ON component.mukey = mu.mukey AND
                                                                            c1.mukey = mapunit.mukey %s
                                                      ORDER BY c1.comppct_r DESC, c1.cokey)
              GROUP BY areasymbol, musym, muname, mapunit.mukey, component.cokey, compname, comppct_r
              ORDER BY mapunit.mukey, areasymbol, musym, muname, comppct_r DESC, component.cokey",
            paste0(sapply(agg_property, .property_dominant_condition_category), collapse = ", "),
            WHERE,
            ifelse(miscellaneous_areas, ""," AND c1.compkind != 'Miscellaneous area'")),

    # NO AGGREGATION
  "NONE" = sprintf("SELECT mapunit.mukey, component.cokey, areasymbol, musym, muname,
                           compname, compkind, component.comppct_r, majcompflag, 
                           %s %s%s %s
             FROM legend
              INNER JOIN mapunit ON mapunit.lkey = legend.lkey
              INNER JOIN component ON component.mukey = mapunit.mukey %s%s
              ORDER BY mapunit.mukey, areasymbol, musym, muname, component.comppct_r DESC, component.cokey%s",
            ifelse(any(is_hz), "chorizon.chkey AS chkey, chorizon.hzdept_r AS hzdept_r, chorizon.hzdepb_r AS hzdepb_r,", ""),
            paste0(sapply(agg_property[!is_hz], function(x) sprintf("component.%s AS %s", x, x)), collapse = ", "),
            ifelse(any(is_hz) && !all(is_hz), ",", ""),
            paste0(sapply(agg_property[is_hz], function(x) sprintf("chorizon.%s AS %s", x, x)), collapse = ", "),
            ifelse(miscellaneous_areas, ""," AND component.compkind != 'Miscellaneous area'"),
            ifelse(any(is_hz),  paste0("\nINNER JOIN chorizon ON chorizon.cokey = component.cokey", " AND ", WHERE), paste0("AND ", WHERE)),
            ifelse(any(is_hz), ", hzdept_r", ""))
  )

}

.valid_chorizon_columns <- function() {
  c("hzname", "desgndisc", "desgnmaster", "desgnmasterprime", "desgnvert",
    "hzdept_l", "hzdept_r", "hzdept_h", "hzdepb_l", "hzdepb_r", "hzdepb_h",
    "hzthk_l", "hzthk_r", "hzthk_h", "fraggt10_l", "fraggt10_r",
    "fraggt10_h", "frag3to10_l", "frag3to10_r", "frag3to10_h", "sieveno4_l",
    "sieveno4_r", "sieveno4_h", "sieveno10_l", "sieveno10_r", "sieveno10_h",
    "sieveno40_l", "sieveno40_r", "sieveno40_h", "sieveno200_l",
    "sieveno200_r", "sieveno200_h", "sandtotal_l", "sandtotal_r",
    "sandtotal_h", "sandvc_l", "sandvc_r", "sandvc_h", "sandco_l",
    "sandco_r", "sandco_h", "sandmed_l", "sandmed_r", "sandmed_h",
    "sandfine_l", "sandfine_r", "sandfine_h", "sandvf_l", "sandvf_r",
    "sandvf_h", "silttotal_l", "silttotal_r", "silttotal_h", "siltco_l",
    "siltco_r", "siltco_h", "siltfine_l", "siltfine_r", "siltfine_h",
    "claytotal_l", "claytotal_r", "claytotal_h", "claysizedcarb_l",
    "claysizedcarb_r", "claysizedcarb_h", "om_l", "om_r", "om_h",
    "dbtenthbar_l", "dbtenthbar_r", "dbtenthbar_h", "dbthirdbar_l",
    "dbthirdbar_r", "dbthirdbar_h", "dbfifteenbar_l", "dbfifteenbar_r",
    "dbfifteenbar_h", "dbovendry_l", "dbovendry_r", "dbovendry_h",
    "partdensity", "ksat_l", "ksat_r", "ksat_h", "awc_l", "awc_r",
    "awc_h", "wtenthbar_l", "wtenthbar_r", "wtenthbar_h", "wthirdbar_l",
    "wthirdbar_r", "wthirdbar_h", "wfifteenbar_l", "wfifteenbar_r",
    "wfifteenbar_h", "wsatiated_l", "wsatiated_r", "wsatiated_h",
    "lep_l", "lep_r", "lep_h", "ll_l", "ll_r", "ll_h", "pi_l", "pi_r",
    "pi_h", "aashind_l", "aashind_r", "aashind_h", "kwfact", "kffact",
    "caco3_l", "caco3_r", "caco3_h", "gypsum_l", "gypsum_r", "gypsum_h",
    "sar_l", "sar_r", "sar_h", "ec_l", "ec_r", "ec_h", "cec7_l",
    "cec7_r", "cec7_h", "ecec_l", "ecec_r", "ecec_h", "sumbases_l",
    "sumbases_r", "sumbases_h", "ph1to1h2o_l", "ph1to1h2o_r", "ph1to1h2o_h",
    "ph01mcacl2_l", "ph01mcacl2_r", "ph01mcacl2_h", "freeiron_l",
    "freeiron_r", "freeiron_h", "feoxalate_l", "feoxalate_r", "feoxalate_h",
    "extracid_l", "extracid_r", "extracid_h", "extral_l", "extral_r",
    "extral_h", "aloxalate_l", "aloxalate_r", "aloxalate_h", "pbray1_l",
    "pbray1_r", "pbray1_h", "poxalate_l", "poxalate_r", "poxalate_h",
    "ph2osoluble_l", "ph2osoluble_r", "ph2osoluble_h", "ptotal_l",
    "ptotal_r", "ptotal_h", "excavdifcl", "excavdifms", "chkey")
}

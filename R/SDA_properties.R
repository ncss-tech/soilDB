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
#' @param top_depth Default: `0` (centimeters); a numeric value for upper boundary (top depth) used only for method="Weighted Average", "Dominant Component (Numeric)", and "MIN/MAX"
#' @param bottom_depth Default: `200` (centimeters); a numeric value for lower boundary (bottom depth) used only for method="Weighted Average", "Dominant Component (Numeric)", and "MIN/MAX"
#' @param FUN Optional: character representing SQL aggregation function either "MIN" or "MAX" used only for method="min/max"; this argument is calculated internally if you specify `method="MIN"` or `method="MAX"`
#' @param include_minors Include minor components in "Weighted Average" or "MIN/MAX" results?
#' @param miscellaneous_areas Include miscellaneous areas  (non-soil components) in "Weighted Average", "MIN" or "MAX" results?
#' @param query_string Default: `FALSE`; if `TRUE` return a character string containing query that would be sent to SDA via `SDA_query`
#'
#' @examples
#'
#' \donttest{
#' if(requireNamespace("curl") &
#'     curl::has_internet()) {
#'
#'  # get 1/3 bar bulk density [0,25] centimeter depth weighted average from dominant component
#'  get_SDA_property(property = c("dbthirdbar_l","dbthirdbar_r","dbthirdbar_h"),
#'                   method = "Dominant Component (Numeric)",
#'                   areasymbols = "CA630",
#'                   top_depth = 0,
#'                   bottom_depth = 25)
#' }
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
#' @importFrom soilDB format_SQL_in_statement SDA_query
get_SDA_property <-
  function(property, # property -- a label or column name from property dictionary
           method = c("Dominant Component (Category)", "Weighted Average",
                      "Min/Max", "Dominant Component (Numeric)", "Dominant Condition",
                      "None"),
           areasymbols = NULL, # vector of areasymbols
           mukeys = NULL, # vector of mukeys
           top_depth = 0, # used for method="weighted average" and "dominant component (numeric)"
           bottom_depth = 200, # used for method="weighted average" and "dominant component (numeric)"
           FUN = NULL,
           include_minors = FALSE,
           miscellaneous_areas = FALSE,
           query_string = FALSE) # used for method="min/max"
    {


  q <- .constructPropQuery(method = method,
                           property = property,
                           areasymbols = areasymbols,
                           mukeys = mukeys,
                           top_depth = top_depth,
                           bottom_depth = bottom_depth,
                           include_minors = include_minors,
                           miscellaneous_areas = miscellaneous_areas,
                           FUN = FUN)

  if (query_string) return(q)

  # execute query
  res <- suppressMessages(soilDB::SDA_query(q))

  # stop if bad
  if (inherits(res, 'try-error')) {
    warnings()
    stop(attr(res, 'condition'))
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
              "None", "None_Horizon")
  method <- match.arg(toupper(method), toupper(labels))

  # determine column name prefix/suffix for method
  suffixes <- c('_dom_comp_cat',
                '_wtd_avg',
                '_min_max',
                '_dom_comp_num',
                '_dom_cond', '', 'chorizon_')
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
                                areasymbols = NULL, mukeys = NULL,
                                top_depth = 0, bottom_depth = 200, FUN = NULL,
                                include_minors = FALSE,
                                miscellaneous_areas = FALSE) {
  # SQL by Jason Nemecek

  # must specify either mukeys or areasymbols
  stopifnot(!is.null(areasymbols) || !is.null(mukeys))

  # convert target mukey/areasymbol to SQL IN statement ('a','b','c')
  if (!is.null(areasymbols))
    areasymbols <- soilDB::format_SQL_in_statement(areasymbols)

  if (!is.null(mukeys))
    mukeys <- soilDB::format_SQL_in_statement(mukeys)

  # if areasymbols is specified, it is used preferentially, otherwise mukeys
  where_clause <- switch(as.character(is.null(areasymbols)),
                         "TRUE" = sprintf("mu.mukey IN %s", mukeys),
                         "FALSE" = sprintf("l.areasymbol IN %s", areasymbols))

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
    agg_property <- property

  } else {

    # remove non-matching if using lookup table labels
    agg_property <- agg_property[!not_in_lut]
  }

  if (!is.character(method))
    stop('argument `method` should be character string containing aggregation method, or `"NONE"` for no aggregation', call. = FALSE)

  method <- toupper(method)

  if (method == "NONE")
    if (all(agg_property %in% colnames(suppressMessages(SDA_query("SELECT TOP 1 * FROM chorizon")))))
      method <- "NONE_HORIZON"

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
    sprintf("(SELECT TOP 1 %s FROM mapunit
          INNER JOIN component ON component.mukey = mapunit.mukey AND mapunit.mukey = mu.mukey %s
          GROUP BY %s, comppct_r ORDER BY SUM(comppct_r) over(partition by %s) DESC) AS %s",
          property,
          ifelse(miscellaneous_areas, ""," AND component.compkind != 'Miscellaneous area'"),
          property, property, property)
  }

  .property_min_max <- function(property, top_depth, bottom_depth, FUN) {
    sprintf("(SELECT TOP 1 %s(chm1.%s) FROM component AS cm1
             INNER JOIN chorizon AS chm1 ON cm1.cokey = chm1.cokey AND cm1.cokey = c.cokey
             WHERE chm1.hzdept_r BETWEEN %s AND %s OR chm1.hzdepb_r BETWEEN %s AND %s) AS %s",
            FUN, property, top_depth, bottom_depth, top_depth, bottom_depth, property)
  }

  .property_weighted_average <- function(property, top_depth, bottom_depth, where_clause, dominant = FALSE, include_minors = FALSE, miscellaneous_areas = FALSE) {

    n <- 1:length(property)
    stopifnot(n > 0)
    sprintf("SELECT areasymbol, musym, muname, mukey
            INTO #kitchensink
            FROM legend  AS lks
            INNER JOIN  mapunit AS muks ON muks.lkey = lks.lkey AND %s
            SELECT mu1.mukey, cokey, comppct_r,
            SUM (comppct_r) OVER (PARTITION BY mu1.mukey) AS SUM_COMP_PCT
            INTO #comp_temp
            FROM legend AS l1
            INNER JOIN mapunit AS mu1 ON mu1.lkey = l1.lkey AND %s
            INNER JOIN component AS c1 ON c1.mukey = mu1.mukey %s
            %s
            %s
            SELECT cokey, SUM_COMP_PCT, CASE WHEN comppct_r = SUM_COMP_PCT THEN 1
            ELSE CAST (CAST (comppct_r AS decimal (5,2)) / CAST (SUM_COMP_PCT AS decimal (5,2)) AS decimal (5,2)) END AS WEIGHTED_COMP_PCT
            INTO #comp_temp3
            FROM #comp_temp
            SELECT areasymbol, musym, muname, mu.mukey/1 AS mukey, c.cokey AS cokey, ch.chkey/1 AS chkey, compname, hzname, hzdept_r, hzdepb_r, CASE WHEN hzdept_r < %s THEN %s ELSE hzdept_r END AS hzdept_r_ADJ,
            CASE WHEN hzdepb_r > %s THEN %s ELSE hzdepb_r END AS hzdepb_r_ADJ,
            %s, %s,
            comppct_r,
            %s
            INTO #main
            FROM legend AS l
            INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
            INNER JOIN component AS c ON c.mukey = mu.mukey
            INNER JOIN chorizon AS ch ON ch.cokey = c.cokey AND hzdepb_r >= %s AND hzdept_r <= %s
            INNER JOIN chtexturegrp AS cht ON ch.chkey = cht.chkey
            WHERE cht.rvindicator = 'yes' AND ch.hzdept_r IS NOT NULL
            ORDER BY areasymbol, musym, muname, mu.mukey, comppct_r DESC, cokey, hzdept_r, hzdepb_r
            %s",
            gsub("^(l|mu)\\.","\\1ks.", where_clause),
            gsub("^(l|mu)\\.","\\11.", where_clause),
            ifelse(include_minors, "", "AND majcompflag = 'Yes'"),
            ifelse(miscellaneous_areas, ""," AND c1.compkind != 'Miscellaneous area'"),
            ifelse(dominant, "            AND c1.cokey = (SELECT TOP 1 c2.cokey FROM component AS c2
                            INNER JOIN mapunit AS mm1 ON
                              c2.mukey = mm1.mukey AND c2.mukey = mu1.mukey
                            ORDER BY c2.comppct_r DESC, c2.cokey)", ""),
            top_depth, top_depth,
            bottom_depth, bottom_depth,
             paste0(sprintf("CASE WHEN %s is NULL THEN NULL ELSE CAST (CASE WHEN hzdepb_r > %s THEN %s ELSE hzdepb_r END - CASE WHEN hzdept_r < %s THEN %s ELSE hzdept_r END AS decimal (5,2)) END AS thickness_wt_%s", property, bottom_depth, bottom_depth, top_depth, top_depth, property), collapse=", \n"),
             paste0(sprintf("CAST (SUM(CAST((CASE WHEN hzdepb_r > %s THEN %s WHEN %s is NULL THEN NULL ELSE hzdepb_r END - CASE WHEN hzdept_r < %s THEN %s WHEN %s is NULL THEN NULL ELSE hzdept_r END) AS decimal (5,2))) OVER (PARTITION BY c.cokey) AS decimal (5,2)) AS sum_thickness_%s", bottom_depth, bottom_depth, property, top_depth, top_depth, property, property), collapse=", \n"),
            paste0(sprintf("CAST(%s AS decimal (5,2)) AS %s", property, property), collapse=", "),
            where_clause,
            top_depth, bottom_depth,
            sprintf("SELECT #main.areasymbol, #main.musym, #main.muname, #main.mukey,
#main.cokey, #main.chkey, #main.compname, hzname, hzdept_r, hzdepb_r, hzdept_r_ADJ, hzdepb_r_ADJ, %s, %s, comppct_r, SUM_COMP_PCT, WEIGHTED_COMP_PCT, %s
                        INTO #comp_temp2
                        FROM #main
                        INNER JOIN #comp_temp3 ON #comp_temp3.cokey=#main.cokey
                        ORDER BY #main.areasymbol, #main.musym, #main.muname, #main.mukey,
                                 comppct_r DESC,  #main.cokey,  hzdept_r, hzdepb_r
                        SELECT #comp_temp2.mukey, #comp_temp2.cokey, %s
                          INTO #last_step
                          FROM #comp_temp2
                          GROUP BY #comp_temp2.mukey, #comp_temp2.cokey, WEIGHTED_COMP_PCT, %s
                          SELECT areasymbol, musym, muname, #kitchensink.mukey, #last_step.cokey, %s
                            INTO #last_step2
                            FROM #last_step
                            RIGHT OUTER JOIN #kitchensink ON #kitchensink.mukey = #last_step.mukey
                            GROUP BY #kitchensink.areasymbol, #kitchensink.musym, #kitchensink.muname, #kitchensink.mukey, %s, #last_step.cokey
                            ORDER BY #kitchensink.areasymbol, #kitchensink.musym, #kitchensink.muname, #kitchensink.mukey
                            SELECT #last_step2.areasymbol, #last_step2.musym, #last_step2.muname, #last_step2.mukey, %s
                              FROM #last_step2
                              LEFT OUTER JOIN #last_step ON #last_step.mukey = #last_step2.mukey
                                  GROUP BY #last_step2.areasymbol, #last_step2.musym, #last_step2.muname, #last_step2.mukey, %s
                                  ORDER BY #last_step2.areasymbol, #last_step2.musym, #last_step2.muname, #last_step2.mukey, %s",
paste0(sprintf("thickness_wt_%s, sum_thickness_%s", property, property), collapse = ", "),
paste0(property, collapse = ", "),
paste0(sprintf("SUM((thickness_wt_%s / (CASE WHEN sum_thickness_%s = 0 THEN 1 ELSE sum_thickness_%s END)) * %s) OVER (PARTITION BY #main.cokey) AS DEPTH_WEIGHTED_AVERAGE%s",
               property, property, property, property, n), collapse = ", "),
paste0(sprintf("WEIGHTED_COMP_PCT * DEPTH_WEIGHTED_AVERAGE%s AS COMP_WEIGHTED_AVERAGE%s", n, n), collapse = ", "),
paste0(sprintf("DEPTH_WEIGHTED_AVERAGE%s", n), collapse = ", "),
paste0(sprintf("CAST (SUM (COMP_WEIGHTED_AVERAGE%s) OVER (PARTITION BY #kitchensink.mukey) AS decimal(5,2)) AS %s",
               n, property), collapse= ", "),
paste0(sprintf("COMP_WEIGHTED_AVERAGE%s", n), collapse = ", "),
paste0(sprintf("#last_step2.%s", property), collapse = ", "),
paste0(sprintf("#last_step2.%s", property), collapse = ", "),
paste0(sprintf("#last_step2.%s", property), collapse = ", ")))
  }

  .property_dominant_component_numeric <- function(property, top_depth, bottom_depth, where_clause) {
    # dominant component numeric is a more specific case of weighted average
    .property_weighted_average(property, top_depth, bottom_depth, where_clause, dominant = TRUE, include_minors = FALSE, miscellaneous_areas = FALSE)
  }

  # create query based on method
  switch(toupper(agg_method$method),
    # dominant component (category)
    "DOMINANT COMPONENT (CATEGORY)" =
    sprintf("SELECT areasymbol, musym, muname, mu.mukey AS mukey, %s
             FROM legend AS l
             INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
             INNER JOIN component AS c ON c.mukey = mu.mukey AND
                                                    c.cokey = (SELECT TOP 1 c1.cokey FROM component AS c1
                                                              INNER JOIN mapunit ON c.mukey=mapunit.mukey AND c1.mukey=mu.mukey ORDER BY c1.comppct_r DESC, c1.cokey)",

            paste0(sapply(agg_property, function(x) sprintf("%s AS %s", x, x)), collapse = ", "),
            where_clause),

    # weighted average (.weighted_average handles vector agg_property)
    "WEIGHTED AVERAGE" = .property_weighted_average(agg_property, top_depth, bottom_depth, where_clause, include_minors = include_minors, miscellaneous_areas = miscellaneous_areas),

    "MIN/MAX" =
      sprintf("SELECT areasymbol, musym, muname, mu.mukey AS mukey, %s
               INTO #funagg
                    FROM legend AS l
                    INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
                    LEFT JOIN component AS c ON c.mukey = mu.mukey %s %s
               SELECT areasymbol, musym, muname, mukey, %s FROM #funagg
               GROUP BY areasymbol, musym, muname, mukey",
              paste0(sapply(agg_property, function(x) .property_min_max(x, top_depth, bottom_depth, FUN = FUN)), collapse = ", "),
              where_clause,
              ifelse(include_minors, ""," AND c.majcompflag = 'Yes'"),
              ifelse(miscellaneous_areas, ""," AND c.compkind != 'Miscellaneous area'"),
              paste0(paste0(FUN, "(", agg_property, ") AS ", agg_property), collapse=",")),

    # dominant component (numeric) (.dominant_component_numeric handles vector agg_property)
    "DOMINANT COMPONENT (NUMERIC)" = .property_dominant_component_numeric(agg_property, top_depth, bottom_depth, where_clause),

    # dominant condition
    "DOMINANT CONDITION" =
    sprintf("SELECT areasymbol, musym, muname, mu.mukey/1 AS mukey, %s
             FROM legend  AS l
              INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
              INNER JOIN component AS c ON c.mukey = mu.mukey AND
                                           c.cokey = (SELECT TOP 1 c1.cokey FROM component AS c1
                                                      INNER JOIN mapunit ON c.mukey = mapunit.mukey AND
                                                                            c1.mukey = mu.mukey
                                                      ORDER BY c1.comppct_r DESC, c1.cokey)
              GROUP BY areasymbol, musym, muname, mu.mukey, c.cokey,  compname, comppct_r
              ORDER BY areasymbol, musym, muname, mu.mukey, comppct_r DESC, c.cokey",
            paste0(sapply(agg_property, .property_dominant_condition_category), collapse = ", "),
            where_clause),

    # NO AGGREGATION (component properties)
  "NONE" = sprintf("SELECT areasymbol, musym, muname, mu.mukey/1 AS mukey,
                           c.compname AS compname, c.comppct_r AS comppct_r, c.cokey AS cokey,
                           %s
             FROM legend AS l
              INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
              INNER JOIN component AS c ON c.mukey = mu.mukey
              ORDER BY areasymbol, musym, muname, mu.mukey, c.comppct_r DESC, c.cokey",
            paste0(sapply(agg_property, function(x) sprintf("c.%s AS %s", x, x)), collapse = ", "),
            where_clause),

  # NO AGGREGATION (horizon properties)
  "NONE_HORIZON" = sprintf("SELECT areasymbol, musym, muname, mu.mukey/1 AS mukey,
                                   c.cokey AS cokey, ch.chkey AS chkey,
                                   c.compname AS compname, c.comppct_r AS comppct_r,
                                   ch.hzdept_r AS hzdept_r, ch.hzdepb_r AS hzdepb_r,
                                   %s
             FROM legend  AS l
              INNER JOIN mapunit AS mu ON mu.lkey = l.lkey AND %s
              INNER JOIN component AS c ON c.mukey = mu.mukey
              INNER JOIN chorizon AS ch ON ch.cokey = c.cokey
              ORDER BY areasymbol, musym, muname, mu.mukey, c.comppct_r DESC, c.cokey, hzdept_r",
             paste0(sapply(agg_property, function(x) sprintf("ch.%s AS %s", x, x)), collapse = ", "),
             where_clause)
  )

}

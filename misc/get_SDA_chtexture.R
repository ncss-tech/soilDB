#' Title
#'
#' @param method 
#' @param mukeys 
#' @param dsn 
#'
#' @returns
#' @export
#'
#' @examples
#' 
#' get_SDA_chtexture(method = "none", 1906473)
#' 
#' get_SDA_chtexture(method = "dominant component", 1906473)
#' get_SDA_chtexture(method = "dominant component (surface)", 1906473)
#' get_SDA_chtexture(method = "dominant component (surface w/o duff)", 1906473)
#' 
#' get_SDA_chtexture(method = "dominant condition", 1906473)
#' get_SDA_chtexture(method = "dominant condition (surface)", 1906473)
#' get_SDA_chtexture(method = "dominant condition (surface w/o duff)", 1906473)
get_SDA_chtexture <- function(method, mukeys, dsn = NULL) {
  filter_logic <- switch(tolower(trimws(method)),
                   "none" = "1=1",
                   "dominant component" = "cn = 1",
                   "dominant component (surface)" = "cn = 1 AND hn = 1",
                   "dominant component (surface w/o duff)" = "cn = 1 AND hn = 1 AND horztype <> 'dry organic'",
                   "dominant condition" = stop("method='dominant condition' not yet implemented"),
                   "dominant condition (surface)" = stop("method='dominant condition' not yet implemented"),
                   "dominant condition (surface w/o duff)" = stop("method='dominant condition' not yet implemented"),)
  q <- sprintf(
    "With RankedTextures AS (
      SELECT areasymbol, mapunit.mukey, musym, muname, nationalmusym, 
            compname, localphase, comppct_r, 
            hzname, horztype, hzdept_r, hzdepb_r,
            texture, stratextsflag, rvindicator, texdesc,
            DENSE_RANK() OVER (PARTITION BY mapunit.mukey ORDER BY comppct_r DESC) AS cn,
            ROW_NUMBER() OVER (PARTITION BY component.cokey ORDER BY comppct_r DESC, hzdept_r) AS hn
        FROM legend
        INNER JOIN mapunit ON legend.lkey = mapunit.lkey AND mapunit.mukey IN %s
        INNER JOIN component ON mapunit.mukey = component.mukey AND majcompflag = 'Yes'
        LEFT JOIN chorizon ON component.cokey = chorizon.cokey
        LEFT JOIN chtexturegrp ON chorizon.chkey = chtexturegrp.chkey AND rvindicator = 'Yes'
    )
    SELECT * FROM RankedTextures WHERE %s",
    soilDB::format_SQL_in_statement(mukeys),
    filter_logic
  )
  soilDB::SDA_query(q, dsn = dsn)
}


# utilities for manipulating JSON results from SDA

# q <- "SELECT DISTINCT component.mukey
#         FROM cosurffrags
#       INNER JOIN component ON component.cokey = cosurffrags.cokey
#       INNER JOIN mapunit ON mapunit.mukey = component.mukey
#       INNER JOIN legend ON legend.lkey = mapunit.lkey
#       WHERE mlraoffice = 'Davis, CA' AND distrocks_r IS NOT NULL"
# res <- soilDB:::.SDA_query_FOR_JSON_AUTO(q)
# q2 <- sprintf("SELECT DISTINCT areasymbol, muname FROM mapunit 
#        INNER JOIN legend ON legend.lkey = mapunit.lkey
#        WHERE mapunit.mukey IN %s", format_SQL_in_statement(res$mukey))
# res2 <- soilDB:::.SDA_query_FOR_JSON_AUTO(q2)
# 
# res2[which(grepl("CA", res2$areasymbol)),2]
# 
.SDA_query_FOR_JSON_AUTO <- function(queries, convert = TRUE, query_string = FALSE) {
  res <- lapply(queries, function(x) {
    q <- sprintf("~DeclareVarchar(@json,max)~
    ;WITH src (n) AS (%s FOR JSON AUTO)
    SELECT @json = src.n
    FROM src
    SELECT @json, LEN(@json);", x)
    if (query_string) return(q)
    res <- SDA_query(q)$V1
    if (convert) return(jsonlite::fromJSON(res))
    res
  })
  if (length(queries) == 1)
    return(res[[1]])
  res
}


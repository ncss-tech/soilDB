#' Get Soil Data Access, Lab Data Mart and Web Soil Survey Usage Metrics
#'
#' Obtain pre-calculated tabular reports of usage, activities, areas of interest (AOI), exports, ecological sites, ratings and reports for specific areas, times and intervals.
#'
#' @param query_name One or more of: `'LDM_Usage'`, `'SDA_Usage'`, `'wss_ActivityCounts'`, `'wss_AOIDefinition'`, `'wss_AOISizeRange'`, `'wss_ExportCounts'`, `'wss_PrintableOutput'`, `'wss_top100AOIs'`, `'wss_top100Ecologicalsites'`, `'wss_top100ratings'`, `'wss_top100reports'`
#' @param query_frequency One or more of: `'M'`, `'CY'`, `'FY'`
#' @param query_year Integer. One or more years e.g. `2020:2021`
#' @param state Optional: State abbreviation; Default: `NULL` uses `"xnational"` for all states.
#'
#' @return A `data.frame` containing query results
#' @export
#' @author Jason Nemecek
#'
#' @examples
#' \dontrun{
#' get_SDA_metrics('SDA_Usage', 'CY', 2019:2021)
#' }
get_SDA_metrics <- function(query_name, query_frequency, query_year, state = NULL) {

  query_name <- match.arg(query_name, c('LDM_Usage', 'SDA_Usage', 'wss_ActivityCounts',
                                        'wss_AOIDefinition', 'wss_AOISizeRange', 'wss_ExportCounts',
                                        'wss_PrintableOutput', 'wss_top100AOIs', 'wss_top100Ecologicalsites',
                                        'wss_top100ratings', 'wss_top100reports'))

  query_frequency <- match.arg(query_frequency, c('M', 'CY', 'FY'))

  query_year <- as.integer(query_year)

  if (!is.integer(query_year)) {
    stop("Query year should be coercible to integer")
  }

  if (is.null(state)) {
    state <- 'xnational'
  }

  q <- sprintf("SELECT [id]
    ,[query_id]
    ,[query_name]
    ,[query_frequency]
    ,[query_month]
    ,[query_year]
    ,[state]
    ,[seqnum]
    ,[query_title]
    ,[query_header]
    ,[query_header2]
    ,[count]
    FROM
    [wss_metric_query_results]
    WHERE
    [query_name] IN %s
    AND [query_frequency] IN %s
    AND [query_year] IN %s
    AND [state] IN %s
    ",
    format_SQL_in_statement(query_name),
    format_SQL_in_statement(query_frequency),
    format_SQL_in_statement(query_year),
    format_SQL_in_statement(state))
  SDA_query(q)
}

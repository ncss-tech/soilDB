get_cosoilmoist_from_NASIS <- function(impute = TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if (!requireNamespace('RODBC')) stop('please install the `RODBC` package', call.=FALSE)

  q.cosoilmoist <- "SELECT dmuiidref AS dmuiid, coiid, compname, comppct_r, drainagecl, month, flodfreqcl, floddurcl, pondfreqcl, ponddurcl, cosoilmoistiid, soimoistdept_l, soimoistdept_r, soimoistdept_h, soimoistdepb_l, soimoistdepb_r, soimoistdepb_h, soimoiststat

  FROM component_View_1 co LEFT OUTER JOIN
       comonth_View_1 com ON com.coiidref = co.coiid LEFT OUTER JOIN
       cosoilmoist_View_1 cosm ON cosm.comonthiidref = com.comonthiid

  ORDER BY dmuiid, comppct_r DESC, compname, month, soimoistdept_r
  ;"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # exec query
  d.cosoilmoist <- RODBC::sqlQuery(channel, q.cosoilmoist, stringsAsFactors = FALSE)

  # close connection
  RODBC::odbcClose(channel)

  # recode metadata domains
  d.cosoilmoist <- uncode(d.cosoilmoist, stringsAsFactors = stringsAsFactors)

  # prep dataset: rename columns, impute empty values, stringsAsFactors
  d.cosoilmoist <- .cosoilmoist_prep(d.cosoilmoist, impute = impute, stringsAsFactors = stringsAsFactors)

  # done
  return(d.cosoilmoist)
}

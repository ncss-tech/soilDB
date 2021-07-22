# TODO: sitebedrock _may_ contain more than 1 row / site... this will result in duplicate rows returned by this function.

## Notes:
# source of the plantassocnm column changed - now resides in the siteobs table
# siteiidref key removed from pedon table - use the pedon.siteobsiidref through the siteobs table (siteobs.siteobsiid) as the new linkage	




#' Get Site Data from a PedonPC Database
#' 
#' Get site-level data from a PedonPC database.
#' 
#' 
#' @param dsn The path to a 'pedon.mdb' database.
#' @return A data.frame.
#' @author Dylan E. Beaudette and Jay M. Skovlin
#' @seealso \code{\link{get_hz_data_from_pedon_db}},
#' \code{\link{get_veg_from_AK_Site}},
#' @keywords manip
#' @export get_site_data_from_pedon_db
get_site_data_from_pedon_db <- function(dsn) {
  # must have odbc installed
  if(!requireNamespace('odbc'))
    stop('please install the `odbc` package', call.=FALSE)
  
  q <- "SELECT 
          site.siteiid,
          pedon.peiid,
          upedonid as pedon_id,
          site.usiteid as site_id,
          siteobs.obsdate as obs_date,
          latdegrees + IIF(IsNull(latminutes), 0.0, latminutes / 60.0) + IIF(IsNULL(latseconds), 0.0, latseconds / 60.0 / 60.0) as y,
          -(longdegrees + IIF(IsNull(longminutes), 0.0, longminutes / 60.0) + IIF(IsNull(longseconds), 0.0, longseconds / 60.0 / 60.0)) as x,
          horizdatnm,
          longstddecimaldegrees as x_std,
          latstddecimaldegrees as y_std,
          descname as describer,
          pedonpurpose,
          pedontype,
          pedlabsampnum,
          psctopdepth,
          pscbotdepth,
          elev as elev_field,
          slope as slope_field,
          aspect as aspect_field,
          plantassocnm,
          siteobs.earthcovkind1,
          bedrckdepth,
          bedrckkind,
          bedrckhardness,
          hillslopeprof,
          geomslopeseg,
          shapeacross,
          shapedown,
          slopecomplex,
          drainagecl
        FROM 
            (((site INNER JOIN siteobs ON site.siteiid = siteobs.siteiidref)
            LEFT OUTER JOIN pedon ON siteobs.siteobsiid = pedon.siteobsiidref)
            LEFT OUTER JOIN sitebedrock ON site.siteiid = sitebedrock.siteiidref)
            ORDER BY site.usiteid"

  # setup connection to our pedon database
  channel <- DBI::dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))
  
  # exec query
  d <- DBI::dbGetQuery(channel, q)
  
  # close connection
  DBI::dbDisconnect(channel)

  # uncode domained columns
  d <- uncode(d)	
  
  # warn if mixed datums
  unique.datums <- unique(na.omit(d$horizdatnm))
  if(length(unique.datums) > 1)
    message(paste('multiple datums present:', paste(unique.datums, collapse=', ')))
  
  # are there any dupes?
  t.pedon_id <- table(d$pedon_id)
  not.unique.pedon_id <- t.pedon_id > 1
  if(any(not.unique.pedon_id)) {
  	assign('dup.pedon.ids', value=names(t.pedon_id[which(not.unique.pedon_id)]), envir=soilDB.env)
  	message("NOTICE: duplicate pedons: use `get('dup.pedon.ids', envir=soilDB.env)` for a list of pedon IDs")
  }
  
  # warn about sites without a matching pedon (records missing peiid)
  missing.pedon <- which(is.na(d$peiid))
  if(length(missing.pedon)> 0) {
  	assign('sites.missing.pedons', value=unique(d$site_id[missing.pedon]), envir=soilDB.env)
  	message("NOTICE: sites without pedons: use `get('sites.missing.pedons', envir=soilDB.env)` for a list of site IDs")
  }
  
  # done
  return(d)
  }


## TODO: leap years? 365 vs 366 total days

# compute water year and day
# d: anything the can be safely converted it POSIXlt
# end: MM-DD notation for end of water year


#' Compute Water Day and Year
#'
#' Compute "water" day and year, based on the end of the typical or legal dry
#' season. This is September 30 in California.
#'
#' This function doesn't know about leap-years. Probably worth checking.
#'
#' @param d anything the can be safely converted to \code{POSIXlt}
#' @param end "MM-DD" notation for end of water year
#' @param format Used in POSIXlt conversion. Default `"%Y-%m-%d"`
#' @param tz Used in POSIXlt conversion for custom timezone. Default is `"UTC"`
#' @return A \code{data.frame} object with the following \item{wy}{the "water
#' year"} \item{wd}{the "water day"}
#' @author D.E. Beaudette
#' @references Ideas borrowed from:
#' \url{https://github.com/USGS-R/dataRetrieval/issues/246} and
#' \url{https://stackoverflow.com/questions/48123049/create-day-index-based-on-water-year}
#' @keywords manip
#' @examples
#'
#' # try it
#' waterDayYear('2019-01-01')
#'
#' @export waterDayYear
waterDayYear <- function(d, end = "09-30", format = "%Y-%m-%d", tz = "UTC") {

  # create a date+timezone (and possibly time) from input
  dLT <- as.POSIXlt(as.Date(d, format = format), tz = tz)

  # add century from epoch
  water_year <- dLT$year + 1900

  # extract julian day the new water year starts
  startDay <- as.POSIXlt(as.Date(end[1], format = "%m-%d") + 1, tz = "UTC")$yday

  # water year starts day after `end` of _prior_ year,
  # so increment calendar year for days after `end`
  wyidx <- which(dLT$yday >= startDay)
  water_year[wyidx] <- water_year[wyidx] + 1

  # date of the last day of the actual water year
  lastWY <- as.POSIXlt(as.Date(sprintf("%s-%s", water_year - 1, end), format = "%Y-%m-%d"), tz = tz)

  # compute water "day" as days since start of water year
  # NOTE: round()ing b/c as.integer(difftime(..., units='days')) is like floor()
  water_day <- as.integer(round(difftime(dLT, lastWY, units = 'days')))

  res <- data.frame(
    wy = water_year,
    wd = water_day
  )

  return(res)
}


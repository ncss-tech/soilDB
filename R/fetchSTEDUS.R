#' Fetch Standardized Elevation Derivatives of the United States (STEDUS) Grids
#'
#' This tool creates a virtual raster or downloads data for an extent from Cloud
#' Optimized GeoTIFFs (COGs) from the Standardized Elevation Derivatives of the
#' United States (STEDUS) project.
#'
#' @param x  An R spatial object (such as a _SpatVector_, _SpatRaster_, or _sf_
#'   object) or a _SoilProfileCollection_ with coordinates initialized via
#'   `aqp::initSpatial<-`. Default: `NULL` returns the CONUS extent as virtual
#'   raster. If `x` is a _SpatRaster_ the coordinate reference system, extent,
#'   and resolution are used as a template for the output raster.
#' @param variables One or more variables corresponding to STEDUS grid file
#'   names (without .tif extension). See
#'   \url{https://storage.googleapis.com/dsm-ft-stedus-30m-public/index.html}
#'   for details.
#' @param filename character. Path to write output raster file. Default: `NULL`
#'   will keep result in memory (or store in temporary file if memory threshold
#'   is exceeded)
#' @param overwrite Overwrite `filename` if it exists? Default: `FALSE`
#'
#' @references Brungard et al. (2024) 30 meter Standardized Elevation
#'   Derivatives of the United States (STEDUS30). Manuscript in preparation.
#'   Available online:
#'   https://storage.googleapis.com/dsm-ft-stedus-30m-public/index.html
#' 
#' @return A _SpatRaster_ object.
#' @noRd
#'
#' @examplesIf curl::has_internet() && requireNamespace("sf") && !inherits(try(requireNamespace("terra", quietly = TRUE), silent = TRUE), 'try-error')
#' \dontrun{
#' b <- c(-119.747629, -119.67935, 36.912019, 36.944987)
#' 
#' bbox.sp <- sf::st_as_sf(wk::rct(
#'   xmin = b[1], xmax = b[2], ymin = b[3], ymax = b[4],
#'   crs = sf::st_crs(4326)
#' ))
#' 
#' ssurgo.geom <- soilDB::SDA_spatialQuery(
#'   bbox.sp,
#'   what = 'mupolygon',
#'   db = 'SSURGO',
#'   geomIntersection = TRUE
#' )
#' 
#' # grid output
#' res <- fetchSTEDUS(
#'   ssurgo.geom,
#'   variables = c("sl_2", "tri_8", "aspct_2", "mca_10000")
#' )
#' 
#' res
#' 
#' plot(res) 
#' }
fetchSTEDUS <- function(x = NULL, variables = NULL, filename = NULL, overwrite = FALSE) {
  # ind <- try(.get_STEDUS_index())
  
  # if (inherits(ind, 'try-error'))
  #   stop("Failed to retrieve STEDUS index", call. = FALSE)

  # subset based on user specified properties, depths, and product type
  # if (!is.null(variables)) {
  #   isub <- ind[ind$property %in% variables, ]
  # } else isub <- ind
  # if (!requireNamespace("terra")) {
  #   stop("package 'terra' is required", call. = FALSE)
  # }
  urls <- paste0("https://storage.googleapis.com/dsm-ft-stedus-30m-public/", variables, ".tif")
  
  # create virtual raster from list of URLs
  r <- terra::rast(
    paste0("/vsicurl/", urls)#isub$url)  
  )
  
  .process_raster_extent(r, x, filename = filename, overwrite = overwrite)
}

.get_STEDUS_index <- function() {
  
  # TODO: parse XML directly instead of HTML?
  if (!requireNamespace("rvest")) {
    stop("package 'rvest' is required", call. = FALSE)  
  }
  
  # read index as HTML table
  res <- rvest::html_table(rvest::read_html("https://storage.googleapis.com/dsm-ft-stedus-30m-public/index.html"), header = FALSE)[[1]]
  
  # column names are in 4th row
  colnames(res) <- res[5, ]
  
  # drop empty rows
  res <- res[-(c(1:5, nrow(res))), ]
  
  # cleanup
  res$filename <- gsub("\r\n *", "", res$filename)
  res$description <- gsub("\r\n *", "", res$description)
  res$notes <- gsub("\r\n *", "", res$notes)
  
  res$property <- gsub("\\.tif$", "", res$filename)
  res
}

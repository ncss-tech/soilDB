#' Fetch Soil Landscapes of the United States (SOLUS) Grids
#'
#' This tool creates a virtual raster from Cloud Optimized GeoTIFFs (COGs) from the [Soil
#' Landscapes of the United States 100-meter (SOLUS100) soil property maps project repository](https://agdatacommons.nal.usda.gov/articles/dataset/Data_from_Soil_Landscapes_of_the_United_States_100-meter_SOLUS100_soil_property_maps_project_repository/25033856).
#' 
#' @details
#'  
#' If the input object `x` is not specified (`NULL` or missing), a _SpatRaster_ object using the virtual URLs is returned. The full data
#' set can be then downloaded and written to file using `terra::writeRaster()` or any other processing step specifying an output file name.
#' When input object `x` is specified, a _SpatRaster_ object using in memory or local (temporary file or `filename`) resources is returned 
#' after downloading the data only for the target extent.
#' 
#' @param x  An R spatial object (such as a _SpatVector_, _SpatRaster_, or _sf_ object) or a 
#'  _SoilProfileCollection_ with coordinates initialized via `aqp::initSpatial<-`. Default: `NULL` returns 
#'  the full extent as a virtual raster. Note that this is nearly 30GB compressed
#' @param depth_slices character. One or more of: `"all"`, `"0"`, `"5"`, `"15"`,
#'  `"30"`, `"60"`, `"100"`, `"150"`
#' @param variables character. One or more of: `"anylithicdpt"`, `"caco3"`, `"cec7"`, `"claytotal"`, 
#'  `"dbovendry"`, `"ec"`, `"ecec"`, `"fragvol"`, `"gypsum"`, `"ph1to1h2o"`, `"resdept"`, `"sandco"`,
#'  `"sandfine"`, `"sandmed"`, `"sandtotal"`, `"sandvc"`, `"sandvf"`, `"sar"`, `"silttotal"`, `"soc"`.
#' @param filetype character. One or more of: `"prediction"`, `"relative prediction interval"`, 
#'  `"95% low prediction interval"`, `"95% high prediction interval"`
#' @param grid logical. Not used. Currently default `TRUE` always returns a _SpatRaster_ object for an extent.
#' @param filename character. Path to write output raster file. Default: `NULL` will keep result in
#'  memory (or store in temporary file if memory threshold is exceeded)
#' @param overwrite Overwrite `filename` if it exists? Default: `FALSE`
#'
#' @return A _SpatRaster_ object containing SOLUS grids for specified extent, depths, variables, and product types.
#' 
#' @references Nauman, T.W., Kienast-Brown, S., White, D.A. Brungard, C.W., Philippe, J., Roecker, S.M., 
#'  Thompson, J.A. Soil Landscapes of the United States (SOLUS): developing predictive soil property maps
#'  of the conterminous US using hybrid training sets. In Prep for SSSAJ.
#' 
#' @author Andrew G. Brown
#' 
#' @export
#'
#' @examplesIf curl::has_internet() && requireNamespace("sf") && requireNamespace("terra")
#' 
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
#' res <- fetchSOLUS(
#'   ssurgo.geom,
#'   depth_slices = "0",
#'   variables = c("sandtotal", "silttotal", "claytotal", "cec7"),
#'   filetype = "prediction"
#' )
#' 
#' terra::plot(res)
fetchSOLUS <- function(x = NULL, 
                       depth_slices = c("all", "0", "5", "15", "30",
                                           "60", "100", "150"), 
                       variables = c("anylithicdpt", "caco3", "cec7", "claytotal",
                                     "dbovendry",  "ec", "ecec", "fragvol", "gypsum",
                                     "ph1to1h2o", "resdept", "sandco", "sandfine", 
                                     "sandmed", "sandtotal", "sandvc", "sandvf", 
                                     "sar", "silttotal", "soc"),
                       filetype = c("prediction",
                                    "relative prediction interval",
                                    "95% low prediction interval", 
                                    "95% high prediction interval"),
                       grid = TRUE,
                       filename = NULL,
                       overwrite = FALSE
                       ) {
  
  # get index of SOLUS COGs
  ind <- .get_SOLUS_index()
  
  # subset based on user specified properties, depths, and product type
  isub <- ind[ind$property %in% variables & 
                ind$depth_slice %in% depth_slices &
                ind$filetype %in% filetype,]
  
  isub$subproperty <- gsub("\\.tif$", "", isub$filename)
  isub$scalar <- as.numeric(isub$scalar)
  
  # create virtual raster from list of URLs
  r <- terra::rast(
    paste0("/vsicurl/", isub$url)
  )
  
  # do conversion of input spatial object 
  if (!missing(x) && !is.null(x)) {
    
    # convert various input types to SpatVector
    if (inherits(x, 'SoilProfileCollection')) {
      x <- as(x, 'sf')
    }
    
    if (inherits(x, 'SpatRaster')) {
      x <- terra::as.polygons(x, extent = TRUE)
    }
    
    if (!inherits(x, 'SpatVector')) {
      x <- terra::vect(x)
    }
    
    # project input object to CRS of SOLUS
    x <- terra::project(x, terra::crs(r))
    
    # crop to target extent (written to temp file if needed)
    r <- terra::crop(r, x)
    
    # apply scaling factors (only when needed)
    if (any(isub$scalar != 1)) {
      r <- terra::rast(lapply(seq_len(nrow(isub)), function(i) {
        r[isub$subproperty[i]] / isub$scalar[i] 
      }))
    }
    
    # write to final output file (if filename specified)
    if (!is.null(filename)) {
      r <- writeRaster(r, x, filename = filename)
    }
  } else {
    if (any(isub$scalar != 1)) {
      warning("NOTE: virtual reference to remote sources returned; no scaling factors have been applied!", call. = FALSE)
    }
  }
  
  r
}

.get_SOLUS_index <- function() {
  
  # TODO: parse XML directly instead of HTML?
  
  # read index as HTML table
  res <- rvest::html_table(rvest::read_html("https://storage.googleapis.com/solus100pub/index.html"), header = FALSE)[[1]]
  
  # column names are in 4th row
  colnames(res) <- res[5,]
  
  # drop empty rows
  res <- res[-(c(1:5, nrow(res))), ]
  
  # fix inconsistencies in depth column
  res$depth[is.na(res$depth) | res$depth == ""] <- "all_cm"
  dlut <- c("all_cm" = "all", 
            "0_cm" = "0", "5_cm" = "5", "15" = "15",
            "30_cm" = "30", "60_cm" = "60", "100_cm" = "100",  
            "150_cm" = "150")
  
  # use depth slices
  res$depth_slice <- dlut[res$depth]
  res$depth_slice <- factor(res$depth_slice, levels = unique(dlut))
  
  res
}



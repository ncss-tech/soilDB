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
#' @param depth_slices character. One or more of: `"0"`, `"5"`, `"15"`,
#'  `"30"`, `"60"`, `"100"`, `"150"`. The "depth slice" `"all"` (used for variables such as 
#'  `"anylithicdpt"`, and `"resdept"`) is always included if any site-level variables are selected. 
#' @param variables character. One or more of: `"anylithicdpt"`, `"caco3"`, `"cec7"`, `"claytotal"`, 
#'  `"dbovendry"`, `"ec"`, `"ecec"`, `"fragvol"`, `"gypsum"`, `"ph1to1h2o"`, `"resdept"`, `"sandco"`,
#'  `"sandfine"`, `"sandmed"`, `"sandtotal"`, `"sandvc"`, `"sandvf"`, `"sar"`, `"silttotal"`, `"soc"`.
#' @param output_type character. One or more of: `"prediction"`, `"relative prediction interval"`, 
#'  `"95% low prediction interval"`, `"95% high prediction interval"`
#' @param grid logical. Default `TRUE` returns a _SpatRaster_ object for an extent. `FALSE` returns a _SoilProfileCollection_. 
#'                      Any other value returns a _list_ object with names `"grid"` and `"spc"` containing both result objects.
#' @param samples integer. Number of regular samples to return when `grid=FALSE`. Default `NULL` will convert all grid cells 
#'                         to a unique profile. Note that for a large extent,  this can produce large _SoilProfileCollection_
#'                         objects with a very large number of layers (especially with `method` other than `"step"`).
#' @param method character. Used when `grid=FALSE` to determine depth interpolation method. Default: `"linear"`.
#'               Options include any `method` allowed for `approxfun()` or `splinefun()` pluse `"step"`. 
#'               `"step"` uses the prediction depths as the top of each interval and returns a number of layers equal
#'               to length of `depth_slices`. Methods other than "step" return data in interpolated 1cm slices.
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
#' @importFrom stats approxfun splinefun
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
#' # grid output
#' res <- fetchSOLUS(
#'   ssurgo.geom,
#'   depth_slices = "0",
#'   variables = c("sandtotal", "silttotal", "claytotal", "cec7"),
#'   output_type = "prediction"
#' )
#' 
#' terra::plot(res)
#' 
#' # SoilProfileCollection output, using linear interpolation for 1cm slices
#' # site-level variables (e.g. resdept) added to site data.frame of SPC
#' res <- fetchSOLUS(
#'   ssurgo.geom,
#'   depth_slices = c("0", "5", "15", "30", "60", "100", "150"),
#'   variables = c("sandtotal", "silttotal", "claytotal", "cec7", "resdept"),
#'   output_type = "prediction",
#'   method = "linear",
#'   grid = FALSE,
#'   samples = 10
#' )
#' 
#' # plot, truncating each profile to the predicted restriction depth
#' aqp::plotSPC(trunc(res, 0, res$resdept_p), color = "claytotal_p", divide.hz = FALSE)
fetchSOLUS <- function(x = NULL, 
                       depth_slices = c("0", "5", "15", "30", "60", "100", "150"), 
                       variables = c("anylithicdpt", "caco3", "cec7", "claytotal",
                                     "dbovendry",  "ec", "ecec", "fragvol", "gypsum",
                                     "ph1to1h2o", "resdept", "sandco", "sandfine", 
                                     "sandmed", "sandtotal", "sandvc", "sandvf", 
                                     "sar", "silttotal", "soc"),
                       output_type = c("prediction",
                                       "relative prediction interval",
                                       "95% low prediction interval", 
                                       "95% high prediction interval"),
                       grid = TRUE,
                       samples = NULL,
                       method = c("linear", "constant", "fmm", "natural", "monoH.FC", "step"),
                       filename = NULL,
                       overwrite = FALSE
                       ) {
  
  # Not all spline methods are relevant, but they can be allowed to work
  # method <- match.arg(method, c("linear", "constant", "fmm", "periodic", "natural", "monoH.FC", "hyman", "step"))
  
  # get index of SOLUS COGs
  ind <- .get_SOLUS_index()
  
  # subset based on user specified properties, depths, and product type
  isub <- ind[ind$property %in% variables & 
                ind$depth_slice %in% c("all", depth_slices) &
                ind$filetype %in% output_type,]
  
  isub$subproperty <- gsub("\\.tif$", "", isub$filename)
  isub$scalar <- as.numeric(isub$scalar)
  
  if (!requireNamespace("terra")) {
    stop("package 'terra' is required", call. = FALSE)
  }
  
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
      r <- terra::writeRaster(r, x, filename = filename)
    }
  } else {
    if (any(isub$scalar != 1)) {
      warning("NOTE: virtual reference to remote sources returned; no scaling factors have been applied!", call. = FALSE)
    }
  }
  
  if (isTRUE(grid)) {
    return(r)
  } 
  
  if (length(depth_slices) == 1 && method != "step") {
    stop("Cannot interpolate for SoilProfileCollection output with only one depth slice! Change `method` to \"step\" or add another `depth_slice`.", call. = FALSE)
  }
    
  if (!missing(x) && !is.null(x) && terra::is.points(x)) {
    dat <- terra::extract(r, x)
  } else {
    if (!missing(samples) && !is.null(samples)) {
      dat <- terra::spatSample(r,
                               size = samples,
                               method = "regular",
                               xy = TRUE) # for testing
    } else {
      dat <- terra::as.data.frame(r, xy = TRUE)
    }
  }
  
  dat$ID <- seq(nrow(dat))
    
  spc <- .convert_SOLUS_dataframe_to_SPC(dat, idname = "ID", method = method)
  initSpatial(spc, terra::crs(r)) <- ~ x + y
  
  if (isFALSE(grid)) {
    return(spc)
  } else {
    return(list(grid = r, spc = spc))
  }
}

.get_SOLUS_index <- function() {
  
  # TODO: parse XML directly instead of HTML?
  
  # read index as HTML table
  res <- rvest::html_table(rvest::read_html("https://storage.googleapis.com/solus100pub/index.html"), header = FALSE)[[1]]
  
  # column names are in 4th row
  colnames(res) <- res[5, ]
  
  # drop empty rows
  res <- res[-(c(1:5, nrow(res))), ]
  
  # fix inconsistencies in depth column
  res$depth[is.na(res$depth) | res$depth == ""] <- "all_cm"
  dlut <- c("all_cm" = "all", "0_cm" = "0", "5_cm" = "5", "15_cm" = "15", 
            "30_cm" = "30", "60_cm" = "60", "100_cm" = "100", "150_cm" = "150")
  
  # use depth slices
  res$depth_slice <- dlut[res$depth]
  res$depth_slice <- factor(res$depth_slice, levels = unique(dlut))
  
  res
}

.convert_SOLUS_dataframe_to_SPC <- function(x, idname = "id", method) {
  # x: data.frame object with column names corresponding to .get_SOLUS_index() filenames
  # idname: character. column name used to identify profiles
  # method: character. depth interpolation method
  
  # dummy global definitions
  .SD <- NULL
  ID <- NULL
  depth <- NULL
  
  .extractTopDepthFromName <- function(x) {
    gsub("^.*_(\\d+|all)_cm_.*$", "\\1", x)
  }
  
  .replaceTopDepthInName <- function(x) {
    gsub("^(.*)_(\\d+|all)_cm(_.*)$", "\\1\\3", x)
  }
  
  tdep <- .extractTopDepthFromName(colnames(x))
  colnames(x) <- .replaceTopDepthInName(colnames(x))
  
  h <- data.table::rbindlist(lapply(unique(tdep[!tdep %in% c("x", "y", "all", idname)]), function(xx) {
    data.frame(ID = x[[idname]], depth = xx, x[which(tdep == xx)])
  }))
  
  s <- data.frame(ID = x[[idname]], x[tdep %in% c("x", "y", "all")])
  
  h$depth <- as.numeric(h$depth)
  
  h <- h[order(ID, depth),]
  
  stepwise_dept <- c(0, 5, 15, 30, 60, 100, 150)
  stepwise_depb <- c(0, 15, 30, 60, 100, 150, 150)
  names(stepwise_depb) <- stepwise_dept
  
  h$top <- h$depth
  h$bottom <- stepwise_depb[as.character(h$depth)]
  
  ldx <- names(h) %in% c(idname, "depth", "x", "y", "top", "bottom")
  iv <- names(h)[ldx]
  vn <- names(h)[!ldx]
  
  if (method == "step") {
    
    message("consider using method=\"constant\" or method=\"linear\"; SOLUS predictions represent depth slices that do not directly translate to intervals implied by method=\"step\"")
    
    # apply fudge factors for depth slices as property input source
    h$bottom[h$bottom == 0] <- stepwise_dept[2]
    h$bottom[h$top == 150] <- 151
  
    h <- as.data.frame(h)  
  
    depths(h) <- c(idname, "top", "bottom")
    
    site(h) <- s
    
    return(h)
    
  } else if (method %in% c("linear", "constant", "fmm", "periodic", "natural", "monoH.FC", "hyman")) {
    
    mindep <- min(h$top, na.rm = TRUE)
    maxdep <- max(h$bottom, na.rm = TRUE)
    
    if (method %in% c("linear", "constant")) {
      FUN <- approxfun
    } else {
      FUN <- splinefun
    }
    
    h2 <- h[, data.frame(top = mindep:(maxdep - 1),
                         bottom = (mindep + 1):maxdep,
                         lapply(.SD, function(x) {
                           FUN(unique(h$top), x, method = method)((mindep:(maxdep - 1)))
                         })), 
            .SDcols = vn, 
            by = list(ID = h[[idname]])]
    
    h2 <- as.data.frame(h2)
    
    depths(h2) <- c(idname, "top", "bottom")
    
    site(h2) <- s
    
    return(h2)
    
  } else {
    stop("Invalid method argument (\"", method, "\")", call. = FALSE)
  }
}

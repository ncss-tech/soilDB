#' Fetch Soil Landscapes of the United States (SOLUS) Grids
#'
#' This tool creates a virtual raster or downloads data for an extent from Cloud Optimized GeoTIFFs
#' (COGs) from the [Soil Landscapes of the United States 100-meter (SOLUS100) soil property maps
#' project
#' repository](https://agdatacommons.nal.usda.gov/articles/dataset/Data_from_Soil_Landscapes_of_the_United_States_100-meter_SOLUS100_soil_property_maps_project_repository/25033856).
#'
#' @details
#'
#' If the input object `x` is not specified (`NULL` or missing), a _SpatRaster_ object using the
#' virtual URLs is returned. The full extent and resolution data set can be then downloaded and
#' written to file using `terra::writeRaster()` (or any other processing step specifying an output
#' file name). When input object `x` is specified, a _SpatRaster_ object using in memory or local
#' (temporary file or `filename`) resources is returned after downloading the data only for the
#' target extent. In the case where `x` is a _SoilProfileCollection_ or an _sf_ or _SpatVector_
#' object containing point geometries, the result will be a _SoilProfileCollection_ for values
#' extracted at the point locations. To return both the _SpatRaster_ and _SoilProfileCollection_
#' object output in a _list_, use `grid = NULL`.
#'
#' @param x  An R spatial object (such as a _SpatVector_, _SpatRaster_, or _sf_ object) or a
#'   _SoilProfileCollection_ with coordinates initialized via `aqp::initSpatial<-`. Default: `NULL`
#'   returns the CONUS extent as virtual raster. If `x` is a _SpatRaster_ the coordinate reference
#'   system, extent, and resolution are used as a template for the output raster.
#' @param depth_slices character. One or more of: `"0"`, `"5"`, `"15"`, `"30"`, `"60"`, `"100"`,
#'   `"150"`. The "depth slice" `"all"` (used for variables such as `"anylithicdpt"`, and
#'   `"resdept"`) is always included if any site-level variables are selected.
#' @param variables character. One or more of: `"anylithicdpt"`, `"caco3"`, `"cec7"`, `"claytotal"`,
#'   `"dbovendry"`, `"ec"`, `"ecec"`, `"fragvol"`, `"gypsum"`, `"ph1to1h2o"`, `"resdept"`,
#'   `"sandco"`, `"sandfine"`, `"sandmed"`, `"sandtotal"`, `"sandvc"`, `"sandvf"`, `"sar"`,
#'   `"silttotal"`, `"soc"`.
#' @param output_type character. One or more of: `"prediction"`, `"relative prediction interval"`,
#'   `"95% low prediction interval"`, `"95% high prediction interval"`
#' @param grid logical. Default `TRUE` returns a _SpatRaster_ object for an extent. `FALSE` returns
#'   a _SoilProfileCollection_. Any other value returns a _list_ object with names `"grid"` and
#'   `"spc"` containing both result objects.
#' @param samples integer. Number of regular samples to return for _SoilProfileCollection_ output.
#'   Default `NULL` will convert all grid cells to a unique profile. Note that for a large extent,
#'   this can produce large objects with a very large number of layers (especially with `method`
#'   other than `"step"`).
#' @param method character. Used to determine depth interpolation method for _SoilProfileCollection_
#'   output. Default: `"linear"`. Options include any `method` allowed for `approxfun()` or
#'   `splinefun()` plus `"step"` and `"slice"`. `"step"` uses the prediction depths as the top and
#'   bottom of each interval to create a piecewise continuous profile to maximum of 200 cm depth
#'   (for 150 cm upper prediction depth). `"slice"` returns a discontinuous profile with 1 cm thick
#'   slices at the predicted depths. Both `"step"` and `"slice"` return a number of layers equal to
#'   length of `depth_slices`, and all other methods return data in interpolated 1cm slices.
#' @param max_depth integer. Maximum depth to interpolate 150 cm slice data to. Default: `151`.
#'   Interpolation deeper than 151 cm is not possible for methods other than `"step"` and will
#'   result in missing values.
#' @param filename character. Path to write output raster file. Default: `NULL` will keep result in
#'   memory (or store in temporary file if memory threshold is exceeded)
#' @param overwrite Overwrite `filename` if it exists? Default: `FALSE` 
#'
#' @return A _SpatRaster_ object containing SOLUS grids for specified extent, depths, variables, and
#'   product types.
#'
#' @references Nauman, T.W., Kienast-Brown, S., White, D.A. Brungard, C.W., Philippe, J., Roecker,
#'   S.M., Thompson, J.A. Soil Landscapes of the United States (SOLUS): developing predictive soil
#'   property maps of the conterminous US using hybrid training sets. In Prep for SSSAJ.
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
                       depth_slices = c(0, 5, 15, 30, 60, 100, 150), 
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
                       method = c("linear", "constant", "fmm", "natural", "monoH.FC", "step", "slice"),
                       max_depth = 151,
                       filename = NULL,
                       overwrite = FALSE
                       ) {
  
  # Not all spline methods are relevant, but they can be allowed to work
  method <- match.arg(method[1], c("linear", "constant", "fmm", "periodic", "natural", "monoH.FC", "hyman", "step", "slice"))
  
  # get index of SOLUS COGs
  ind <- try(.get_SOLUS_index())
  
  if (inherits(ind, 'try-error')) {
    stop("Failed to fetch SOLUS grid index", call. = FALSE)
  }
  
  # subset based on user specified properties, depths, and product type
  isub <- ind[ind$property %in% variables & 
                as.character(ind$depth_slice) %in% c("all", depth_slices) &
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
  
  # manually apply scaling factors to source raster
  terra::scoff(r) <- cbind(1 / isub$scalar, 0)
  
  # do conversion of input spatial object 
  if (!missing(x) && !is.null(x)) {
    
    # convert various input types to SpatVector
    if (inherits(x, 'SoilProfileCollection')) {
      x <- as(x, 'sf')
    }
    
    if (inherits(x, c('RasterLayer', 'RasterStack'))) {
      x <- terra::rast(x)
    }
    
    if (!inherits(x, c('SpatRaster', 'SpatVector'))) {
      x <- terra::vect(x)
    }
    
    if (inherits(x, 'SpatVector')) {
      # project any input vector object to CRS of SOLUS
      x <- terra::project(x, terra::crs(r))
    }
    
    xe <- terra::ext(terra::project(terra::as.polygons(x, ext = TRUE), r))
    
    # handle requests out-of-bounds
    if (!(terra::relate(terra::ext(r), xe, relation = "contains")[1] || 
        terra::relate(terra::ext(r), xe, relation = "overlaps")[1])) {
      stop("Extent of `x` is outside the boundaries of the source data extent.", call. = FALSE)
    }
    
    if (!inherits(x, 'SpatRaster')){
      # crop to target extent (written to temp file if needed)
      r <- terra::crop(r, x, filename = filename)
    } else {
      # if x is a spatraster, use it as a template for GDAL warp
      r <- terra::project(r, x, filename = filename, align_only = FALSE, mask = TRUE, threads = TRUE)
    }
  }
  
  if (isTRUE(grid)) {
    return(r)
  } 
  
  if (length(depth_slices) == 1 && method != "step") {
    stop("Cannot interpolate for SoilProfileCollection output with only one depth slice! Change `method` to \"step\" or add another `depth_slice`.", call. = FALSE)
  }
    
  if (!missing(x) && !is.null(x) && inherits(x, 'SpatVector') && terra::is.points(x)) {
    dat <- terra::extract(r, x)
  } else {
    if (!missing(samples) && !is.null(samples)) {
      dat <- terra::spatSample(r,
                               size = samples,
                               method = "regular",
                               xy = TRUE) # for testing
    } else {
      dat <- terra::as.data.frame(r, xy = TRUE, na.rm = FALSE)
    }
  }
  
  dat$ID <- seq(nrow(dat))
    
  spc <- .convert_SOLUS_dataframe_to_SPC(dat, idname = "ID", method = method, max_depth = max_depth)
  aqp::initSpatial(spc, terra::crs(r)) <- ~ x + y
  
  if (isFALSE(grid)) {
    return(spc)
  } else {
    return(list(grid = r, spc = spc))
  }
}

.get_SOLUS_index <- function() {
  
  # TODO: parse XML directly instead of HTML?
  if (!requireNamespace("rvest")) {
    stop("package 'rvest' is required", call. = FALSE)  
  }
  
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

.convert_SOLUS_dataframe_to_SPC <- function(x, idname = "id", method, max_depth = 151) {
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
  
  if (method %in% c("slice", "step")) {
    
    if (method == "slice") {
      
      h$bottom <- h$top + 1
        
    } else {
      message("NOTE: SOLUS predictions represent depth slices (method=\"slice\")\nConsider using method=\"constant\" or method=\"linear\".") 
    
      # apply fudge factors for depth slices as property input source
      h$bottom[h$bottom == 0] <- 5
      h$bottom[h$top == 150] <- max_depth
    }
  
    h <- as.data.frame(h)  
  
    depths(h) <- c(idname, "top", "bottom")
    
    site(h) <- s
    
    return(h)
  } else if (method %in% c("linear", "constant", "fmm", "periodic", "natural", "monoH.FC", "hyman")) {
    
    mindep <- min(h$top, na.rm = TRUE)
    maxdep <- max(h$bottom, na.rm = TRUE)
    
    if (maxdep == 150) {
      maxdep <- max_depth
    }
    
    if (method %in% c("linear", "constant")) {
      FUN <- approxfun
    } else {
      FUN <- splinefun
    }
    
    xx <- (mindep:(maxdep - 1))
    y <- unique(h$top)
    h2 <- h[, data.frame(top = mindep:(maxdep - 1),
                         bottom = (mindep + 1):maxdep,
                         lapply(.SD, function(x) {
                           if (all(is.na(x)))
                             return(rep(NA_real_, length(xx)))
                           FUN(y, x, method = method)(xx)
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

#' @title Get Rapid Carbon Assessment (RaCA) data
#' 
#' @description 
#' 
#' **NOTICE:** The SoilWeb snapshot of the RaCA data has been deprecated. The latest version of the data, including values measured by the Kellogg Soil Survey Laboratory, and supporting documentation, are available here: <https://www.nrcs.usda.gov/resources/data-and-reports/rapid-carbon-assessment-raca>. Download link on National Agricultural Library Ag Data Commons: <https://data.nal.usda.gov/dataset/rapid-carbon-assessment-raca>
#' 
#' Get Rapid Carbon Assessment (RaCA) data by state, geographic bounding-box, RaCA site ID, or soil series query from the SoilWeb API. This interface to the data was an experimental delivery service that does not include the latest soil organic carbon (SOC) measurements. 
#' 
#' Please use [current RaCA distribution](https://data.nal.usda.gov/dataset/rapid-carbon-assessment-raca) if you need lab _measured_ SOC rather than SOC estimated by VNIR.
#' 
#' This interface will be updated sometime calendar year 2022 to include the latest soil morphology, taxonomic classification, and measured SOC values. More detailed coordinates for sample sites should also be available.
#' 
#' @param series a soil series name; case-insensitive
#' 
#' @param bbox a bounding box in WGS84 geographic coordinates e.g. `c(-120, 37, -122, 38)`, constrained to a 5-degree block
#' 
#' @param state a two-letter US state abbreviation; case-insensitive
#' 
#' @param rcasiteid a RaCA site id (e.g. 'C1609C01')
#' 
#' @param get.vnir logical, should associated VNIR spectra be downloaded? (see details)
#' 
#' @details The VNIR spectra associated with RaCA data are quite large (each gzip-compressed VNIR spectra record is about 6.6kb), so requests for these data are disabled by default. Note that VNIR spectra can only be queried by soil series or geographic BBOX.
#' 
#' @return {
#' \describe{
#' \item{\code{pedons}:}{a \code{SoilProfileCollection} object containing site/pedon/horizon data}
#' \item{\code{trees}:}{a \code{data.frame} object containing tree DBH and height}
#' \item{\code{veg}:}{a \code{data.frame} object containing plant species}
#' \item{\code{stock}:}{a \code{data.frame} object containing carbon quantities (stocks) at standardized depths}
#' \item{\code{sample}:}{a \code{data.frame} object containing sample-level bulk density and soil organic carbon values}
#' \item{\code{spectra}:}{a numeric \code{matrix} containing VNIR reflectance spectra from 350--2500 nm}
#' }
#' }
#' @author D.E. Beaudette, USDA-NRCS staff
#' @references {
#'   \url{https://data.nal.usda.gov/dataset/rapid-carbon-assessment-raca}
#' }
#' @seealso \code{\link{fetchOSD}}
#' @export
fetchRaCA <- function(series=NULL, bbox=NULL, state=NULL, rcasiteid=NULL, get.vnir=FALSE) {

  # see https://github.com/ncss-tech/soilDB/issues/249
  .Deprecated(msg = "The SoilWeb snapshot of the RaCA data has been deprecated. The latest version of the data, including values measured by the Kellogg Soil Survey Laboratory, and supporting documentation, are available here: <https://www.nrcs.usda.gov/resources/data-and-reports/rapid-carbon-assessment-raca>. Download link on Box.com: <https://nrcs.app.box.com/s/upx5xhlwis7saunfiysclfrhl5vxxudn>")
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  # important: change the default behavior of data.frame
  opt.original <- options(stringsAsFactors = FALSE)

  # sanity-check: user must supply some kind of criteria
  if(missing(series) & missing(state) & missing(bbox) & missing(rcasiteid))
    stop('you must provide some filtering criteria', call.=FALSE)

  # sanity-check: cannot request VNIR by state
  if(!missing(state) & get.vnir)
    stop('VNIR spectra cannot be requested for an entire state', call.=FALSE)

  ## 2015-09-23
  ## releasing point data for privates lands may be a problem, coordinates are truncated to 2 decimal places
  message('Site coordinates have been truncated to 2 decimal places, contact the National Soil Survey Center for more detailed coordinates.')

  # init empty filter
  f <- vector()

  # init empty pieces
  s <- NULL
  h <- NULL
  trees <- NULL
  veg <- NULL
  stock <- NULL
  sample <- NULL
  vnir <- NULL
  spectra <- NULL

  # process filter components
  if(!missing(series)) {
    f <- c(f, paste0('&series=', series))
  }

  if(!missing(bbox)) {
    bbox <- paste(bbox, collapse=',')
    f <- c(f, paste0('&bbox=', bbox))
  }

  if(!missing(state)) {
    f <- c(f, paste0('&state=', state))
  }

  if(!missing(rcasiteid)) {
    f <- c(f, paste0('&rcasiteid=', rcasiteid))
  }

  # combine filters
  f <- paste(f, collapse = '')

  # build URLs
  site.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=site', f))
  hz.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=horizon', f))
  trees.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=trees', f))
  veg.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=veg', f))
  stock.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=stock', f))
  sample.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=sample', f))
  vnir.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/rca/rca_query.php?what=vnir', f))

  # init temp files
  tf.site <- tempfile()
  tf.hz <- tempfile()
  tf.trees <- tempfile()
  tf.veg <- tempfile()
  tf.stock <- tempfile()
  tf.sample <- tempfile()
  tf.vnir <- tempfile()

  # download pieces
  curl::curl_download(url = site.url, destfile = tf.site, mode = 'wb', handle = .soilDB_curl_handle(), quiet = TRUE)
  curl::curl_download(url = hz.url, destfile = tf.hz, mode = 'wb', handle = .soilDB_curl_handle(), quiet = TRUE)
  curl::curl_download(url = trees.url, destfile = tf.trees, mode = 'wb',  handle = .soilDB_curl_handle(), quiet = TRUE)
  curl::curl_download(url = veg.url, destfile = tf.veg, mode = 'wb', handle = .soilDB_curl_handle(), quiet = TRUE)
  curl::curl_download(url = stock.url, destfile = tf.stock, mode = 'wb', handle = .soilDB_curl_handle(), quiet = TRUE)
  curl::curl_download(url = sample.url, destfile = tf.sample, mode = 'wb',  handle = .soilDB_curl_handle(), quiet = TRUE)

  # load pieces
  try(s <- read.table(gzfile(tf.site), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  try(h <- read.table(gzfile(tf.hz), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  try(trees <- read.table(gzfile(tf.trees), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  try(veg <- read.table(gzfile(tf.veg), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)

  ### 2014-01-16: data need to be re-generated, offline for now:
  message('Carbon concentration and stock values are probably wrong, or at least suspect. USE WITH CAUTION.')
  try(stock <- read.table(gzfile(tf.stock), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)
  try(sample <- read.table(gzfile(tf.sample), header=TRUE, sep='|', quote='', comment.char=''), silent=TRUE)

  # optionally load spectra
  if(get.vnir) {
    message('spectra are large, download may take some time...', appendLF=TRUE)

    # save the file locally
    curl::curl_download(url = vnir.url, destfile = tf.vnir, mode = 'wb',  handle = .soilDB_curl_handle(), quiet = TRUE)
    # try to open
    try(vnir <- read.table(gzfile(tf.vnir), header=TRUE, sep='|'), silent=TRUE)

    # test for missing data
    if(!is.null(vnir)) {
      # extract and parse the serialized spectra as matrix
      spectra <- as.matrix(read.table(textConnection(vnir$spectra), header=FALSE, sep=','))

      # since order is preserved (row-wise and col-wise), we can assign:
      # rownames = sample_id
      # colnames = wavenumber
      dimnames(spectra)[[1]] <- vnir$sample_id
      dimnames(spectra)[[2]] <- 350:2500
    }
  }

  # report missing data
  if(all(c(is.null(s), is.null(h)))) {
    stop('query returned no data', call.=FALSE)
  }

  # upgrade to SoilProfileCollection
  aqp::depths(h) <- rcapid ~ hzdept + hzdepb

  # extract landuse, region, soilgroup as characters
  s$landuse <- substr(s$rcasiteid, 6, 6)
  s$region <- substr(s$rcasiteid, 2, 3)
  s$soilgroup <- substr(s$rcasiteid, 2, 5)

  # set NASIS-specific horizon identifier
  aqp::hzidname(h) <- 'phiid'

  # set optional hz designation and texture slots
  aqp::hzdesgnname(h) <- "hzname"
  aqp::hztexclname(h) <- "texture_class"

  # merge-in site data
  aqp::site(h) <- s

  # reset options:
  options(opt.original)

  # pack into a list for the user
  res <- list(
    pedons = h,
    trees = trees,
    veg = veg,
    stock = stock,
    sample = sample,
    spectra = spectra
  )
  res.size <- round(object.size(res) / 1024 / 1024, 2)

  # some feedback via message:
  message(paste0(length(unique(h$rcasiteid)), ' RaCA sites loaded (', res.size, ' Mb transferred)'))

  # done
  return(res)

}

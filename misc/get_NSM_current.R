#' Download nationalsoilmoisture.com CONUS Soil Moisture Percentile ASCII Grids
#'
#' @return data.frame containing file name root, grid label, source URL and download status
#' @export
#'
#' @examples
#' # library(terra)
#' 
#' # options(timeout = 300)
#' # nsm_products <- get_NSM_current()
#' # x <- rast(nsm_products$filename)
#' 
#' ## as-downloaded (2022/09/18) missing rows at end of file, fixed manually
#' # plot(x$nldas_05_current)
#' # plot(x$nldas_50_current)
#' # writeRaster(x, "~/Geodata/NationalSoilMoisture.tif", overwrite = TRUE)
#' # file.remove(list.files(pattern = "\\.asc"))
#' 
#' # x <- rast("~/Geodata/NationalSoilMoisture.tif")
#' # plot(x)
get_NSM_current <- function() {
  x <- read.table(
    header = TRUE,
    text = 'root label
   rk_05 "RK 5cm"
   rk_20 "RK 20cm"
   rk_50 "RK 50cm"
   last7days "Precipitation last seven days"
   nldas_05 "NLDAS Soil Moisture Layer 1"
   nldas_20 "NLDAS Soil Moisture Layer 2"
   nldas_50 "NLDAS Soil Moisture Layer 3"
   smap_05 "SMAP L3"
   smap3_05 "SMAP L3 (3-Day Mean)"
   blend_05 "All Blend 5cm"
   alluncertaintymax_05 "All Blend 5cm Maximum Uncertainty"
   alluncertaintymean_05 "All Blend 5cm Mean Uncertainty"
   nkblend_05 "NLDAS/RK 5cm Blend"
   nkuncertainty_05 "NLDAS/RK 5cm Uncertainty"
   nkblend_20 "NLDAS/RK 20cm Blend"
   nkuncertainty_20 "NLDAS/RK 20cm Uncertainty"
   nkblend_50 "NLDAS/RK 50cm Blend"
   nkuncertainty_50 "NLDAS/RK 50cm Uncertainty"'
  )
  x$filename <- paste0(x$root, "_current.asc")
  x$url <-
    paste0("http://nationalsoilmoisture.com/", x$filename)
  x$downloaded <- try(download.file(x$url, x$filename))
  x
}

# see also: http://nationalsoilmoisture.com/in_situ_05_current.txt etc.

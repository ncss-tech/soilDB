# Fetch Harmonized World Soil Database Data

Creates a local cache of FAO Harmonized World Soil Database (HWSD)
information. Source raster map in ESRI Grid format (.bil) is converted
to GeoTIFF. The source tabular database in Microsoft Access (.mdb)
format is converted to SQLite.

## Usage

``` r
fetchHWSD(
  x = NULL,
  hwsd_url = "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/",
  hwsd_version = 2L,
  force = FALSE
)

get_HWSD_path(what = c("sqlite", "mdb", "raster", "path"), hwsd_version = 2L)
```

## Source

Food and Agriculture Organization of the United Nations (FAO), Soils
Portal, Harmonized World Soil Database (HWSD) v2.0
<https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/>

## Arguments

- x:

  A *SpatRaster*, *SpatVector*, *SpatExtent*, or any other object that
  has a *SpatExtent*. Default `NULL` returns the full dataset. Extent of
  interest that is passed to
  [`terra::crop()`](https://rspatial.github.io/terra/reference/crop.html)
  `y` argument.

- hwsd_url:

  *character*. URL for downloading HWSD dataset. Default
  `"https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/"`

- hwsd_version:

  *integer*. Default `2L`.

- force:

  Force rebuilding of cache. Default: `FALSE`

- what:

  *character*. One of "sqlite", "raster", or "path"

## Value

A *SpatRaster* object with mapunit-level aggregate information stored as
categories.

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- terra::vect(system.file("ex", "lux.shp", package = "terra"))
  res <- fetchHWSD(x)
  
  # categorical data (WRB class)
  terra::activeCat(res) <- "WRB4"
  
  # view WRB4 map
  terra::plot(res)
  terra::lines(x, col = "white")
  
  # convert categories containing numeric data to numeric values
  res2 <- terra::catalyze(res)
  
  # view AWC map
  terra::plot(res2$AWC, main = "Available Water Capacity, mm")
  terra::lines(x, col = "white")
  
  # access tabular data from cached SQLite database
  SDA_query("SELECT * FROM HWSD2_SMU LIMIT 1", dsn = get_HWSD_path())
} # }
```

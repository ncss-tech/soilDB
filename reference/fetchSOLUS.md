# Fetch Soil Landscapes of the United States (SOLUS) Grids

This tool creates a virtual raster or downloads data for an extent from
Cloud Optimized GeoTIFFs (COGs) from the [Soil Landscapes of the United
States 100-meter (SOLUS100) soil property maps project
repository](https://agdatacommons.nal.usda.gov/articles/dataset/Data_from_Soil_Landscapes_of_the_United_States_100-meter_SOLUS100_soil_property_maps_project_repository/25033856).

## Usage

``` r
fetchSOLUS(
  x = NULL,
  depth_slices = c(0, 5, 15, 30, 60, 100, 150),
  variables = c("anylithicdpt", "caco3", "cec7", "claytotal", "dbovendry", "ec", "ecec",
    "fragvol", "gypsum", "ph1to1h2o", "resdept", "sandco", "sandfine", "sandmed",
    "sandtotal", "sandvc", "sandvf", "sar", "silttotal", "soc"),
  output_type = c("prediction", "relative prediction interval",
    "95% low prediction interval", "95% high prediction interval"),
  grid = TRUE,
  samples = NULL,
  method = c("linear", "constant", "fmm", "natural", "monoH.FC", "step", "slice"),
  max_depth = 151,
  filename = NULL,
  overwrite = FALSE
)
```

## Arguments

- x:

  An R spatial object (such as a *SpatVector*, *SpatRaster*, or *sf*
  object) or a *SoilProfileCollection* with coordinates initialized via
  `aqp::initSpatial<-`. Default: `NULL` returns the CONUS extent as
  virtual raster. If `x` is a *SpatRaster* the coordinate reference
  system, extent, and resolution are used as a template for the output
  raster.

- depth_slices:

  character. One or more of: `"0"`, `"5"`, `"15"`, `"30"`, `"60"`,
  `"100"`, `"150"`. The "depth slice" `"all"` (used for variables such
  as `"anylithicdpt"`, and `"resdept"`) is always included if any
  site-level variables are selected.

- variables:

  character. One or more of: `"anylithicdpt"`, `"caco3"`, `"cec7"`,
  `"claytotal"`, `"dbovendry"`, `"ec"`, `"ecec"`, `"fragvol"`,
  `"gypsum"`, `"ph1to1h2o"`, `"resdept"`, `"sandco"`, `"sandfine"`,
  `"sandmed"`, `"sandtotal"`, `"sandvc"`, `"sandvf"`, `"sar"`,
  `"silttotal"`, `"soc"`.

- output_type:

  character. One or more of: `"prediction"`,
  `"relative prediction interval"`, `"95% low prediction interval"`,
  `"95% high prediction interval"`

- grid:

  logical. Default `TRUE` returns a *SpatRaster* object for an extent.
  `FALSE` returns a *SoilProfileCollection*. Any other value returns a
  *list* object with names `"grid"` and `"spc"` containing both result
  objects.

- samples:

  integer. Number of regular samples to return for
  *SoilProfileCollection* output. Default `NULL` will convert all grid
  cells to a unique profile. Note that for a large extent, this can
  produce large objects with a very large number of layers (especially
  with `method` other than `"step"`).

- method:

  character. Used to determine depth interpolation method for
  *SoilProfileCollection* output. Default: `"linear"`. Options include
  any `method` allowed for
  [`approxfun()`](https://rdrr.io/r/stats/approxfun.html) or
  [`splinefun()`](https://rdrr.io/r/stats/splinefun.html) plus `"step"`
  and `"slice"`. `"step"` uses the prediction depths as the top and
  bottom of each interval to create a piecewise continuous profile to
  maximum of 200 cm depth (for 150 cm upper prediction depth). `"slice"`
  returns a discontinuous profile with 1 cm thick slices at the
  predicted depths. Both `"step"` and `"slice"` return a number of
  layers equal to length of `depth_slices`, and all other methods return
  data in interpolated 1cm slices.

- max_depth:

  integer. Maximum depth to interpolate 150 cm slice data to. Default:
  `151`. Interpolation deeper than 151 cm is not possible for methods
  other than `"step"` and will result in missing values.

- filename:

  character. Path to write output raster file. Default: `NULL` will keep
  result in memory (or store in temporary file if memory threshold is
  exceeded)

- overwrite:

  Overwrite `filename` if it exists? Default: `FALSE`

## Value

A *SpatRaster* object containing SOLUS grids for specified extent,
depths, variables, and product types.

## Details

If the input object `x` is not specified (`NULL` or missing), a
*SpatRaster* object using the virtual URLs is returned. The full extent
and resolution data set can be then downloaded and written to file using
[`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)
(or any other processing step specifying an output file name). When
input object `x` is specified, a *SpatRaster* object using in memory or
local (temporary file or `filename`) resources is returned after
downloading the data only for the target extent. In the case where `x`
is a *SoilProfileCollection* or an *sf* or *SpatVector* object
containing point geometries, the result will be a
*SoilProfileCollection* for values extracted at the point locations. To
return both the *SpatRaster* and *SoilProfileCollection* object output
in a *list*, use `grid = NULL`.

## References

Nauman, T. W., Kienast-Brown, S., Roecker, S. M., Brungard, C., White,
D., Philippe, J., & Thompson, J. A. (2024). Soil landscapes of the
United States (SOLUS): developing predictive soil property maps of the
conterminous United States using hybrid training sets. Soil Science
Society of America Journal, 88, 2046–2065.
[doi:10.1002/saj2.20769](https://doi.org/10.1002/saj2.20769)

## Author

Andrew G. Brown

## Examples

``` r
if (FALSE) { # \dontrun{
b <- c(-119.747629, -119.67935, 36.912019, 36.944987)

bbox.sp <- sf::st_as_sf(wk::rct(
  xmin = b[1], xmax = b[2], ymin = b[3], ymax = b[4],
  crs = sf::st_crs(4326)
))

ssurgo.geom <- soilDB::SDA_spatialQuery(
  bbox.sp,
  what = 'mupolygon',
  db = 'SSURGO',
  geomIntersection = TRUE
)

# grid output
res <- fetchSOLUS(
  ssurgo.geom,
  depth_slices = "0",
  variables = c("sandtotal", "silttotal", "claytotal", "cec7"),
  output_type = "prediction"
)

terra::plot(res)

# SoilProfileCollection output, using linear interpolation for 1cm slices
# site-level variables (e.g. resdept) added to site data.frame of SPC
res <- fetchSOLUS(
  ssurgo.geom,
  depth_slices = c("0", "5", "15", "30", "60", "100", "150"),
  variables = c("sandtotal", "silttotal", "claytotal", "cec7", "resdept"),
  output_type = "prediction",
  method = "linear",
  grid = FALSE,
  samples = 10
)

# plot, truncating each profile to the predicted restriction depth
aqp::plotSPC(trunc(res, 0, res$resdept_p), color = "claytotal_p", divide.hz = FALSE)
} # }
```

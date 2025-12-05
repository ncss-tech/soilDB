# Get SoilGrids 2.0 Property Estimates for Points or Spatial Extent

This function obtains [SoilGrids 2.0](https://soilgrids.org) properties
information (250m raster resolution) given a `data.frame` containing
site IDs, latitudes and longitudes, or a spatial extent (see `grid=TRUE`
argument).

SoilGrids API and maps return values as whole (integer) numbers to
minimize the storage space used. These values have conversion factors
applied by `fetchSoilGrids()` to produce conventional units shown in the
table below (see Details).

## Usage

``` r
fetchSoilGrids(
  x,
  loc.names = c("id", "lat", "lon"),
  depth_intervals = c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200"),
  variables = c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt",
    "soc", "ocd", "wv0010", "wv0033", "wv1500"),
  grid = FALSE,
  filename = NULL,
  overwrite = TRUE,
  target_resolution = c(250, 250),
  summary_type = c("Q0.05", "Q0.5", "Q0.95", "mean"),
  endpoint = ifelse(!grid, "https://rest.isric.org/soilgrids/v2.0/properties/query",
    "https://files.isric.org/soilgrids/latest/data/"),
  ...,
  verbose = FALSE,
  progress = FALSE
)
```

## Arguments

- x:

  A `data.frame` containing 3 columns referring to site ID, latitude and
  longitude. Or a spatial (sf, terra) object for which a bounding box
  can be calculated when `grid=TRUE`.

- loc.names:

  Optional: Column names referring to site ID, latitude and longitude.
  Default: `c("id", "lat", "lon")`

- depth_intervals:

  Default: `"0-5"`, `"5-15"`, `"15-30"`, `"30-60"`, `"60-100"`,
  `"100-200"`

- variables:

  Default: `"bdod"`, `"cec"`, `"cfvo"`, `"clay"`, `"nitrogen"`,
  `"phh2o"`, `"sand"`, `"silt"`, `"soc"`, `"ocd"`, `"wv0010"`,
  `"wv0033"`, `"wv1500"`. Optionally `"ocs"` (only for 0 to 30 cm
  interval).

- grid:

  Download subset of SoilGrids Cloud Optimized GeoTIFF? Default: `FALSE`

- filename:

  Only used when `grid=TRUE`. If `NULL` defaults to an in-memory raster,
  or temporary file if result does not fit in memory.

- overwrite:

  Only used when `grid=TRUE`. Default: `FALSE`

- target_resolution:

  Only used when `grid=TRUE`. Default: `c(250, 250)` (250m x 250m
  pixels)

- summary_type:

  Only used when `grid=TRUE`. One or more of `"Q0.05"`, `"Q0.5"`,
  `"Q0.95"`, `"mean"`; these are summary statistics that correspond to
  5th, 50th, 95th percentiles, and mean value for selected `variables`.

- endpoint:

  Optional: custom API endpoint. Default:
  `"https://rest.isric.org/soilgrids/v2.0/properties/query"` when
  `grid=FALSE`; `"https://files.isric.org/soilgrids/latest/data/"` when
  `grid=TRUE`.

- ...:

  Additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)
  when `grid=TRUE`.

- verbose:

  Print messages? Default: `FALSE`

- progress:

  logical, give progress when iterating over multiple requests; Default:
  `FALSE`

## Value

A *SoilProfileCollection* (or *SpatRaster* when `grid=TRUE`). Returns
`try-error` if all requests fail. Any error messages resulting from
parsing will be echoed when `verbose=TRUE`.

## Details

### Properties

|          |                                                                                        |                         |                   |                     |
|----------|----------------------------------------------------------------------------------------|-------------------------|-------------------|---------------------|
| Name     | Description                                                                            | Mapped units            | Conversion factor | Conventional units  |
| bdod     | Bulk density of the fine earth fraction                                                | cg/cm^3                 | 100               | kg/dm^3             |
| cec      | Cation Exchange Capacity of the soil                                                   | mmol(c)/kg              | 10                | cmol(c)/kg          |
| cfvo     | Volumetric fraction of coarse fragments (\> 2 mm)                                      | cm^3/dm^3 (vol per mil) | 10                | cm^3/100cm^3 (vol%) |
| clay     | Proportion of clay particles (\< 0.002 mm) in the fine earth fraction                  | g/kg                    | 10                | g/100g (%)          |
| nitrogen | Total nitrogen (N)                                                                     | cg/kg                   | 100               | g/kg                |
| phh2o    | Soil pH                                                                                | pH\*10                  | 10                | pH                  |
| sand     | Proportion of sand particles (\> 0.05 mm) in the fine earth fraction                   | g/kg                    | 10                | g/100g (%)          |
| silt     | Proportion of silt particles (\>= 0.002 mm and \<= 0.05 mm) in the fine earth fraction | g/kg                    | 10                | g/100g (%)          |
| soc      | Soil organic carbon content in the fine earth fraction                                 | dg/kg                   | 10                | g/kg                |
| ocd      | Organic carbon density                                                                 | hg/m^3                  | 10                | kg/m^3              |
| ocs      | Organic carbon stocks (0-30cm depth interval only)                                     | t/ha                    | 10                | kg/m^2              |
| wv0010   | Volumetric Water Content at 10kPa                                                      | 0.1 v% or 1 mm/m        | 10                | volume (%)          |
| wv0033   | Volumetric Water Content at 33kPa                                                      | 0.1 v% or 1 mm/m        | 10                | volume (%)          |
| wv1500   | Volumetric Water Content at 1500kPa                                                    | 0.1 v% or 1 mm/m        | 10                | volume (%)          |

SoilGrids predictions are made for the six standard depth intervals
specified in the GlobalSoilMap IUSS working group and its
specifications. The default depth intervals returned are (in
centimeters): `"0-5"`, `"5-15"`, `"15-30"`, `"30-60"`, `"60-100"`,
`"100-200"` for the properties `"bdod"`, `"cec"`, `"cfvo"`, `"clay"`,
`"nitrogen"`, `"phh2o"`, `"sand"`, `"silt"`, `"soc"`, `"ocd"`,
`"wv0010"`, `"wv0033"`, `"wv1500"`–each with percentiles (5th, 50th,
95th), mean and uncertainty values. The summary statistic name will be
appended to the abbreviate variable name for each depth interval
returned. Soil organic carbon stocks (0-30cm) (`variables="ocs"`) are
returned only for `depth_intervals="0-30"`. The uncertainty values are
the ratio between the inter-quantile range (90% prediction interval
width) and the median : `(Q0.95-Q0.05)/Q0.50.` All values are converted
from "mapped" to "conventional" based on above table conversion factors.
Point data requests are made through `"properties/query"` endpoint of
the [SoilGrids v2.0 REST
API](https://www.isric.org/explore/soilgrids/faq-soilgrids). Please
check ISRIC's data policy, disclaimer and citation:
<https://www.isric.org/about/data-policy>.

Find out more information about the SoilGrids and GlobalSoilMap products
here:

- <https://www.isric.org/explore/soilgrids/faq-soilgrids>

- <https://www.isric.org/sites/default/files/GlobalSoilMap_specifications_december_2015_2.pdf>

## References

- **Common soil chemical and physical properties:** Poggio, L., de
  Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro,
  E., and Rossiter, D.: SoilGrids 2.0: producing soil information for
  the globe with quantified spatial uncertainty, SOIL, 7, 217–240, 2021.
  DOI:
  [doi:10.5194/soil-7-217-2021](https://doi.org/10.5194/soil-7-217-2021)

- **Soil water content at different pressure heads:** Turek, M.E.,
  Poggio, L., Batjes, N. H., Armindo, R. A., de Jong van Lier, Q., de
  Sousa, L.M., Heuvelink, G. B. M. : Global mapping of volumetric water
  retention at 100, 330 and 15000 cm suction using the WoSIS database,
  International Soil and Water Conservation Research, 11-2,
  225-239, 2023. DOI:
  [doi:10.1016/j.iswcr.2022.08.001](https://doi.org/10.1016/j.iswcr.2022.08.001)

## Author

Andrew G. Brown

## Examples

``` r
if (FALSE) { # \dontrun{
  library(aqp)
  
  your.points <- data.frame(id  = c("A", "B"), 
                            lat = c(37.9, 38.1), 
                            lon = c(-120.3, -121.5), 
                            stringsAsFactors = FALSE)
  x <- try(fetchSoilGrids(your.points))
 
  if (!inherits(x, 'try-error'))
   aqp::plotSPC(x, name = NA, color = "socQ50")
 
  # organic carbon stocks use 0-30cm interval
  y <- try(fetchSoilGrids(your.points[1, ], 
                          depth_interval = c("0-5", "0-30", "5-15", "15-30"),
                          variables = c("soc", "bdod", "ocd", "ocs")))
                          
  # extract horizons from a SoilProfileCollection where horizon 2 overlaps 1, 3, and 4
  h <- aqp::horizons(y)
  
  # "ocs" (organic carbon stock 0-30cm interval)
  h[2, ]
  
  h$thickness_meters <- ((h$hzdepb - h$hzdept) / 100)

  # estimate "ocs" from modeled organic carbon and bulk density in 0-5, 5-15, 15-30 intervals
  #  (sum the product of soc, bdod, and thickness in meters)
  #  (1 gram per cubic decimeter = 1 kilogram per cubic meter)
  sum(h$socmean * h$bdodmean * h$thickness_meters, na.rm = TRUE)
  
  # estimate "ocs" from modeled organic carbon density in 0-5, 5-15, 15-30 intervals
  #  (sum the product of "ocd" and thickness in meters)
  sum(h$ocdmean * h$thickness_meters, na.rm = TRUE)
 
} # }
```

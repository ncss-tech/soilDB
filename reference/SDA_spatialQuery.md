# Query Soil Data Access by spatial intersection with supplied geometry

Query SDA (SSURGO / STATSGO) records via spatial intersection with
supplied geometries. Input can be an 'sf', 'terra', or 'sp' object with
a valid CRS. Map unit keys, overlapping polygons, or the spatial
intersection of `geom` + SSURGO / STATSGO polygons can be returned. See
details.

## Usage

``` r
SDA_spatialQuery(
  geom,
  what = "mukey",
  geomIntersection = FALSE,
  geomAcres = TRUE,
  db = c("SSURGO", "STATSGO", "SAPOLYGON"),
  byFeature = FALSE,
  idcol = "gid",
  addFields = NULL,
  query_string = FALSE,
  as_Spatial = getOption("soilDB.return_Spatial", default = FALSE)
)
```

## Arguments

- geom:

  an sf, terra, or sp object with valid CRS. May contain multiple
  features.

- what:

  a character vector specifying what to return. `'mukey'`: `data.frame`
  with intersecting map unit keys and names, `'mupolygon'`, `'mupoint'`,
  `'muline'` overlapping or intersecting map unit polygons, points or
  lines from selected database, `'featpoint'` or `'featline'` for
  special feature points and lines, `'areasymbol'`: `data.frame` with
  intersecting soil survey areas, `'sapolygon'`: overlapping or
  intersecting soil survey area polygons (SSURGO only)

- geomIntersection:

  logical; `FALSE` (default): overlapping map unit polygons returned,
  `TRUE`: intersection of `geom` + map unit polygons is returned.

- geomAcres:

  logical; `TRUE` (default): calculate acres of result geometry in
  column `"area_ac"` when `what` returns a geometry column. `FALSE` does
  not calculate acres.

- db:

  a character vector identifying the Soil Geographic Databases
  (`'SSURGO'` or `'STATSGO'`) to query. Option `STATSGO` works with
  `what = "mukey"` and `what = "mupolygon"`.

- byFeature:

  Iterate over features, returning a combined data.frame where each
  feature is uniquely identified by value in `idcol`. Default `FALSE`.

- idcol:

  Unique IDs used for individual features when `byFeature = TRUE`;
  Default `"gid"`

- addFields:

  character; Amend result with a query to `mapunit` table for additional
  information? Default: `NULL`. A character vector can be used to
  specify columns from the `legend`, `mapunit`, and `muaggatt` tables
  (for `mupolygon`, `mupoint`, `muline` and `mukey`), `legend` table for
  areasymbol and sapolygon, and `featdesc` for `featpoint` and
  `featline`. Mapunit and mapunit aggregate attribute tables are ignored
  for soil survey area polygon results.

- query_string:

  Default: `FALSE`; if `TRUE` return a character string containing query
  that would be sent to SDA via `SDA_query`

- as_Spatial:

  For `what` that return spatial data, return `sp` package classes
  instead of `sf`? e.g. `Spatial*DataFrame`. Default: `FALSE`.

## Value

A `data.frame` if `what = 'mukey'`, otherwise an `sf` object. A
`try-error` in the event the request cannot be made or if there is an
error in the query.

## Details

Queries for map unit keys are always more efficient vs. queries for
overlapping or intersecting (i.e. least efficient) features. `geom` is
converted to GCS / WGS84 as needed. Map unit keys are always returned
when using `what = "mupolygon"`.

SSURGO (detailed soil survey, typically 1:24,000 scale) and STATSGO
(generalized soil survey, 1:250,000 scale) data are stored together
within SDA. This means that queries that don't specify an area symbol
may result in a mixture of SSURGO and STATSGO records. See the examples
below and the [SDA
Tutorial](http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.md) for
details.

## Note

Row-order is not preserved across features in `geom` and returned
object. Use `byFeature` argument to iterate over features and return
results that are 1:1 with the inputs. Polygon area in acres is computed
server-side when `what = 'mupolygon'` and `geomIntersection = TRUE`.

## See also

[`SDA_query`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)

## Author

D.E. Beaudette, A.G. Brown, D.R. Schlaepfer

## Examples

``` r
if (FALSE) { # \dontrun{
library(aqp)
library(sf)

## query at a point

# example point
p <- sf::st_as_sf(data.frame(x = -119.72330, y = 36.92204),
                  coords = c('x', 'y'),
                  crs = 4326)

# query map unit records at this point
res <- SDA_spatialQuery(p, what = 'mukey')

# convert results into an SQL "IN" statement
# useful when there are multiple intersecting records
mu.is <- format_SQL_in_statement(res$mukey)

# composite SQL WHERE clause
sql <- sprintf("mukey IN %s", mu.is)

# get commonly used map unit / component / chorizon records
# as a SoilProfileCollection object
# request that results contain `mukey` with `duplicates = TRUE`
x <- fetchSDA(sql, duplicates = TRUE)

# safely set texture class factor levels
# by making a copy of this column
# this will save in lieu of textures in the original
# `texture` column
aqp::horizons(x)$texture.class <- factor(x$texture, levels = aqp::SoilTextureLevels())

# graphical depiction of the result
aqp::plotSPC(
  x,
  color = 'texture.class',
  label = 'compname',
  name = 'hzname',
  cex.names = 1,
  width = 0.25,
  plot.depth.axis = FALSE,
  hz.depths = TRUE,
  name.style = 'center-center'
)

## query mukey + geometry that intersect with a bounding box

# define a bounding box: xmin, xmax, ymin, ymax
#
#         +-------------------(ymax, xmax)
#         |                        |
#         |                        |
#     (ymin, xmin) ----------------+
b <- c(-119.747629, -119.67935, 36.912019, 36.944987)

# convert bounding box to WKT
bbox.sp <- sf::st_as_sf(wk::rct(
  xmin = b[1],
  xmax = b[2],
  ymin = b[3],
  ymax = b[4],
  crs = sf::st_crs(4326)
))

# results contain associated map unit keys (mukey)
# return SSURGO polygons, after intersection with provided BBOX
ssurgo.geom <- SDA_spatialQuery(bbox.sp,
                                what = 'mupolygon',
                                db = 'SSURGO',
                                geomIntersection = TRUE)

# return STATSGO polygons, after intersection with provided BBOX
statsgo.geom <- SDA_spatialQuery(bbox.sp,
                                 what = 'mupolygon',
                                 db = 'STATSGO',
                                 geomIntersection = TRUE)

# inspect results
par(mar = c(0, 0, 3, 1))
plot(sf::st_geometry(ssurgo.geom), border = 'royalblue')
plot(
  sf::st_geometry(statsgo.geom),
  lwd = 2,
  border = 'firebrick',
  add = TRUE
)
plot(sf::st_geometry(bbox.sp), lwd = 3, add = TRUE)
legend(
  x = 'topright',
  legend = c('BBOX', 'STATSGO', 'SSURGO'),
  lwd = c(3, 2, 1),
  col = c('black', 'firebrick', 'royalblue'),
)

# quick reminder that STATSGO map units often contain many components
# format an SQL IN statement using the first STATSGO mukey
mu.is <- format_SQL_in_statement(statsgo.geom$mukey[1])

# composite SQL WHERE clause
sql <- sprintf("mukey IN %s", mu.is)

# get commonly used map unit / component / chorizon records
# as a SoilProfileCollection object
x <- fetchSDA(sql)

# tighter figure margins
par(mar = c(0, 0, 3, 1))

# organize component sketches by national map unit symbol
# color horizons via awc
# adjust legend title
# add alternate label (vertical text) containing component percent
# move horizon names into the profile sketches
# make profiles wider
aqp::groupedProfilePlot(
  x,
  groups = 'nationalmusym',
  label = 'compname',
  color = 'awc_r',
  col.label = 'Available Water Holding Capacity (cm / cm)',
  alt.label = 'comppct_r',
  name.style = 'center-center',
  width = 0.3
)

mtext(
  'STATSGO (1:250,000) map units contain a lot of components!',
  side = 1,
  adj = 0,
  line = -1.5,
  at = 0.25,
  font = 4
)
} # }
```

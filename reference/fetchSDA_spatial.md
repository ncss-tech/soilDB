# Get Spatial Data from Soil Data Access by `mukey`, `nationalmusym` or `areasymbol`

This method facilitates queries to Soil Data Access (SDA) mapunit and
survey area geometry. Queries are generated based on map unit key
(`mukey`) and national map unit symbol (`nationalmusym`) for `mupolygon`
(SSURGO) or `gsmmupolygon` (STATSGO) geometry OR legend key (`lkey`) and
area symbols (`areasymbol`) for `sapolygon` (Soil Survey Area; SSA)
geometry).

A Soil Data Access query returns geometry and key identifying
information about the map unit or area of interest. Additional columns
from the map unit or legend table can be included; see `add.fields`
argument.

## Usage

``` r
fetchSDA_spatial(
  x,
  by.col = "mukey",
  method = "feature",
  geom.src = "mupolygon",
  db = "SSURGO",
  add.fields = NULL,
  chunk.size = 10,
  verbose = TRUE,
  as_Spatial = getOption("soilDB.return_Spatial", default = FALSE)
)
```

## Arguments

- x:

  A vector of map unit keys (`mukey`) or national map unit symbols
  (`nationalmusym`) for `mupolygon`, `muline` or `mupoint`; feature keys
  (`featkey`) for `featpoint` and `featline`; legend keys (`lkey`) or
  soil survey area symbols (`areasymbol`) for `sapolygon` geometry. If
  `geom.src="mlrapolygon"` then `x` refers to `MLRARSYM` (major land
  resource area symbols).

- by.col:

  Column name containing map unit identifier `"mukey"`,
  `"nationalmusym"`, or `"ecoclassid"` for `geom.src` `mupolygon` OR
  `"areasymbol"`, `"areaname"`, `"mlraoffice"`, `"mouagncyresp"` for
  `geom.src` `sapolygon`; default is determined by
  `isTRUE(is.numeric(x))` for `mukey`, `featkey` or `lkey`, using
  `nationalmusym` or `areasymbol` otherwise.

- method:

  geometry result type: `"feature"` returns polygons, `"bbox"` returns
  the bounding box of each polygon (via `STEnvelope()`), `"point"`
  returns a single point (via `STPointOnSurface()`) within each polygon,
  `"extent"` returns an aggregate bounding box (the extent of all
  polygons, `geometry::EnvelopeAggregate()`) ), `"convexhull"`
  (`geometry::ConvexHullAggregate()`) returns the aggregate convex hull
  around all polygons, `"union"` (`geometry::UnionAggregate()`) and
  `"collection"` (`geometry::CollectionAggregate()`) return a
  `MULTIPOLYGON` or a `GEOMETRYCOLLECTION`, respectively, for each
  `mukey`, `nationalmusym`, or `areasymbol `. In the case of the latter
  four aggregation methods, the groups for aggregation depend on
  `by.col` (default by `"mukey"`).

- geom.src:

  Either `mupolygon` (map unit polygons), `muline` (map unit lines),
  `mupoint` (map unit points), `featpoint` (feature points), `featline`
  (feature lines), `sapolygon` (soil survey area boundary polygons), or
  `mlrapolygon` (major land resource area boundary polygons)

- db:

  Default: `"SSURGO"`. When `geom.src` is `mupolygon`, use STATSGO
  polygon geometry instead of SSURGO by setting `db = "STATSGO"`

- add.fields:

  Column names from `mapunit` or `legend` table to add to result. Must
  specify parent table name as the prefix before column name e.g.
  `mapunit.muname`.

- chunk.size:

  Number of values of `x` to process per query. Necessary for large
  results. Default: `10`

- verbose:

  Print messages?

- as_Spatial:

  Return sp classes? e.g. `Spatial*DataFrame`. Default: `FALSE`.

## Value

an `sf` data.frame corresponding to SDA spatial data for all symbols
requested. If `as_Spatial=TRUE` returns a `Spatial*DataFrame` from the
sp package via
[`sf::as_Spatial()`](https://r-spatial.github.io/sf/reference/coerce-methods.html)
for backward compatibility. Default result contains geometry with
attribute table containing unique feature ID, symbol and area symbol
plus additional fields in result specified with `add.fields`.

## Details

This function automatically "chunks" the input vector (using
[`makeChunks()`](http://ncss-tech.github.io/soilDB/reference/makeChunks.md))
of map unit identifiers to minimize the likelihood of exceeding the SDA
data request size. The number of chunks varies with the `chunk.size`
setting and the length of your input vector. If you are working with
many map units and/or large extents, you may need to decrease this
number in order to have more chunks.

Querying regions with complex mapping may require smaller `chunk.size`.
Numerically adjacent IDs in the input vector may share common qualities
(say, all from same soil survey area or region) which could cause
specific chunks to perform "poorly" (slow or error) no matter what the
chunk size is. Shuffling the order of the inputs using
[`sample()`](https://rdrr.io/r/base/sample.html) may help to eliminate
problems related to this, depending on how you obtained your set of
MUKEY/nationalmusym to query. One could feasibly use `muacres` as a
heuristic to adjust for total acreage within chunks.

Note that STATSGO data are fetched where `CLIPAREASYMBOL = 'US'` to
avoid duplicating state and national subsets of the geometry.

A prototype interface, `geom.src="mlrapolygon"`, is provided for
obtaining Major Land Resource Area (MLRA) polygon boundaries. When using
this geometry source `x` is a vector of `MLRARSYM` (MLRA Symbols). The
geometry source is the MLRA Geographic Database v5.2 (2022) which is not
(yet) part of Soil Data Access. Instead of SDA, GDAL utilities are used
to read a zipped ESRI Shapefile from a remote URL:
<https://www.nrcs.usda.gov/sites/default/files/2022-10/MLRA_52_2022.zip>.
Therefore, most additional `fetchSDA_spatial()` arguments are *not*
currently supported for the MLRA geometry source. In the future a
`mlrapolygon` table may be added to SDA (analogous to `mupolygon` and
`sapolygon`), and the function will be updated accordingly at that time.

## Author

Andrew G. Brown, Dylan E. Beaudette

## Examples

``` r
# \donttest{
 
    # get spatial data for a single mukey
    single.mukey <- try(fetchSDA_spatial(x = "2924882"))
#> Using 1 chunks...
#> Chunk #1 completed (n = 1; 0.1 secs)
#> Done in 0.1 secs; mean/chunk: 0.1 secs; mean/symbol: 0.1 secs.

    # demonstrate fetching full extent (multi-mukey) of national musym
    full.extent.nmusym <- try(fetchSDA_spatial(x = "2x8l5", by = "nmusym"))
#> Using 1 chunks...
#> Chunk #1 completed (n = 3; 0.5 secs)
#> Done in 0.7 secs; mean/chunk: 0.5 secs; mean/symbol: 0.22 secs.

    # compare extent of nmusym to single mukey within it
    if (!inherits(single.mukey, 'try-error') && 
        !inherits(full.extent.nmusym, 'try-error')) {
        
        if (requireNamespace("sf")) {
      
         plot(sf::st_geometry(full.extent.nmusym), col = "RED", border = 0)
         plot(sf::st_geometry(single.mukey), add = TRUE, col = "BLUE", border = 0)
       
        }
        
    }


    # demo adding a field (`muname`) to attribute table of result
    head(try(fetchSDA_spatial(x = "2x8l5", by="nmusym", add.fields="muname")))
#> Using 1 chunks...
#> Chunk #1 completed (n = 3; 0.4 secs)
#> Done in 0.6 secs; mean/chunk: 0.4 secs; mean/symbol: 0.18 secs.
#> Simple feature collection with 6 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -120.9763 ymin: 37.96617 xmax: -120.9297 ymax: 38.15251
#> Geodetic CRS:  WGS 84
#>    mukey areasymbol nationalmusym                                        muname
#> 1 462101      CA077         2x8l5 Pentz-Bellota complex, 2 to 15 percent slopes
#> 2 462101      CA077         2x8l5 Pentz-Bellota complex, 2 to 15 percent slopes
#> 3 462101      CA077         2x8l5 Pentz-Bellota complex, 2 to 15 percent slopes
#> 4 462101      CA077         2x8l5 Pentz-Bellota complex, 2 to 15 percent slopes
#> 5 462101      CA077         2x8l5 Pentz-Bellota complex, 2 to 15 percent slopes
#> 6 462101      CA077         2x8l5 Pentz-Bellota complex, 2 to 15 percent slopes
#>                             geom
#> 1 POLYGON ((-120.9358 37.9796...
#> 2 POLYGON ((-120.9664 38.1111...
#> 3 POLYGON ((-120.956 38.11547...
#> 4 POLYGON ((-120.951 38.1185,...
#> 5 POLYGON ((-120.969 38.14764...
#> 6 POLYGON ((-120.9327 37.9670...
# }
```

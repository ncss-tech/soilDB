# Search full text of Official Series Description on SoilWeb

This is the R interface to [OSD search by
Section](https://casoilresource.lawr.ucdavis.edu/osd-search/) and [OSD
Search](https://casoilresource.lawr.ucdavis.edu/osd-search/search-entire-osd.php)
APIs provided by SoilWeb.

OSD records are searched with the [PostgreSQL fulltext
indexing](https://www.postgresql.org/docs/9.5/textsearch.html) and query
system ([syntax
details](https://www.postgresql.org/docs/9.5/datatype-textsearch.html)).
Each search field (except for the "brief narrative" and MLRA)
corresponds with a section header in an OSD. The results may not include
every OSD due to formatting errors and typos. Results are scored based
on the number of times search terms match words in associated sections.

## Usage

``` r
OSDquery(
  everything = NULL,
  mlra = "",
  taxonomic_class = "",
  typical_pedon = "",
  brief_narrative = "",
  ric = "",
  use_and_veg = "",
  competing_series = "",
  geog_location = "",
  geog_assoc_soils = ""
)
```

## Arguments

- everything:

  search entire OSD text (default is NULL), `mlra` may also be
  specified, all other arguments are ignored

- mlra:

  a comma-delimited string of MLRA to search ('17,18,22A'), see Details
  section below

- taxonomic_class:

  search family level classification

- typical_pedon:

  search typical pedon section

- brief_narrative:

  search brief narrative

- ric:

  search range in characteristics section

- use_and_veg:

  search use and vegetation section

- competing_series:

  search competing series section

- geog_location:

  search geographic setting section

- geog_assoc_soils:

  search geographically associated soils section

## Value

a `data.frame` object containing soil series names that match patterns
supplied as arguments.

## Details

The `mlra` argument must be combined with another argument in order to
become active. For example, search for series with 5GY hues in the
"typical pedon" section, but limit to just MLRA 18:
`OSDquery(mlra = '18', typical_pedon = '5GY')`.

See [this webpage](https://casoilresource.lawr.ucdavis.edu/osd-search/)
for more information.

- family level taxa are derived from SC database, not parsed OSD records

- MLRA are derived via spatial intersection (SSURGO x MLRA polygons)

- MLRA-filtering is only possible for series used in the current SSURGO
  snapshot (component name)

- logical AND: &

- logical OR: \|

- wildcard, e.g. rhy-something rhy:\*

- search terms with spaces need doubled single quotes: ”san joaquin”

- combine search terms into a single expression: (grano:\* \| granite)

Related documentation can be found in the following tutorials

- [overview of all soil series query
  functions](http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.md)

- [competing soil
  series](https://ncss-tech.github.io/AQP/soilDB/competing-series.html)

- [siblings](https://ncss-tech.github.io/AQP/soilDB/siblings.html)

## Note

SoilWeb maintains a snapshot of the Official Series Description data.

## References

USDA-NRCS OSD search tools: <https://soilseries.sc.egov.usda.gov/>

## See also

[`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md),
[`siblings()`](http://ncss-tech.github.io/soilDB/reference/siblings.md),
[`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)

## Author

D.E. Beaudette

## Examples

``` r
# \donttest{
  # find all series that list Pardee as a geographically associated soil.
  s <- OSDquery(geog_assoc_soils = 'pardee')

  # get data for these series
  x <- fetchOSD(s$series, extended = TRUE, colorState = 'dry')

  # simple figure
  par(mar=c(0,0,1,1))
  aqp::plotSPC(x$SPC)

# }
```

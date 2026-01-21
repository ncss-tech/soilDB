# Search full text of Official Series Description on SoilWeb

This is the R interface to [OSD search by
Section](https://casoilresource.lawr.ucdavis.edu/osd-search/) and [OSD
Search](https://casoilresource.lawr.ucdavis.edu/osd-search/search-entire-osd.php)
APIs provided by SoilWeb.

OSD records are searched with the [PostgreSQL fulltext
indexing](https://www.postgresql.org/docs/current/textsearch.html) and
query system ([syntax
details](https://www.postgresql.org/docs/current/textsearch-controls.html)).
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
  geog_assoc_soils = "",
  remarks = ""
)
```

## Arguments

- everything:

  search entire OSD text (default is NULL), `mlra` may also be
  specified, all other arguments are ignored

- mlra:

  a comma-delimited string of MLRA to search ('17,18,22A'), see Details

- taxonomic_class:

  search family level classification, see Details

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

- remarks:

  search remarks section (typically contains diagnostic horizons /
  features)

## Value

a `data.frame` object containing soil series names that match patterns
supplied as arguments.

## Details

Queries including the `taxonomic_class` argument make use of the Soil
Classification database, not fulltext search of OSD records. Queries
including the `mlra` argument make use of a SoilWeb data source based on
spatial intersection (SSURGO x MLRA polygons), updated quarterly. MLRA
queries are only possible for those soil series used in the current
SSURGO snapshot.

The `mlra` argument must be combined with another argument in order to
become active. For example, search for series with "5GY" hues in the
"typical pedon" section, but limit to just MLRA 18:
`OSDquery(mlra = '18', typical_pedon = '5GY')`.

### Syntax Notes:

The PostgreSQL fulltext query syntax is complex, but many common text
search concepts are familiar:

- logical AND: `&`

- logical OR: `|`

- wildcard, e.g. rhy-something: `rhy:*`

- search terms with spaces need doubled single quotes: `''san joaquin''`

- combine search terms into a single expression: `(grano:* | granite)`

### Examples

Strategies for searching entire OSD records:

- `iowa & smectitic & verti:* & Cg & ! saturated`

- `iowa & smectitic & verti:* & Cg & terrace`

- `(sulfi:* | sulfa:*) & aq:*`

- `Coarse-loamy & mixed & active & thermic & Mollic & Haploxeralfs`

- `sierra & nevada & (meta:* | metamorphic) & xer:* & thermic & lithic`

- `sierra & nevada & foothill & (grano:* | granite) & thermic`

- `rhyo:* & tuff:* & California & thermic`

- `paralithic & thermic & !mesic & mollic & epipedon`

- `(gypsum | gyp:*) (MLRA: 15,17)`

- `flood & plains & toe & slope`

Strategies for search OSD fields:

- `taxonomic_class = 'duri:* & thermic'`: family level classification
  contains "duri-" prefix and "thermic"

- `typical_pedon = 'cobbly & ashy & silt & loam'`: "cobbly, ashy, silt,
  loam", any horizon

- `geog_location = 'strath & terrace'`: "strath" and "terrace" in
  geographic setting narrative

Related documentation can be found in the following tutorials

- [overview of all soil series query
  functions](http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.md)

- [competing soil
  series](https://ncss-tech.github.io/AQP/soilDB/competing-series.html)

- [siblings](https://ncss-tech.github.io/AQP/soilDB/siblings.html)

## Note

SoilWeb maintains a snapshot of the Official Series Description data,
updated quarterly.

## References

USDA-NRCS OSD search tools: <https://soilseries.sc.egov.usda.gov/>

## See also

[`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md),
[`siblings()`](http://ncss-tech.github.io/soilDB/reference/siblings.md)

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
  par(mar = c(0,0,1,1))
  aqp::plotSPC(x$SPC)

# }
```

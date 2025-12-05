# Make Ecological Dynamics Interpretive Tool (EDIT) web services URL

Construct a URL for Ecological Dynamics Interpretive Tool (EDIT) web
services (`https://edit.jornada.nmsu.edu/services/...`) to return PDF,
TXT or JSON results.

## Usage

``` r
make_EDIT_service_URL(
  src = c("descriptions", "downloads", "plant-community-tables", "models", "keys"),
  catalog = c("esd", "esg"),
  geoUnit = NULL,
  ecoclass = NULL,
  landuse = NULL,
  state = NULL,
  community = NULL,
  key = NULL,
  endpoint = NULL,
  querystring = NULL
)
```

## Arguments

- src:

  One of: `descriptions`, `downloads`, `plant-community-tables`,
  `models`, `keys`

- catalog:

  Catalog ID. One of: `esd` or `esg`

- geoUnit:

  Geographic unit ID. For example: `022A`

- ecoclass:

  Ecological class ID. For example: `F022AX101CA`

- landuse:

  Optional: Used only for `src = "plant-community-tables"`

- state:

  Optional: Used only for `src = "plant-community-tables"`

- community:

  Optional: Used only for `src = "plant-community-tables"`

- key:

  Optional: Key number. All keys will be returned if not specified.

- endpoint:

  Optional: Specific endpoint e.g. `overview.json`, `class-list.json`,
  `soil-features.json`

- querystring:

  Optional: Additional request parameters specified as a query string
  `?param1=value&param2=value`.

## Value

A character vector containing URLs with specified parameters. This
function is vectorized.

## Details

See the official EDIT developer resources to see which endpoints are
available for Ecological Site Description (ESD) or Ecological Site Group
(ESG) catalogs:

## See also

get_EDIT_ecoclass_by_geoUnit

## Examples

``` r
# url for all geoUnit keys as PDF
make_EDIT_service_URL(src = "descriptions",
                      catalog = "esd",
                      geoUnit = "039X")
#> [1] "https://edit.jornada.nmsu.edu/services/descriptions/esd/039X.pdf"

# url for a single key within geoUnit as PDF
make_EDIT_service_URL(src = "descriptions",
                      catalog = "esd",
                      geoUnit = "039X",
                      key = "1")
#> [1] "https://edit.jornada.nmsu.edu/services/descriptions/esd/039X/1.pdf"

# query for "full" description in JSON
desc <-  make_EDIT_service_URL(src = "descriptions",
                               catalog = "esd",
                               geoUnit = "039X",
                               endpoint = "R039XA109AZ.json")

# query for "overview"
desc_ov <- make_EDIT_service_URL(src = "descriptions",
                                 catalog = "esd",
                                 geoUnit = "039X",
                                 ecoclass = "R039XA109AZ",
                                 endpoint = "overview.json")

# query for specific section, e.g. "water features"
desc_wf <- make_EDIT_service_URL(src = "descriptions",
                                 catalog = "esd",
                                 geoUnit = "039X",
                                 ecoclass = "R039XA109AZ",
                                 endpoint = "water-features.json")

# construct the URLs -- that is a query essentially
# then download the result with read_json

#full <- jsonlite::read_json(desc)
#overview <- jsonlite::read_json(desc_ov)
#waterfeature <- jsonlite::read_json(desc_wf)
```

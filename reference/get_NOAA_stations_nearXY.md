# Get NOAA station data near a given latitude and longitude

Query the NOAA API to get station data (limit 1000 records) near a
point. Default extent is plus or minus 0.5 degrees (bounding box) (with
`bbox = 1`) around the specified point \[lat, lng\].

In order to use this function, you must obtain an API token from this
website: https://www.ncdc.noaa.gov/cdo-web/token

## Usage

``` r
get_NOAA_stations_nearXY(lat, lng, apitoken, bbox = 1, crs = "EPSG:4326")
```

## Arguments

- lat:

  Latitude or Y coordinate in `crs`

- lng:

  Longitude or X coordinate in `crs`

- apitoken:

  API key token for NOAA NCDC web service

- bbox:

  Optional: Dimension of the bounding box centered at `lat`, `lng`.

- crs:

  Coordinate Reference System. Default `"EPSG:4326"`

## Value

data.frame containing station information for all stations within a
bounding box around `lat`, `lng`.

## Examples

``` r
## in order to use this function, you must obtain an API token from this website:
##  https://www.ncdc.noaa.gov/cdo-web/token

# stations <- get_NOAA_stations_nearXY(lat = 37, lng = -120,
#                                      apitoken = "yourtokenhere")
```

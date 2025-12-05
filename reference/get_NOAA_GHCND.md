# Get Global Historical Climatology Network Daily (GHCND) data from NOAA API

Obtain daily climatic summary data for a set of station IDs, years, and
datatypes.

Note that typically results from the NOAA API are limited to 1000
records. However, by "chunking" up data into individual
station*year*datatypeid combinations, record results generally do not
exceed 365 records for daily summaries.

In order to use this function, you must obtain an API token from this
website: https://www.ncdc.noaa.gov/cdo-web/token

## Usage

``` r
get_NOAA_GHCND(stations, years, datatypeids, apitoken)
```

## Arguments

- stations:

  Station ID (e.g. `GHCND:USC00388786`)

- years:

  One or more years (e.g. 2017:2020)

- datatypeids:

  One or more NOAA GHCND data type IDs (e.g `c("PRCP","SNOW")`)

- apitoken:

  API key token for NOAA NCDC web services
  (https://www.ncdc.noaa.gov/cdo-web/token)

## Value

A data.frame containing the GHCND data requested (limit 1000 records)

## Examples

``` r
#' ## in order to use this function, you must obtain an API token from this website:
##  https://www.ncdc.noaa.gov/cdo-web/token

# get_NOAA_GHCND(c("GHCND:USC00388786", "GHCND:USC00388787"),
#                years = 2017:2020,
#                datatypeids = c("PRCP","SNOW"),
#                apitoken = "yourtokenhere")
```

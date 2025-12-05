# Get data from Henry Mount Soil Temperature and Water Database

This function is a front-end to the REST query functionality of the
Henry Mount Soil Temperature and Water Database.

## Usage

``` r
summarizeSoilTemperature(soiltemp.data)

month2season(x)

fetchHenry(
  what = "all",
  usersiteid = NULL,
  project = NULL,
  sso = NULL,
  gran = "day",
  start.date = NULL,
  stop.date = NULL,
  pad.missing.days = TRUE,
  soiltemp.summaries = TRUE,
  tz = ""
)
```

## Arguments

- soiltemp.data:

  A `data.frame` containing soil temperature data

- x:

  character vector containing month abbreviation e.g.
  `c('Jun', 'Dec', 'Sep')`

- what:

  type of data to return: 'sensors': sensor metadata only \| 'soiltemp':
  sensor metadata + soil temperature data \| 'soilVWC': sensor
  metadata + soil moisture data \| 'airtemp': sensor metadata + air
  temperature data \| 'waterlevel': sensor metadata + water level data
  \|'all': sensor metadata + all sensor data

- usersiteid:

  (optional) filter results using a NASIS user site ID

- project:

  (optional) filter results using a project ID

- sso:

  (optional) filter results using a soil survey office code

- gran:

  data granularity: "hour" (if available), "day", "week", "month",
  "year"; returned data are averages

- start.date:

  (optional) starting date filter

- stop.date:

  (optional) ending date filter

- pad.missing.days:

  should missing data ("day" granularity) be filled with NA? see details

- soiltemp.summaries:

  should soil temperature ("day" granularity only) be summarized? see
  details

- tz:

  Used for custom timezone. Default `""` is current locale

## Value

a list containing:

- sensors:

  a `sf` `data.frame` object containing site-level information

- soiltemp:

  a `data.frame` object containing soil temperature timeseries data

- soilVWC:

  a `data.frame` object containing soil moisture timeseries data

- airtemp:

  a `data.frame` object containing air temperature timeseries data

- waterlevel:

  a `data.frame` object containing water level timeseries data

## Details

Filling missing days with NA is useful for computing and index of how
complete the data are, and for estimating (mostly) unbiased MAST and
seasonal mean soil temperatures. Summaries are computed by first
averaging over Julian day, then averaging over all days of the year
(MAST) or just those days that occur within "summer" or "winter". This
approach makes it possible to estimate summaries in the presence of
missing data. The quality of summaries should be weighted by the number
of "functional years" (number of years with non-missing data after
combining data by Julian day) and "complete years" (number of years of
data with \>= 365 days of non-missing data).

See:

- [Henry Mount Soil Climate
  Database](http://soilmap2-1.lawr.ucdavis.edu/henry/)

- [`fetchHenry`
  Tutorial](http://ncss-tech.github.io/AQP/soilDB/Henry-demo.md)

## Note

This function and the back-end database are very much a work in
progress.

## See also

[`fetchSCAN`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md)

## Author

D.E. Beaudette

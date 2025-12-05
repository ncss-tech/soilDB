# Get Daily Climate Data from USDA-NRCS SCAN (Soil Climate Analysis Network) Stations

Query soil/climate data from USDA-NRCS SCAN Stations.

## Usage

``` r
fetchSCAN(
  site.code = NULL,
  year = NULL,
  report = "SCAN",
  timeseries = c("Daily", "Hourly"),
  tz = "US/Central",
  ...
)

SCAN_sensor_metadata(site.code)

SCAN_site_metadata(site.code = NULL)
```

## Arguments

- site.code:

  a vector of site codes. If `NULL` `SCAN_site_metadata()` returns
  metadata for all SCAN sites and no sensor data.

- year:

  a vector of years

- report:

  report name, single value only; default `'SCAN'`, other example
  options include individual sensor codes, e.g. `'SMS'` for Soil
  Moisture Storage, `'TEMP'` for temperature

- timeseries:

  either `'Daily'` or `'Hourly'`

- tz:

  Target timezone to convert `datetime` columns of results. Default:
  `"US/Central"`.

- ...:

  additional arguments. May include `intervalType`, `format`, `sitenum`,
  `interval`, `year`, `month`. Presence of additional arguments bypasses
  default batching functionality provided in the function and submits a
  'raw' request to the API form.

## Value

a `list` of `data.frame` objects, where each element name is a sensor
type, plus a `metadata` table; different `report` types change the types
of sensor data returned. `SCAN_sensor_metadata()` and
`SCAN_site_metadata()` return a `data.frame`. `NULL` on bad request.

## Details

Possible above and below ground sensor types include: 'SMS' (soil
moisture), 'STO' (soil temperature), 'SAL' (salinity), 'TAVG' (daily
average air temperature), 'TMIN' (daily minimum air temperature), 'TMAX'
(daily maximum air temperature), 'PRCP' (daily precipitation), 'PREC'
(daily precipitation), 'SNWD' (snow depth), 'WTEQ' (snow water
equivalent),'WDIRV' (wind direction), 'WSPDV' (wind speed), 'LRADT'
(solar radiation/langley total).

This function converts below-ground sensor depth from inches to cm. All
temperature values are reported as degrees C. Precipitation, snow depth,
and snow water content are reported as *inches*.

The `datetime` column in sensor data results is converted to the target
time zone specified in `tz` argument, the default is `"US/Central"`. Use
`tz = "UTC"` (or other
[`OlsonNames()`](https://rdrr.io/r/base/timezones.html) that do not use
daylight savings, e.g. `"US/Arizona"`) to avoid having a mix of time
offsets due to daylight savings time.

### SCAN Sensors

All Soil Climate Analysis Network (SCAN) sensor measurements are
reported hourly.

|                          |                                                                                                            |                                                |
|--------------------------|------------------------------------------------------------------------------------------------------------|------------------------------------------------|
| Element Measured         | Sensor Type                                                                                                | Precision                                      |
| Air Temperature          | Shielded thermistor                                                                                        | 0.1 degrees C                                  |
| Barometric Pressure      | Silicon capacitive pressure sensor                                                                         | 1%                                             |
| Precipitation            | Storage-type gage or tipping bucket                                                                        | Storage: 0.1 inches;                           |
| Relative Humidity        | Thin film capacitance-type sensor                                                                          | 1%                                             |
| Snow Depth               | Sonic sensor (not on all stations)                                                                         | 0.5 inches                                     |
| Snow Water Content       | Snow pillow device and a pressure transducer (not on all stations)                                         | 0.1 inches                                     |
| Soil Moisture            | Dielectric constant measuring device. Typical measurements are at 2", 4", 8", 20", and 40" where possible. | 0.50%                                          |
| Soil Temperature         | Encapsulated thermistor. Typical measurements are at 2", 4", 8", 20", and 40" where possible.              | 0.1 degrees C                                  |
| Solar Radiation          | Pyranometer                                                                                                | 0.01 watts per meter                           |
| Wind Speed and Direction | Propellor-type anemometer                                                                                  | Speed: 0.1 miles per hour; Direction: 1 degree |

### SNOTEL Sensors

All Snow Telemetry (SNOTEL) sensor measurements are reported daily.

|                          |                                                                                                            |                                                  |
|--------------------------|------------------------------------------------------------------------------------------------------------|--------------------------------------------------|
| Element Measured         | Sensor Type                                                                                                | Precision                                        |
| Air Temperature          | Shielded thermistor                                                                                        | 0.1 degrees C                                    |
| Barometric Pressure      | Silicon capacitive pressure sensor                                                                         | 1%                                               |
| Precipitation            | Storage-type gage or tipping bucket                                                                        | Storage: 0.1 inches; Tipping bucket: 0.01 inches |
| Relative Humidity        | Thin film capacitance-type sensor                                                                          | 1%                                               |
| Snow Depth               | Sonic sensor                                                                                               | 0.5 inches                                       |
| Snow Water Content       | Snow pillow device and a pressure transducer                                                               | 0.1 inches                                       |
| Soil Moisture            | Dielectric constant measuring device. Typical measurements are at 2", 4", 8", 20", and 40" where possible. | 0.50%                                            |
| Soil Temperature         | Encapsulated thermistor. Typical measurements are at 2", 4", 8", 20", and 40" where possible.              | 0.1 degrees C                                    |
| Solar Radiation          | Pyranometer                                                                                                | 0.01 watts per meter                             |
| Wind Speed and Direction | Propellor-type anemometer                                                                                  | Speed: 0.1 miles per hour; Direction: 1 degree   |

See the [fetchSCAN
tutorial](http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.md) for
additional usage and visualization examples.

## References

See the [Soil Climate Analysis
Network](https://www.nrcs.usda.gov/resources/data-and-reports/soil-climate-analysis-network)
home page for more information on the SCAN program, and links to other
associated programs such as SNOTEL, at the National Weather and Climate
Center. You can get information on available web services, as well as
interactive maps of snow water equivalent, precipitation and streamflow.

## Author

D.E. Beaudette, A.G. Brown, J.M. Skovlin

## Examples

``` r
if (FALSE) { # \dontrun{
    # get data
    x <- try(fetchSCAN(site.code = c(356, 2072), year = c(2015, 2016)))
    str(x, 1)

    # get sensor metadata
    m <- SCAN_sensor_metadata(site.code = c(356, 2072))
    m
    
    # get site metadata
    m <- SCAN_site_metadata(site.code = c(356, 2072))
    m
    
    # # get hourly data (warning, result is large ~11MB) 
    # x <- try(fetchSCAN(site.code = c(356, 2072), 
    #                    year = 2015, 
    #                    timeseries = "Hourly"))
    #
    # # data are in US/Central time, standard or daylight savings time based on day of year
    # unique(format(x$SMS$datetime, '%Z'))
    #
    # # the site metadata indicate timeseries data time zone (dataTimeZone)
    # # for site 356 the timezone is offset of 8 hours behind UTC
    #
    # # to obtain all datetime data with a consistent offset use ETC GMT offset
    # # e.g. "Etc/GMT+8". note the sign is inverted ("GMT+8" vs. `dataTimeZone=-8`)
    # x <- try(fetchSCAN(site.code = c(356, 2072), 
    #          year = 2015, 
    #          timeseries = "Hourly", 
    #          tz = "Etc/GMT+8"))
    
} # }
```

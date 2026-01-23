# Compute Water Day and Year

Compute "water" day and year, based on the end of the typical or legal
dry season. This is September 30 in California. This function accounts
for leap years.

## Usage

``` r
waterDayYear(d, end = "09-30", format = "%Y-%m-%d", tz = "UTC")
```

## Arguments

- d:

  anything the can be safely converted to `POSIXlt`

- end:

  "MM-DD" notation for end of water year

- format:

  Used in POSIXlt conversion. Default `"%Y-%m-%d"`

- tz:

  Used in POSIXlt conversion for custom timezone. Default is `"UTC"`

## Value

A `data.frame` object with the following

- wy:

  the "water year"

- wd:

  the "water day"

## Author

D.E. Beaudette

## Examples

``` r
# try it
waterDayYear('2019-01-01')
#>     wy wd
#> 1 2019 93
```

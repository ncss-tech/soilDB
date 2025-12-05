# Get Vegetation Data from an NPS PLOTS Database

Used to extract species, stratum, and cover vegetation data from a
backend NPS PLOTS Database. Currently works for any Microsoft Access
database with an .mdb file format.

## Usage

``` r
get_veg_from_NPS_PLOTS_db(dsn)
```

## Arguments

- dsn:

  file path to the NPS PLOTS access database on your system.

## Value

A data.frame with vegetation data in a long format with linkage to NRCS
soil pedon data via the site_id key field.

## Note

This function currently only works on Windows.

## Author

Jay M. Skovlin

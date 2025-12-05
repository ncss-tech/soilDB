# Get a SoilProfileCollection from a PedonPC v.5 database

Fetch commonly used site/horizon data from a version 5.x PedonPC
database, return as a SoilProfileCollection object.

## Usage

``` r
fetchPedonPC(dsn)

getHzErrorsPedonPC(dsn, strict = TRUE)
```

## Arguments

- dsn:

  The path to a PedonPC version 6.x database

- strict:

  Use "strict" horizon error checking? Default: `TRUE`

## Value

a SoilProfileCollection class object

## Note

This function attempts to do most of the boilerplate work when
extracting site/horizon data from a PedonPC or local NASIS database.
Pedons that have errors in their horizonation are excluded from the
returned object, however, their IDs are printed on the console. See
`getHzErrorsPedonPC` for a simple approach to identifying pedons with
problematic horizonation. Records from the 'taxhistory' table are
selected based on 1) most recent record, or 2) record with the least
amount of missing data.

## See also

[`get_hz_data_from_pedon_db`](http://ncss-tech.github.io/soilDB/reference/get_hz_data_from_pedon_db.md)

## Author

D. E. Beaudette and J. M. Skovlin

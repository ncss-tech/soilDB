# Get Horizon Data from a PedonPC Database

Get horizon-level data from a PedonPC database.

## Usage

``` r
get_hz_data_from_pedon_db(dsn)
```

## Arguments

- dsn:

  The path to a 'pedon.mdb' database.

## Value

A data.frame.

## Note

NULL total rock fragment values are assumed to represent an *absence* of
rock fragments, and set to 0.

## See also

[`get_colors_from_pedon_db`](http://ncss-tech.github.io/soilDB/reference/get_colors_from_pedon_db.md),
[`get_site_data_from_pedon_db`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_pedon_db.md)

## Author

Dylan E. Beaudette and Jay M. Skovlin

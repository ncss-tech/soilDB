# Simplify Color Data by ID

Simplify multiple Munsell color observations associated with each
horizon.

This function is mainly intended for the processing of NASIS
pedon/horizon data which may or may not contain multiple colors per
horizon/moisture status combination. `simplifyColorData` will "mix"
multiple colors associated with horizons in `d`, according to IDs
specified by `id.var`, using "weights" (area percentages) specified by
the `wt` argument.

Note that this function doesn't actually simulate the mixture of
pigments on a surface, rather, "mixing" is approximated via weighted
average in the CIELAB colorspace.

The `simplifyColorData` function can be applied to data sources other
than NASIS by careful use of the `id.var` and `wt` arguments. However,
`d` must contain Munsell colors split into columns named "colorhue",
"colorvalue", and "colorchroma". In addition, the moisture state ("Dry"
or "Moist") must be specified in a column named "colormoistst".

Examples:

- [KSSL data](http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.md)

- [soil color mixing
  tutorial](http://ncss-tech.github.io/AQP/soilDB/mixing-soil-color-data.md)

## Usage

``` r
simplifyColorData(d, id.var = "phiid", wt = "colorpct", bt = FALSE)
```

## Arguments

- d:

  a `data.frame` object, typically returned from NASIS, see details

- id.var:

  character vector with the name of the column containing an ID that is
  unique among all horizons in `d`

- wt:

  a character vector with the name of the column containing color
  weights for mixing

- bt:

  logical, should the mixed sRGB representation of soil color be
  transformed to closest Munsell chips? This is performed by
  [`aqp::col2Munsell()`](https://ncss-tech.github.io/aqp/reference/col2Munsell.html)

## Author

D.E. Beaudette

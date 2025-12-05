# Estimate color mixtures using weighted average of CIELAB color coordinates

Estimate color mixtures using weighted average of CIELAB color
coordinates

## Usage

``` r
estimateColorMixture(x, wt = "pct", backTransform = FALSE)
```

## Arguments

- x:

  data.frame, typically from NASIS containing at least CIE LAB ('L',
  'A', 'B') and some kind of weight

- wt:

  numeric. fractional weights, usually area of horizon face

- backTransform:

  logical, should the mixed sRGB representation of soil color be
  transformed to closest Munsell chips? This is performed by
  [`aqp::col2Munsell()`](https://ncss-tech.github.io/aqp/reference/col2Munsell.html)
  default: `FALSE`

## Value

A data.frame containing estimated color mixture

## Note

See
[`aqp::mixMunsell()`](https://ncss-tech.github.io/aqp/reference/mixMunsell.html)
for a more realistic (but slower) simulation of subtractive mixing of
pigments. An efficient replacement for this function (wt. mean in CIELAB
coordinates) is implemented in
`aqp::mixMunsell(..., mixingMethod = 'estimate')`.

## Author

D.E. Beaudette

# Estimate Soil Temperature Regime

Estimate soil temperature regime (STR) based on mean annual soil
temperature (MAST), mean summer temperature (MSST), mean winter soil
temperature (MWST), presence of O horizons, saturated conditions, and
presence of permafrost. Several assumptions are made when O horizon or
saturation are undefined.

## Usage

``` r
estimateSTR(
  mast,
  mean.summer,
  mean.winter,
  O.hz = NA,
  saturated = NA,
  permafrost = FALSE
)
```

## Arguments

- mast:

  vector of mean annual soil temperature (deg C)

- mean.summer:

  vector of mean summer soil temperature (deg C)

- mean.winter:

  vector of mean winter soil temperature (deg C)

- O.hz:

  logical vector of O horizon presence / absence

- saturated:

  logical vector of seasonal saturation

- permafrost:

  logical vector of permafrost presence / absence

## Value

Vector of soil temperature regimes.

## Details

[Soil Temperature Regime Evaluation
Tutorial](http://ncss-tech.github.io/AQP/soilDB/STR-eval.md)

## References

Soil Survey Staff. 2015. Illustrated guide to soil taxonomy. U.S.
Department of Agriculture, Natural Resources Conservation Service,
National Soil Survey Center, Lincoln, Nebraska.

## See also

[`STRplot`](http://ncss-tech.github.io/soilDB/reference/STRplot.md)

## Author

D.E. Beaudette

## Examples

``` r
# simple example
estimateSTR(mast=17, mean.summer = 22, mean.winter = 12)
#> [1] thermic
#> 10 Levels: gelic cryic frigid isofrigid mesic isomesic thermic ... isohyperthermic

```

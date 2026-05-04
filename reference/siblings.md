# Get "siblings" and "cousins" for a given soil series

Look up siblings and cousins for a given soil series from the current
fiscal year SSURGO snapshot via SoilWeb.

The siblings of any given soil series are defined as those soil
components (major and minor) that share a parent map unit with the named
series (as a major component). Component names are filtered using a
snapshot of the Soil Classification database to ensure that only valid
soil series names are included. Cousins are siblings of siblings. Data
are sourced from SoilWeb which maintains a copy of the current SSURGO
snapshot. Visualizations of soil "siblings"-related concepts can be
found in the "Sibling Summary" tab of Soil Data Explorer app:
<https://casoilresource.lawr.ucdavis.edu/sde/>.

Additional resources:

- [Soil Series Query
  Functions](http://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.md)

- [Soil "Siblings"
  Tutorial](http://ncss-tech.github.io/AQP/soilDB/siblings.md)

- [SSSA 2019 Presentation - Mapping Soilscapes Using Soil Co-Occurrence
  Networks](http://ncss-tech.github.io/AQP/presentations/beaudette-soil-networks-2019-www.pdf)

## Usage

``` r
siblings(s, only.major = FALSE, component.data = FALSE, cousins = FALSE)
```

## Arguments

- s:

  character vector, the name of a single soil series, case-insensitive.

- only.major:

  logical, should only return siblings that are major components

- component.data:

  logical, should component data for siblings (and optionally cousins)
  be returned?

- cousins:

  logical, should siblings-of-siblings (cousins) be returned?

## Value

A `list` containing:

- sib: `data.frame` containing siblings, major component flag, and
  number of co-occurrences

- sib.data: `data.frame` containing sibling component data (only when
  `component.data = TRUE`)

- cousins: `data.frame` containing cousins, major component flag, and
  number of co-occurrences (only when `cousins = TRUE`)

- cousin.data: `data.frame` containing cousin component data (only when
  `cousins = TRUE, component.data = TRUE`)

## References

O'Geen, A., Walkinshaw, M. and Beaudette, D. (2017), SoilWeb: A
Multifaceted Interface to Soil Survey Information. Soil Science Society
of America Journal, 81: 853-862.
[doi:10.2136/sssaj2016.11.0386n](https://doi.org/10.2136/sssaj2016.11.0386n)

## See also

[OSDquery](http://ncss-tech.github.io/soilDB/reference/OSDquery.md),
siblings,
[fetchOSD](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)

## Author

D.E. Beaudette

## Examples

``` r
# \donttest{
    # basic usage
    x <- siblings('zook')
    x$sib
#>    series      sibling majcompflag  n
#> 1    zook       Olmitz        TRUE 17
#> 2    zook        Vigar        TRUE 12
#> 3    zook       Vesser        TRUE 10
#> 4    zook          Ely        TRUE 10
#> 5    zook      Excello        TRUE  9
#> 6    zook         Colo        TRUE  8
#> 7    zook      Nodaway        TRUE  5
#> 8    zook          Zoe        TRUE  3
#> 9    zook Mt. Sterling        TRUE  3
#> 10   zook        Kezan        TRUE  2
#> 11   zook        Clamo        TRUE  2
#> 12   zook     Humeston        TRUE  1
#> 13   zook         Klum        TRUE  1
#> 14   zook       Quiver        TRUE  1
#> 15   zook         Colo       FALSE 73
#> 16   zook       Wabash       FALSE 59
#> 17   zook     Chequest       FALSE 45
#> 18   zook      Nodaway       FALSE 45
#> 19   zook     Humeston       FALSE 27
#> 20   zook       Arbela       FALSE 23
#> 21   zook      Ackmore       FALSE 18
#> 22   zook      Dockery       FALSE 16
#> 23   zook       Bremer       FALSE 16
#> 24   zook         Lamo       FALSE 15
#> 25   zook       Landes       FALSE 14
#> 26   zook     Kennebec       FALSE 13
#> 27   zook        Shell       FALSE 12
#> 28   zook         Napa       FALSE 12
#> 29   zook       Olmitz       FALSE  9
#> 30   zook       Coland       FALSE  9
#> 31   zook      Sawmill       FALSE  9
#> 32   zook       Vesser       FALSE  9
#> 33   zook     Blackoar       FALSE  5
#> 34   zook       Nishna       FALSE  5
#> 35   zook       Eudora       FALSE  3
#> 36   zook        Chase       FALSE  3
#> 37   zook      Reading       FALSE  3
#> 38   zook       Judson       FALSE  2
#> 39   zook        Calco       FALSE  2
#> 40   zook       Quiver       FALSE  2
#> 41   zook     Clarinda       FALSE  1
#> 42   zook          Ely       FALSE  1
    
    # restrict to siblings that are major components
    # e.g. the most likely siblings
    x <- siblings('zook', only.major = TRUE)
    x$sib
#>    series      sibling majcompflag  n
#> 1    zook       Olmitz        TRUE 17
#> 2    zook        Vigar        TRUE 12
#> 3    zook       Vesser        TRUE 10
#> 4    zook          Ely        TRUE 10
#> 5    zook      Excello        TRUE  9
#> 6    zook         Colo        TRUE  8
#> 7    zook      Nodaway        TRUE  5
#> 8    zook          Zoe        TRUE  3
#> 9    zook Mt. Sterling        TRUE  3
#> 10   zook        Kezan        TRUE  2
#> 11   zook        Clamo        TRUE  2
#> 12   zook     Humeston        TRUE  1
#> 13   zook         Klum        TRUE  1
#> 14   zook       Quiver        TRUE  1
# }
```

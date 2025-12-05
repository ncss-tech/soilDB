# Work with NASIS Choice Lists

Create (ordered) factors and interchange between choice names, values
and labels for lists of input vectors.

## Usage

``` r
NASISChoiceList(
  x = NULL,
  colnames = names(x),
  what = "ColumnPhysicalName",
  choice = c("ChoiceName", "ChoiceValue", "ChoiceLabel"),
  obsolete = FALSE,
  factor = TRUE,
  droplevels = FALSE,
  ordered = TRUE,
  simplify = TRUE,
  dsn = NULL
)
```

## Arguments

- x:

  A named list of vectors to use as input for NASIS Choice List lookup

- colnames:

  vector of values of the column specified by `what`. E.g.
  `colnames="texcl"` for `what="ColumnPhysicalName"`. Default:
  `names(x)` (if x is named)

- what:

  passed to
  [`get_NASIS_column_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_metadata.md);
  Column to match `x` against. Default `"ColumnPhysicalName"`; alternate
  options include `"DomainID"`, `"DomainName"`, `"DomainRanked"`,
  `"DisplayLabel"`, `"ChoiceSequence"`, `"ChoiceValue"`, `"ChoiceName"`,
  `"ChoiceLabel"`, `"ChoiceObsolete"`, `"ChoiceDescription"`,
  `"ColumnLogicalName"`

- choice:

  one of: `"ChoiceName"`, `"ChoiceValue"`, or `"ChoiceLabel"`

- obsolete:

  Include "obsolete" choices? Default: `FALSE`

- factor:

  Convert result to factor? Default: `TRUE`

- droplevels:

  Drop unused factor levels? Default: `TRUE` (used only when
  `factor=TRUE`)

- ordered:

  Should the result be an ordered factor? Default: `TRUE` (use *only* if
  `DomainRanked` is true for all choices)

- simplify:

  Should list result with length 1 be reduced to a single vector?
  Default: `TRUE`

- dsn:

  Optional: path or *DBIConnection* to [local database containing NASIS
  table
  structure](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md);
  default: NULL

## Value

A list of "choices" based on the input `x` that have been converted to a
consistent target set of levels (specified by `choice`) via NASIS 7
metadata.

When `factor=TRUE` the result is a factor, possibly ordered when
`ordered=TRUE` and the target domain is a "ranked" domain (i.e.
`ChoiceSequence` has logical meaning).

When `factor=FALSE` the result is a character or numeric vector. Numeric
vectors are always returned when `choice` is `"ChoiceValue"`.

## Examples

``` r
NASISChoiceList(1:3, "texcl")
#> [1] cos s   fs 
#> 21 Levels: c cl cos cosl fs fsl l lcos lfs ls lvfs s sc scl si sic sicl ... vfsl

NASISChoiceList(1:3, "pondfreqcl")
#> [1] none       rare       occasional
#> Levels: none < rare < occasional < frequent

NASISChoiceList("Clay loam", "texcl", choice = "ChoiceValue")
#> [1] 17

NASISChoiceList("Silty clay loam", "texcl", choice = "ChoiceName")
#> [1] sicl
#> 21 Levels: c cl cos cosl fs fsl l lcos lfs ls lvfs s sc scl si sic sicl ... vfsl
```

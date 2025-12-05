# Simplify Coarse Fraction Data

Simplify multiple coarse fraction (\>2mm) records by horizon.

## Usage

``` r
simplifyArtifactData(
  art,
  id.var,
  vol.var = "huartvol",
  nullFragsAreZero = nullFragsAreZero,
  ...
)

simplifyFragmentData(
  rf,
  id.var,
  vol.var = "fragvol",
  prefix = "frag",
  nullFragsAreZero = TRUE,
  msg = "rock fragment volume",
  ...
)
```

## Arguments

- art:

  a `data.frame` object, typically returned from NASIS, see details

- id.var:

  character vector with the name of the column containing an ID that is
  unique among all horizons in `rf`

- vol.var:

  character vector with the name of the column containing the coarse
  fragment volume. Default `"fragvol"` or `"huartvol`".

- nullFragsAreZero:

  should fragment volumes of NULL be interpreted as 0? (default:
  `TRUE`), see details

- ...:

  Additional arguments passed to sieving function (e.g. `sieves` a named
  numeric containing sieve size thresholds with class name)

- rf:

  a `data.frame` object, typically returned from NASIS, see details

- prefix:

  a character vector prefix for input

- msg:

  Identifier of data being summarized. Default is
  `"rock fragment volume"` but this routine is also used for
  `"surface fragment cover"`

## Details

This function is mainly intended for processing of NASIS pedon/component
data which contains multiple coarse fragment descriptions per horizon.
`simplifyFragmentData` will "sieve out" coarse fragments into the USDA
classes, split into hard and para- fragments. Likewise,
`simplifyArtifactData` will sieve out human artifacts, and split total
volume into "cohesive", "penetrable", "innocuous", and "persistent".

These functions can be applied to data sources other than NASIS by
careful use of the `id.var` and `vol.var` arguments.

- `rf` must contain rock or other fragment volumes in the column
  "fragvol" (or be specified with `vol.var`), fragment size (mm) in
  columns "fragsize_l", "fragsize_r", "fragsize_h", fragment cementation
  class in "fraghard" and flat/non-flat in "fragshp".

- `art` must contain artifact volumes in the column "huartvol" (or be
  specified with `vol.var`), fragment size (mm) in columns
  "huartsize_l", "huartsize_r", "huartsize_h", artifact cementation
  class in "huarthard" and flat/non-flat in "huartshp".

Examples:

- [KSSL data](http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.md)

## Author

D.E. Beaudette, A.G Brown

# Get Soil Data Viewer Attribute Information

Get Soil Data Viewer Attribute Information

## Usage

``` r
get_SDV_legend_elements(
  WHERE,
  alpha = 255,
  notratedcolor = rgb(1, 1, 1, 0),
  simplify = TRUE
)
```

## Arguments

- WHERE:

  WHERE clause for query of Soil Data Access `sdvattribute` table

- alpha:

  transparency value applied in calculation of hexadecimal color.
  Default: `255` (opaque).

- notratedcolor:

  Used to add 'Not rated' color entries where applicable. Default:
  `"#FFFFFF00"` (transparent white).

- simplify:

  Return a data.frame when `WHERE` is length 1? Return a list with 1
  element per legend when `WHERE` is length \> `1`? Default: `TRUE`

## Value

A list with a data.frame element for each element of `WHERE` containing
`"attributekey"`, `"attributename"`, `"attributetype"`,
`"attributetablename"`, `"attributecolumnname"`,
`"attributedescription"`, `"nasisrulename"`, `"label"`, `"order"`,
`"value"`, `"lower_value"`, `"upper_value"`,`"red"`, `"green"`, `"blue"`
and `"hex"` columns.

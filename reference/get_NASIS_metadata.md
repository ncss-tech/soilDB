# Get NASIS Metadata (Domain, Column and Choice Lists)

Retrieve a table containing domain and column names with choice list
labels/names/sequences/values from the NASIS 7 metadata tables.

## Usage

``` r
get_NASIS_metadata(dsn = NULL, include_description = FALSE)

get_NASIS_column_metadata(
  x,
  what = "ColumnPhysicalName",
  include_description = FALSE,
  dsn = NULL
)
```

## Arguments

- dsn:

  Optional: path or *DBIConnection* to [local database containing NASIS
  table
  structure](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md);
  default: `NULL`

- include_description:

  Include "ChoiceDescription" column? Default: `FALSE`

- x:

  character vector to match in NASIS metadata

- what:

  Column to match `x` against. Default `"ColumnPhysicalName"`; alternate
  options include `"DomainID"`, `"DomainName"`, `"DomainRanked"`,
  `"DisplayLabel"`, `"ChoiceSequence"`, `"ChoiceValue"`, `"ChoiceName"`,
  `"ChoiceLabel"`, `"ChoiceObsolete"`, `"ChoiceDescription"`,
  `"ColumnLogicalName"`

## Value

a `data.frame` containing DomainID, DomainName, DomainRanked,
DisplayLabel, ChoiceSequence, ChoiceValue, ChoiceName, ChoiceLabel,
ChoiceObsolete, ColumnPhysicalName, ColumnLogicalName and optionally
ChoiceDescription when `include_description=TRUE`.

a `data.frame` containing selected NASIS metadata sorted first on
`DomainID` and then on `ChoiceSequence`

## Details

These data are derived from the MetadataDomainDetail,
MetadataDomainMaster, and MetadataTableColumn tables and help with
mapping between values stored in the NASIS database and human-readable
values. The human-readable values align with the values returned in
public facing interfaces such as SSURGO via Soil Data Access and NASIS
Web Reports. The data in these tables can also be used to create
*ordered* factors where options for levels of a particular data element
follow a logical `ChoiceSequence`.

If a local NASIS instance is set up, and this is the first time
`get_NASIS_metadata()` has been called, the metadata will be obtained
from the NASIS local database. Subsequent runs in the same session will
use a copy of the data object `NASIS.metadata` cached in `soilDB.env`
which can be accessed with `get_soilDB_env()$NASIS.metadata`.

For users without a local NASIS instance, a cached copy of the NASIS
metadata are used `(data/metadata.rda)`.

See
[`?soilDB::metadata`](http://ncss-tech.github.io/soilDB/reference/metadata.md)
for additional details.

## Examples

``` r
get_NASIS_column_metadata("texcl")
#>      DomainID    DomainName DomainRanked DisplayLabel ChoiceSequence
#> 6363      189 texture_class            0            1              1
#> 6359      189 texture_class            0            1              2
#> 6343      189 texture_class            0            1              3
#> 6351      189 texture_class            0            1              4
#> 6345      189 texture_class            0            1              5
#> 6353      189 texture_class            0            1              6
#> 6355      189 texture_class            0            1              7
#> 6347      189 texture_class            0            1              8
#> 6349      189 texture_class            0            1              9
#> 6348      189 texture_class            0            1             10
#> 6350      189 texture_class            0            1             11
#> 6344      189 texture_class            0            1             12
#> 6361      189 texture_class            0            1             13
#> 6358      189 texture_class            0            1             14
#> 6357      189 texture_class            0            1             15
#> 6362      189 texture_class            0            1             16
#> 6360      189 texture_class            0            1             17
#> 6356      189 texture_class            0            1             18
#> 6352      189 texture_class            0            1             19
#> 6346      189 texture_class            0            1             20
#> 6354      189 texture_class            0            1             21
#>      ChoiceValue ChoiceName          ChoiceLabel ChoiceObsolete
#> 6363          21          c                 Clay              0
#> 6359          17         cl            Clay loam              0
#> 6343           1        cos          Coarse sand              0
#> 6351           9       cosl    Coarse sandy loam              0
#> 6345           3         fs            Fine sand              0
#> 6353          11        fsl      Fine sandy loam              0
#> 6355          13          l                 Loam              0
#> 6347           5       lcos    Loamy coarse sand              0
#> 6349           7        lfs      Loamy fine sand              0
#> 6348           6         ls           Loamy sand              0
#> 6350           8       lvfs Loamy very fine sand              0
#> 6344           2          s                 Sand              0
#> 6361          19         sc           Sandy clay              0
#> 6358          16        scl      Sandy clay loam              0
#> 6357          15         si                 Silt              0
#> 6362          20        sic           Silty clay              0
#> 6360          18       sicl      Silty clay loam              0
#> 6356          14        sil            Silt loam              0
#> 6352          10         sl           Sandy loam              0
#> 6346           4        vfs       Very fine sand              0
#> 6354          12       vfsl Very fine sandy loam              0
#>      ColumnPhysicalName ColumnLogicalName
#> 6363              texcl     texture_class
#> 6359              texcl     texture_class
#> 6343              texcl     texture_class
#> 6351              texcl     texture_class
#> 6345              texcl     texture_class
#> 6353              texcl     texture_class
#> 6355              texcl     texture_class
#> 6347              texcl     texture_class
#> 6349              texcl     texture_class
#> 6348              texcl     texture_class
#> 6350              texcl     texture_class
#> 6344              texcl     texture_class
#> 6361              texcl     texture_class
#> 6358              texcl     texture_class
#> 6357              texcl     texture_class
#> 6362              texcl     texture_class
#> 6360              texcl     texture_class
#> 6356              texcl     texture_class
#> 6352              texcl     texture_class
#> 6346              texcl     texture_class
#> 6354              texcl     texture_class
#>                                                                                                                   ChoiceDescription
#> 6363 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6359 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6343 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6351 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6345 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6353 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6355 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6347 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6349 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6348 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6350 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6344 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6361 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6358 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6357 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6362 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6360 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6356 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6352 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6346 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
#> 6354 Reference:  Soil Survey Manual, Agricultural Handbook No. 18, Soil Survey Staff, USDA, Natural Resources Conservation Service.
```

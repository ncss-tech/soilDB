# Query data from Kellogg Soil Survey Laboratory Data Mart via Soil Data Access or local SQLite snapshot

This function provides access to the Kellogg Soil Survey Laboratory Data
Mart via Soil Data Access or a local SQLite snapshot. See details and
examples for additional usage instructions.

## Usage

``` r
fetchLDM(
  x = NULL,
  what = "pedlabsampnum",
  bycol = "pedon_key",
  tables = c("lab_physical_properties", "lab_chemical_properties",
    "lab_calculations_including_estimates_and_default_values", "lab_rosetta_Key"),
  WHERE = NULL,
  chunk.size = 1000,
  ntries = 3,
  layer_type = c("horizon", "layer", "reporting layer"),
  area_type = c("ssa", "country", "state", "county", "mlra", "nforest", "npark"),
  prep_code = c("S", ""),
  analyzed_size_frac = c("<2 mm", ""),
  dsn = NULL
)
```

## Arguments

- x:

  A vector of values to find in column specified by `what`, default
  `NULL` uses no constraints on `what`

- what:

  A single column name from tables: `lab_combine_nasis_ncss`,
  `lab_webmap`, `lab_site`, `lab_pedon` or `lab_area`. Common choices
  include `pedlabsampnum` (Laboratory Pedon ID), `upedonid` (User Pedon
  ID), `corr_name` ('Correlated' Taxon Name), `samp_name` ('Sampled As'
  Taxon Name), or `area_code` (area symbol for specified `lab_area`
  records, see `area_type`).

- bycol:

  A single column name from `lab_layer` used for processing chunks;
  default: `"pedon_key"`

- tables:

  A vector of table names; Default is `"lab_physical_properties"`,
  `"lab_chemical_properties"`,
  `"lab_calculations_including_estimates_and_default_values"`, and
  `"lab_rosetta_Key"`. May also include one or more of: `"lab_mir"`,
  `"lab_mineralogy_glass_count"`,
  `"lab_major_and_trace_elements_and_oxides"`, `"lab_xray_and_thermal"`
  but it will be necessary to select appropriate `prep_code` and
  `analyzed_size_frac` for your analysis (see *Details*).

- WHERE:

  character. A custom SQL WHERE clause, which overrides `x`, `what`, and
  `bycol`, such as
  `CASE WHEN corr_name IS NOT NULL THEN LOWER(corr_name) ELSE LOWER(samp_name) END = 'musick'`

- chunk.size:

  Number of pedons per chunk (for queries that may exceed
  `maxJsonLength`)

- ntries:

  Number of tries (times to halve `chunk.size`) before returning `NULL`;
  default `3`

- layer_type:

  Default: `"horizon"`, `"layer"`, and `"reporting layer"`

- area_type:

  Default: `"ssa"` (Soil Survey Area). Other options include (choose
  one): `"country"`, `"state"`, `"county"`, `"mlra"` (Major Land
  Resource Area), `"nforest"` (National Forest), `"npark"` (National
  Park)

- prep_code:

  Default: `"S"` and `""`. May also include one or more of: `"F"`,
  `"HM"`, `"HM_SK"` `"GP"`, `"M"`, `"N"`, or `"S"`

- analyzed_size_frac:

  Default: `"<2 mm"` and `""`. May also include one or more of:
  `"<0.002 mm"`, `"0.02-0.05 mm"`, `"0.05-0.1 mm"`, `"0.1-0.25 mm"`,
  `"0.25-0.5 mm"`, `"0.5-1 mm"`, `"1-2 mm"`, `"0.02-2 mm"`,
  `"0.05-2 mm"`

- dsn:

  Data source name; either a path to a SQLite database, an open
  DBIConnection or (default) `NULL` (to use
  [`soilDB::SDA_query`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md))

## Value

a `SoilProfileCollection` for a successful query, a `try-error` if no
site/pedon locations can be found or `NULL` for an empty `lab_layer`
(within sites/pedons) result

## Details

You can download SQLite or GeoPackage snapshots here:
<https://ncsslabdatamart.sc.egov.usda.gov/database_download.aspx>.
Specify the `dsn` argument to use a local copy of the lab data rather
than Soil Data Access web service.

Lab Data Mart model diagram:
<https://jneme910.github.io/Lab_Data_Mart_Documentation/Documents/SDA_KSSL_Data_model.html>
If the `chunk.size` parameter is set too large and the Soil Data Access
request fails, the algorithm will re-try the query with a smaller
(halved) `chunk.size` argument. This will be attempted up to 3 times
before returning `NULL`

The default behavior joins the `lab_area` tables only for the "Soil
Survey Area" related records. You can specify alternative area records
for use in `x`, `what` or `WHERE` arguments by setting `area_type` to a
different value.

When requesting data from `"lab_major_and_trace_elements_and_oxides"`,
`"lab_mineralogy_glass_count"`, or `"lab_xray_and_thermal"` multiple
preparation codes (`prep_code`) or size fractions (`analyzed_size_frac`)
are possible. The default behavior of `fetchLDM()` is to attempt to
return a topologically valid (minimal overlaps) *SoilProfileCollection*.
This is achieved by setting `prep_code="S"` ("sieved") and
`analyzed_size_frac="<2 mm"`. You may specify alternate or additional
preparation codes or fractions as needed, but note that this may cause
"duplication" of some layers where measurements were made with different
preparation or on fractionated samples

## Examples

``` r
if (FALSE) { # \dontrun{
  
  # fetch by Soil Survey Area area symbol (area_code using default "ssa" area_type)
  res <- fetchLDM("CA630", what = "area_code")
  
  # fetch by Major Land Resource area symbol (area_code using "mlra" area_type)
  res <- fetchLDM("22A", what = "area_code", area_type = "mlra")
  
  # fetch by multiple case-insensitive taxon name
  # (correlated or sampled as Musick or Holland series)
  res <- fetchLDM(WHERE = "(CASE WHEN corr_name IS NOT NULL 
                                THEN LOWER(corr_name) 
                                ELSE LOWER(samp_name) 
                            END) IN ('musick', 'holland')")

  # physical properties of soils correlated as taxonomic subgroup "Typic Argialbolls"
  res <- fetchLDM(x = "Typic Argialbolls", 
                  what = "corr_taxsubgrp", 
                  tables = "lab_physical_properties")

} # }
```

# Soil Data Access

## Introduction to Soil Data Access (SDA)

The Soil Data Access (SDA) system provides REST-based access to the
SSURGO and STATSGO2 databases maintained by USDA-NRCS. Instead of
downloading large databases, SDA allows you to query exactly what you
need via a web service.

`soilDB` provides several functions that help with generating requests
to the web service at a low level, as well as higher-level functions
that can return complex data structures like the `SoilProfileCollection`
from the `aqp` package or spatial object types.

### What is SDA?

SDA provides access to a SQL Server database containing a variety of
soil information:

- **Tabular data**: map units, components, horizons, interpretations,
  ecological classes, laboratory characterization results, and more
- **Spatial data**: map unit polygons with geometry (available via WKT
  or for spatial queries), point locations (XY coordinates)

The SDA REST endpoint is available at:

    https://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest

All queries to SDA are executed through the
[`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
function in `soilDB`, which handles the HTTP POST requests and JSON
response parsing automatically.

### SSURGO and STATSGO2 Data

SDA contains both the detailed Soil Survey Geographic Database
(**SSURGO**) and the generalized U.S. General Soil Map (**STATSGO2**).

- **SSURGO**: Detailed mapping (scales 1:12,000 to 1:24,000). Identified
  by specific area symbols (e.g., `CA630`, `IN005`).
- **STATSGO2**: Generalized mapping (scale 1:250,000) for
  regional/national analysis. Identified by the single area symbol
  `'US'`.

See the Data Filters section below for details on filtering out STATSGO2
data.

### When to Use SDA vs. Local SSURGO

Use SDA for current data from NRCS without manual downloads, queries
across multiple survey areas, quick one-time analyses, or access to
interpretations and aggregated properties. Use local SSURGO databases
for repeated queries to the same survey areas (faster), offline access,
complete survey databases with all tables, or direct SQLite
manipulation.

The same `get_SDA_*()` functions documented in this vignette can be
applied to local SSURGO databases by passing the `dsn` parameter. See
the [Local SSURGO
vignette](http://ncss-tech.github.io/soilDB/articles/local-ssurgo.md)
for downloading and creating local SSURGO GeoPackage databases, and for
examples of using these functions with local data.

### Package Requirements

``` r
library(soilDB)
```

For spatial data examples, use `sf` and `terra`, which are both optional
but recommended.

``` r
install.packages(c("sf", "terra"))
```

------------------------------------------------------------------------

## Basic `SDA_query()` Function

### Core Syntax

The
[`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
function executes SQL queries directly against the SDA database:

``` r
# Simple query to get the first 5 map units from an area
q <- "
SELECT TOP 5
  mapunit.mukey,
  mapunit.nationalmusym,
  legend.areasymbol,
  mapunit.musym,
  mapunit.muname
FROM legend
INNER JOIN mapunit ON mapunit.lkey = legend.lkey
WHERE legend.areasymbol = 'CA630'
ORDER BY mapunit.musym
"

result <- SDA_query(q)
head(result)
```

    ##     mukey nationalmusym areasymbol musym
    ## 1 2403709         2lp80      CA630  1012
    ## 2 2924914         2x4d4      CA630  1013
    ## 3 2924833         2x297      CA630  1090
    ## 4 2924834         2x298      CA630  1091
    ## 5 2924907         2x8lf      CA630   128
    ##                                                                   muname
    ## 1                                                             Mined land
    ## 2      Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
    ## 3 Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
    ## 4  Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
    ## 5                            Cogna loam, 0 to 2 percent slopes, overwash

### Handling Errors

If
[`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
errors for any reason, the result will not be a data.frame with data.
Instead it will be an object of class `"try-error"`. You can handle the
possibility of error using
[`inherits()`](https://rdrr.io/r/base/class.html). For example:

``` r
result <- SDA_query(q)

# check if the result is an error response
if (!inherits(result, 'try-error')){
  head(result)
} else {
  # stop with error message extracted from result
  stop("Error occured! ", result[1])
}
```

    ##     mukey nationalmusym areasymbol musym
    ## 1 2403709         2lp80      CA630  1012
    ## 2 2924914         2x4d4      CA630  1013
    ## 3 2924833         2x297      CA630  1090
    ## 4 2924834         2x298      CA630  1091
    ## 5 2924907         2x8lf      CA630   128
    ##                                                                   muname
    ## 1                                                             Mined land
    ## 2      Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
    ## 3 Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
    ## 4  Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
    ## 5                            Cogna loam, 0 to 2 percent slopes, overwash

You can set up a loop to retry a query that fails in the event of
intermittent network or server issues. You should have a brief period of
wait time ([`Sys.sleep()`](https://rdrr.io/r/base/Sys.sleep.html))
between requests.

For repeated failures, it is best to increase that wait time in between
each try up to, for example, a maximum of 3 attempts. The following
example uses “exponential backoff” which increases the wait time in
between requests incrementally from ~3 seconds to ~20 seconds over the
course of several attempts.

``` r
n_tries <- 1
while (n_tries <= 3){
  result <- SDA_query(q)
  
  # check if the result is an error response
  if (!inherits(result, 'try-error')){
    head(result)
    
    # if no error, break out of the loop
    break
  } else {
    # on error, increment the number of tries
    # and sleep (time in seconds)
    Sys.sleep(exp(n_tries))
    
    n_tries <- n_tries + 1
    if (n_tries > 3) {
      stop("Error occured! ", result[1])
    }
  }
}
```

### Understanding SDA SQL

SDA uses **Microsoft SQL Server Transact-SQL (T-SQL)** dialect.

Key differences of T-SQL from SQLite and other SQL dialects that are
relevant to queries used in soilDB include:

| Feature              | T-SQL            | SQLite           | Notes                    |
|----------------------|------------------|------------------|--------------------------|
| Row limit            | `TOP 10`         | `LIMIT 10`       | Place after SELECT       |
| NULL functions       | `ISNULL(col, 0)` | `IFNULL(col, 0)` | Different function names |
| String concatenation | `col1 + col2`    | `col1 || col2`   | Different operators      |

When using
[`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
with a **local SQLite database** (via `dsn` parameter), `soilDB`
automatically converts some T-SQL syntax to SQLite equivalents to allow
for both to work. This behavior is triggered when the `dsn` argument is
specified. You can view the queries (and save them to run yourself) by
passing the `query_string=TRUE` argument.

### Basic Table Joins

Many custom SSURGO queries follow a standard pattern of relationships
from legend, to mapunit, to component, to a related table to component
such as component horizon.

In SSURGO data model primary and foreign key physical column names all
end with the word `"key"`. The primary key is a unique identifier of a
particular record.

Foreign keys are references to primary keys of other tables and define
the relationships between tables that can be used for joins.

Here we select a handful of values from all of the above mentioned
tables corresponding to the first 10 component horizon (`chorizon`) in
the SDA table.

``` r
# Get map unit, component, and horizon data
q <- "
SELECT TOP 10
  mu.mukey,
  leg.areasymbol,
  mu.musym,
  co.compname,
  co.comppct_r,
  ch.hzname,
  ch.hzdept_r,
  ch.hzdepb_r,
  ch.claytotal_r
FROM legend AS leg
INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
INNER JOIN component AS co ON co.mukey = mu.mukey
LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
WHERE leg.areasymbol = 'CA630'
  AND co.majcompflag = 'Yes'
  AND ch.claytotal_r IS NOT NULL
ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
"

result <- SDA_query(q)
head(result, 10)
```

    ##      mukey areasymbol musym               compname comppct_r hzname hzdept_r
    ## 1  2924914      CA630  1013 Anthraltic Xerorthents        30     ^A        0
    ## 2  2924914      CA630  1013 Anthraltic Xerorthents        30    ^AC        3
    ## 3  2924914      CA630  1013 Anthraltic Xerorthents        30     ^C       23
    ## 4  2924833      CA630  1090     Ultic Haploxeralfs        60      A        0
    ## 5  2924833      CA630  1090     Ultic Haploxeralfs        60     BA        8
    ## 6  2924833      CA630  1090     Ultic Haploxeralfs        60    Bt1       24
    ## 7  2924833      CA630  1090     Ultic Haploxeralfs        60    Bt2       61
    ## 8  2924833      CA630  1090     Ultic Haploxeralfs        60     Ct      100
    ## 9  2924833      CA630  1090    Mollic Haploxeralfs        30      A        0
    ## 10 2924833      CA630  1090    Mollic Haploxeralfs        30    Bt1        6
    ##    hzdepb_r claytotal_r
    ## 1         3           5
    ## 2        23           3
    ## 3       100           2
    ## 4         8          12
    ## 5        24          15
    ## 6        61          18
    ## 7       100          25
    ## 8       152          29
    ## 9         6          16
    ## 10       27          18

We supplied all of the filtering constraints in a `WHERE` clause for
clarity, with joins only using primary and foreign keys.

In general, the same constraints can be expressed as part of a join to
relevant tables, which can be more efficient in complex queries.

### Understanding LEFT JOIN vs INNER JOIN

One of the most important considerations in SSURGO queries is **data
completeness**: not all relationships in the SSURGO database are
populated for all soils or components. Understanding when to use LEFT
JOIN vs. INNER JOIN is critical for correct analysis.

#### When Data are Complete: INNER JOIN

Use `INNER JOIN` when the relationship **must exist** for meaningful
results:

``` r
# These relationships are always present in SSURGO:
# - mapunit always belongs to a legend (survey area)
# - component always belongs to a mapunit
q <- "
SELECT TOP 10
  leg.areasymbol,
  mu.musym,
  co.compname,
  co.comppct_r
FROM legend AS leg
INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
INNER JOIN component AS co ON co.mukey = mu.mukey
WHERE leg.areasymbol = 'CA630'
ORDER BY mu.musym, co.comppct_r DESC
"

result <- SDA_query(q)

# Every row represents a valid component with all parent relationships
head(result)
```

    ##   areasymbol musym               compname comppct_r
    ## 1      CA630  1012             Mined land        94
    ## 2      CA630  1012                  Water         6
    ## 3      CA630  1013             Mined land        70
    ## 4      CA630  1013 Anthraltic Xerorthents        30
    ## 5      CA630  1090     Ultic Haploxeralfs        60
    ## 6      CA630  1090    Mollic Haploxeralfs        30

#### When Data May Be Missing: LEFT JOIN

Use `LEFT JOIN` when a parent record may exist **without child
records**. Two example cases:

**Case 1: Components Without Horizons** Some soil components in SSURGO
do **not** have associated horizon data. These are often “non-soil”
components (`compkind` “Miscellaneous Area”) or minor components
(`majcompflag` “No”) which may lack detailed horizon description:

``` r
# Some components have NO horizons defined
# INNER JOIN would exclude these components entirely
# LEFT JOIN preserves components even without horizon data
q <- "
SELECT
  leg.areasymbol,
  mu.mukey,
  mu.musym,
  co.compname,
  co.comppct_r,
  ch.chkey,
  ch.hzdept_r,
  ch.hzdepb_r
FROM legend AS leg
INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
INNER JOIN component AS co ON co.mukey = mu.mukey
LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
WHERE leg.areasymbol = 'CA630'
  AND co.majcompflag = 'Yes'
ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
"

result <- SDA_query(q)

# Check for components without horizons:
# Rows with NA in horizon columns indicate a component with no horizon data
if (is.data.frame(result) && nrow(result) > 0) {
  components_no_horizons <- result[is.na(result$chkey), ]
  if (nrow(components_no_horizons) > 0) {
    cat('Found', nrow(components_no_horizons), 'components without horizon data:\n')
    print(components_no_horizons[, c('musym', 'compname', 'comppct_r')])
  }
}
```

    ## Found 29 components without horizon data:
    ##      musym     compname comppct_r
    ## 1     1012   Mined land        94
    ## 2     1013   Mined land        70
    ## 59    3020 Rock outcrop        15
    ## 69    3021 Rock outcrop        15
    ## 116   3046 Rock outcrop        20
    ## 269   5016 Rock outcrop        20
    ## 341   6029 Rock outcrop        25
    ## 550   6076 Rock outcrop        25
    ## 786   7091 Rock outcrop        11
    ## 796   7092 Rock outcrop        15
    ## 857   7166 Rock outcrop        12
    ## 935   8110    Riverwash        20
    ## 944   8111    Riverwash        15
    ## 1166  8289 Rock outcrop        25
    ## 1197  8314 Rock outcrop        40
    ## 1211  8317 Rock outcrop        35
    ## 1217  8318 Rock outcrop        25
    ## 1227  8319 Rock outcrop        35
    ## 1236  9010   Urban land        85
    ## 1237  9011   Urban land        50
    ## 1249  9012   Urban land        65
    ## 1254  9013   Urban land        50
    ## 1260  9014   Urban land        50
    ## 1271  9015   Urban land        50
    ## 1283  9016   Urban land        50
    ## 1298  9017   Urban land        60
    ## 1302  9018   Urban land        65
    ## 1310   DAM         Dams       100
    ## 1311     W        Water       100

**Comparison: INNER JOIN would have excluded those components:**

``` r
# Using INNER JOIN on horizons drops components that lack horizon data
q_inner <- "
SELECT
  leg.areasymbol,
  mu.mukey,
  mu.musym,
  co.compname,
  co.comppct_r,
  ch.desgnmaster,
  ch.hzdept_r,
  ch.hzdepb_r
FROM legend AS leg
INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
INNER JOIN component AS co ON co.mukey = mu.mukey
INNER JOIN chorizon AS ch ON ch.cokey = co.cokey
WHERE leg.areasymbol = 'CA630'
  AND co.majcompflag = 'Yes'
ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
"

result_inner <- SDA_query(q_inner)

# number of rows in LEFT JOIN result
nrow(result)
```

    ## [1] 1311

``` r
# number of rows in INNER JOIN result
nrow(result_inner)
```

    ## [1] 1282

**Case 2: Horizons Without Rock Fragments**

Similarly, not all horizons have child table rock fragment volume
recorded. An empty chfrags table is conventionally used for horizons
with no fragments, but in some cases may be used for soils that have
non-zero fragment weight fractions (for example, those with human
artifacts). When you need to include horizons regardless of fragment
availability, use LEFT JOIN:

``` r
# Some horizons have NO rock fragment data recorded
# LEFT JOIN preserves horizons even without fragment information
q <- "
SELECT
  mu.musym,
  co.compname,
  ch.desgnmaster,
  ch.hzdept_r,
  ch.hzdepb_r,
  rf.fragsize_h,
  rf.fragvol_r
FROM legend AS leg
INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
INNER JOIN component AS co ON co.mukey = mu.mukey
LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
LEFT JOIN chfrags AS rf ON rf.chkey = ch.chkey
WHERE leg.areasymbol = 'CA630'
  AND co.majcompflag = 'Yes'
ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
"

result <- SDA_query(q)

# Check for horizons without fragments:
if (is.data.frame(result) && nrow(result) > 0) {
  horizons_no_frags <- result[!duplicated(result$chkey) & is.na(result$fragsize_h), ]
  if (nrow(horizons_no_frags) > 0) {
    cat('Found', nrow(horizons_no_frags), 'horizons without rock fragment data:\n')
    print(horizons_no_frags[, c('musym', 'compname', 'desgnmaster', 'fragvol_r')])
  }
}
```

#### Summary: When to Use Each Join Type

| Situation                                                                                   | Join Type      | Reason                                                          |
|---------------------------------------------------------------------------------------------|----------------|-----------------------------------------------------------------|
| Query result must have values at all levels                                                 | INNER          | Filter out incomplete records                                   |
| Parent may exist without children (components without horizons, horizons without fragments) | LEFT           | Preserve parent records even when children are missing          |
| Calculating aggregations (COUNT, SUM, AVG) with missing data                                | Depends        | LEFT includes zero-occurrence groups; INNER ignores them        |
| Applying WHERE filters to child tables                                                      | Typically LEFT | INNER JOIN combined with WHERE can lose parents unintentionally |

**Tip:** When using LEFT JOIN, always check for `NA` values in child
table columns to identify records with missing child data.

------------------------------------------------------------------------

### Aggregation Functions

SDA supports standard SQL aggregate functions (COUNT, SUM, AVG, MIN,
MAX, etc.):

``` r
# Get component statistics per map unit
q <- "
SELECT
  mu.mukey,
  mu.musym,
  mu.muname,
  COUNT(co.cokey) AS n_components,
  SUM(co.comppct_r) AS total_comppct,
  AVG(co.comppct_r) AS avg_comppct
FROM legend AS leg
INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
INNER JOIN component AS co ON co.mukey = mu.mukey
WHERE leg.areasymbol = 'CA630'
GROUP BY mu.mukey, mu.musym, mu.muname
ORDER BY mu.musym
"

result <- SDA_query(q)
head(result)
```

    ##     mukey musym
    ## 1 2403709  1012
    ## 2 2924914  1013
    ## 3 2924833  1090
    ## 4 2924834  1091
    ## 5 2924907   128
    ## 6 2924879   206
    ##                                                                   muname
    ## 1                                                             Mined land
    ## 2      Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
    ## 3 Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
    ## 4  Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
    ## 5                            Cogna loam, 0 to 2 percent slopes, overwash
    ## 6                               Pentz sandy loam, 2 to 15 percent slopes
    ##   n_components total_comppct avg_comppct
    ## 1            2           100          50
    ## 2            2           100          50
    ## 3            6           100          16
    ## 4            2           100          50
    ## 5            6           100          16
    ## 6            8           100          12

Here we use SQL `GROUP BY` to define the columns used to determine
unique groups. Other columns that are not part of the GROUP BY statement
need to be included in some sort of aggregation expression to be used in
SELECT clause.

When calculating map unit weighted averages using custom SQL, be aware
that `comppct_r` (Component Percentage) does not always sum to 100% for
a map unit due to minor component inclusions that may not be exported or
missing data.

A robust weighted average should normalize weights by the sum of
component percentages present in the data, rather than assuming a sum of
100. This is especially important if you are relying on INNER JOIN to
chorizon table, as many mapunits have at least some components with no
horizon data.

### Data Filters

#### Filtering SSURGO vs. STATSGO2

When querying SDA, especially without specific map unit keys or area
symbols, it is often important to exclude STATSGO2 data to prevent
mixing these two distinct datasets.

**Common filter to exclude STATSGO2:**

``` sql
WHERE areasymbol != 'US'
```

#### Filtering Horizon Data

When aggregating soil properties, it is sometimes important to exclude
layers that do not represent mineral soil so that depth-weighted
averages and other statistics are not skewed.

Note that if zero values are present for a soil property they can drag
down averages. If you include a weighting factor in a custom query for
the thickness of a layer with a `NULL` value, that is essentially the
same as if the property value were `0`.

The two most common types of non-mineral soil layers to exclude are:

1.  **Bedrock and other Root-Limiting Layers:** Below the “bottom of the
    soil” (e.g., `R`, `Cr` horizons).

2.  **Dry Surface Organic Layers (Duff):** High-carbon, low bulk
    density, dry organic (O) horizons

#### Filtering Strategy

Since SSURGO data spans decades of standards, no single column perfectly
identifies these non-soil layers.

The safest approach is to use the new computed `horztype` column *in
combination with* in lieu texture class codes. Currently `horztype` only
supports identification of dry organic layers on the surface, but it may
be extended to other material types in the future.

**1. Filter Bedrock:**

Bedrock layers often have `NULL` property values, which is not much of a
problem provided there are data available for the overlying soil layers;
but you will need to exclude the thickness of the bedrock layers for
accurate depth-weighted average calculations.

The `horztype` column does not capture bedrock. You must filter by
texture code:

``` sql
/* Exclude Bedrock */
AND texturerv NOT IN ('BR', 'WB', 'UWB') 
```

This includes the modern “bedrock” (BR), as well as the obsolete
“weathered bedrock” and “unweathered bedrock”. For the purposes of
excluding non-soil bedrock material, they can be treated as equivalent
and will cover many cases.

There are a variety of other “in lieu” texture codes that are used to
identify cemented materials (“CEM”, “IND”, “MAT”) which may or may not
have other texture information and may also need to be considered for
exclusion.

While it might be tempting to just filter on horizon designation
(e.g. excluding R or Cr layers explicitly), as discussed below this is
not completely reliable on its own.

**2. Filter Dry Organics (Duff):**

The `horztype` column is currently primarily for identifying “Dry
Organic” layers. This is a new column that has been programmatically
populated to help users filter out a common set of data.

Combine `horztype` with texture codes like `"SPM"` (which is the
least-decomposed organic soil material: dry fibric material) for more
robust confirmation of filtering logic. You can also include `"MPM"` and
`"HPM"` if you are interested in excluding even more decomposed dry
organic materials:

``` sql
/* Exclude "Duff" / Dry Organics */
AND ISNULL(horztype, '') != 'Dry Organic'
AND texturerv NOT IN ('SPM', 'MPM', 'HPM') 
```

The population of O horizons in SSURGO is inconsistent due to evolving
data collection standards. While modern surveys often include numerical
data for organic surface layers, legacy data may only mention them in
descriptions without populated properties.

Retain these layers when analyzing data completeness or identifying
diagnostic features like folistic epipedons. However, for analyses
focused on mineral soil properties, excluding them is often necessary to
prevent skewing depth-weighted averages.

**Note:** We generally want to *keep* “Wet Organic” layers (PEAT, MPT
\[mucky peat\], and MUCK) as these may be diagnostic for Histosols,
Histic epipedons etc., which are important concepts for identification
of hydric soils.

**3. Filter by Horizon Designation**

It is also possible, but not completely reliable due to soil survey
vintage and patterns in historical data population, to filter by horizon
designation.

For instance, a logical statement to exclude the master horizon
designation “O” for organics:

``` r
AND desgnmaster != 'O'
```

While using horizon designations is conceptually appealing, it is
problematic when it is the only logic you are using. Horizon
designations were not historically populated for the aggregate soil
component data (SSURGO).

In the last two decades layers in the `chorizon` table have been
assigned morphologic horizon designations, but this has not been done
retroactively. Some older soil surveys still use “H” as the master
horizon designation for all layers e.g. H1, H2, H3 instead of A, B, C.
If this is the case the only way to identify bedrock, or organic layers,
is to use other data elements such as in lieu texture class.

#### Soil Materials

This section demonstrates filtering horizon data based on material
types.

We will start with a custom query that targets components with “Histic”
taxonomic subgroup formative element.

``` r
q <- "
SELECT TOP 10
  component.compname,
  ch.hzname,
  ch.hzdept_r,
  ch.hzdepb_r,
  ch.texturerv,
  ch.horztype,
  ch.claytotal_r,
  ch.om_r
FROM chorizon AS ch
INNER JOIN component ON component.cokey = ch.cokey AND taxsubgrp LIKE '%Histic%'
ORDER BY component.cokey, hzdept_r
"

result <- SDA_query(q)
result
```

    ##    compname hzname hzdept_r hzdepb_r texturerv horztype claytotal_r om_r
    ## 1  Scarboro     H1        0       23      <NA>     <NA>         4.0   NA
    ## 2  Scarboro     H2       23       41      <NA>     <NA>         3.0   NA
    ## 3  Scarboro     H3       41       84      <NA>     <NA>         1.0   NA
    ## 4  Scarboro     H4       84      165      <NA>     <NA>         1.0   NA
    ## 5  Scarboro     H1        0       23      <NA>     <NA>         4.0   NA
    ## 6  Scarboro     H2       23       41      <NA>     <NA>         3.0   NA
    ## 7  Scarboro     H3       41       84      <NA>     <NA>         1.0   NA
    ## 8  Scarboro     H4       84      165      <NA>     <NA>         1.0   NA
    ## 9    Canova     H1        0       23      <NA>     <NA>          NA   55
    ## 10   Canova     H2       23       56      <NA>     <NA>         3.5   NA

Here is a sample query to select mineral and wet organic soil layers:

``` r
q <- "
SELECT TOP 10
  ch.hzname,
  ch.hzdept_r,
  ch.hzdepb_r,
  ch.texturerv,
  ch.horztype,
  ch.claytotal_r,
  ch.om_r
FROM chorizon AS ch
WHERE 
  -- 1. Exclude Bedrock
  ch.texturerv NOT IN ('BR', 'WB', 'UWB') 
  
  -- 2. Exclude Dry Organic / Duff
  AND ISNULL(ch.horztype, '') != 'Dry Organic'
  AND ch.texturerv NOT IN ('SPM', 'MPM', 'HPM')
ORDER BY cokey, hzdept_r
"

result <- SDA_query(q)
head(result)
```

    ##   hzname hzdept_r hzdepb_r texturerv horztype claytotal_r om_r
    ## 1     Ap        0       25       SIL    Other          26 2.00
    ## 2      A       25       36      SICL    Other          31 1.75
    ## 3     Bg       36       96      SICL    Other          31 1.00
    ## 4     Cg       96      135      SICL    Other          37 0.75
    ## 5     Ab      135      152      SICL    Other          37 2.00
    ## 6     H1        0       38      SICL    Other          38 3.50

In contrast to bedrock, organic soil materials are soil materials,
whether they are wet or dry–but it is important to know when you may
need to look past them. Organic layers have very low bulk density and
very high organic carbon content, both of which can skew “mineral soil”
averages if included in depth-weighted calculations.

Take for example the K factor for estimating erosion hazard: users often
want properties for an exposed mineral soil surface rather than that of
an organic layer over the mineral soil surface. In general, the concept
of K factor does not apply to the organic surface.

To demonstrate, we will look at the whole SSURGO database. We count the
number of horizons with master designation “O” and horizon top depth `0`
within groups of different combinations of `Kw` and `Kf`, and sort in
decreasing order.

``` r
q <- "
SELECT 
  COUNT(chkey) AS n, 
  kwfact, 
  kffact 
FROM chorizon 
WHERE desgnmaster = 'O' AND hzdept_r = 0
GROUP BY kwfact, kffact
ORDER BY n DESC
"
result <- SDA_query(q)
result$percent <- round(prop.table(result$n) * 100, 1)
head(result, 10)
```

    ##         n kwfact kffact percent
    ## 1  128757   <NA>   <NA>    99.0
    ## 2     754    .02    .02     0.6
    ## 3     402    .05    .05     0.3
    ## 4      42    .10    .10     0.0
    ## 5      25    .15    .15     0.0
    ## 6      12    .17    .17     0.0
    ## 7      11    .28    .28     0.0
    ## 8       8    .24    .24     0.0
    ## 9       5    .37    .37     0.0
    ## 10      4    .10    .17     0.0

This query shows that many O horizons at the soil surface have NULL K
factor values.

The take away from this should be: if your analysis relies on having K
factor **everywhere** you will either need to filter out the O horizons,
or come up with a way to impute a value to replace the NULL values.

### Rock Fragment Data

Rock fragments are particles larger than 2 mm. In SSURGO, fragment data
are stored in several places both aggregated up to the horizon level,
and in child tables of horizon.

Fragments are classified by size class (e.g. gravel, cobbles, stones,
boulders) and recorded as volume percentages. Derived weight fractions
are also stored at the horizon level.

#### Basic Rock Fragment Query

``` r
# Query horizons with their rock fragment content
q <- "
SELECT TOP 20
  mu.musym,
  co.compname,
  ch.hzname,
  ch.hzdept_r,
  ch.hzdepb_r,
  rf.fragkind,
  rf.fragsize_h,
  rf.fragvol_r
FROM legend AS leg
INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
INNER JOIN component AS co ON co.mukey = mu.mukey
LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
LEFT JOIN chfrags AS rf ON rf.chkey = ch.chkey
WHERE leg.areasymbol = 'CA630'
  AND co.majcompflag = 'Yes'
ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r, rf.fragsize_h
"

result <- SDA_query(q)
head(result, 20)
```

    ##    musym               compname hzname hzdept_r hzdepb_r             fragkind
    ## 1   1012             Mined land   <NA>       NA       NA                 <NA>
    ## 2   1013             Mined land   <NA>       NA       NA                 <NA>
    ## 3   1013 Anthraltic Xerorthents     ^A        0        3 Mixed rock fragments
    ## 4   1013 Anthraltic Xerorthents     ^A        0        3 Mixed rock fragments
    ## 5   1013 Anthraltic Xerorthents     ^A        0        3 Mixed rock fragments
    ## 6   1013 Anthraltic Xerorthents    ^AC        3       23 Mixed rock fragments
    ## 7   1013 Anthraltic Xerorthents    ^AC        3       23 Mixed rock fragments
    ## 8   1013 Anthraltic Xerorthents    ^AC        3       23 Mixed rock fragments
    ## 9   1013 Anthraltic Xerorthents     ^C       23      100 Mixed rock fragments
    ## 10  1013 Anthraltic Xerorthents     ^C       23      100 Mixed rock fragments
    ## 11  1013 Anthraltic Xerorthents     ^C       23      100 Mixed rock fragments
    ## 12  1090     Ultic Haploxeralfs      A        0        8 Mixed rock fragments
    ## 13  1090     Ultic Haploxeralfs     BA        8       24 Mixed rock fragments
    ## 14  1090     Ultic Haploxeralfs     BA        8       24 Mixed rock fragments
    ## 15  1090     Ultic Haploxeralfs     BA        8       24 Mixed rock fragments
    ## 16  1090     Ultic Haploxeralfs    Bt1       24       61 Mixed rock fragments
    ## 17  1090     Ultic Haploxeralfs    Bt1       24       61 Mixed rock fragments
    ## 18  1090     Ultic Haploxeralfs    Bt1       24       61 Mixed rock fragments
    ## 19  1090     Ultic Haploxeralfs    Bt2       61      100 Mixed rock fragments
    ## 20  1090     Ultic Haploxeralfs    Bt2       61      100 Mixed rock fragments
    ##    fragsize_h fragvol_r
    ## 1          NA        NA
    ## 2          NA        NA
    ## 3          75        30
    ## 4         250        30
    ## 5         600        10
    ## 6          75        40
    ## 7         250        30
    ## 8         600        10
    ## 9          75        15
    ## 10        250        55
    ## 11        600        20
    ## 12         75        20
    ## 13         75         0
    ## 14         75        15
    ## 15        250         0
    ## 16         75         3
    ## 17         75        25
    ## 18        250        10
    ## 19         75        33
    ## 20         75        10

Above you can see multiple fragment size classes can exist in a single
horizon (a many-to-one relationship). Horizons without fragments
generally have no records in the `chfrags` table.

- `chfrags` table contains individual fragment records per horizon and
  size class (e.g. at least one each for gravel, cobbles, stones,
  boulders, where present)

- `fragsize_h` indicates upper end of fragment size range for a specific
  record

- `fragvol_r` is the representative volume percentage for that specific
  record

#### Aggregating Rock Fragments by Horizon

Sum fragment volumes to get total rock fragment content per horizon:

``` r
# Total rock fragment content per horizon
q <- "
SELECT
  mu.musym,
  co.compname,
  co.comppct_r,
  ch.hzname,
  ch.hzdept_r,
  ch.hzdepb_r,
  SUM(rf.fragvol_r) AS calc_fragvoltot_r,
  COUNT(DISTINCT rf.fragsize_h) AS n_frag_classes
FROM legend AS leg
INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
INNER JOIN component AS co ON co.mukey = mu.mukey
LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
LEFT JOIN chfrags AS rf ON rf.chkey = ch.chkey
WHERE leg.areasymbol = 'CA630'
  AND co.majcompflag = 'Yes'
GROUP BY mu.musym, co.compname, co.comppct_r, ch.hzname,
         ch.hzdept_r, ch.hzdepb_r, ch.chkey
ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
"

result <- SDA_query(q)
head(result)
```

    ##   musym               compname comppct_r hzname hzdept_r hzdepb_r
    ## 1  1012             Mined land        94   <NA>       NA       NA
    ## 2  1013             Mined land        70   <NA>       NA       NA
    ## 3  1013 Anthraltic Xerorthents        30     ^A        0        3
    ## 4  1013 Anthraltic Xerorthents        30    ^AC        3       23
    ## 5  1013 Anthraltic Xerorthents        30     ^C       23      100
    ## 6  1090     Ultic Haploxeralfs        60      A        0        8
    ##   calc_fragvoltot_r n_frag_classes
    ## 1                NA              0
    ## 2                NA              0
    ## 3                70              3
    ## 4                80              3
    ## 5                90              3
    ## 6                20              1

**Note:** T-SQL is strict about aggregation syntax. Every column in the
SELECT clause that is not wrapped in an aggregate function (SUM, COUNT,
etc.) MUST be listed in the GROUP BY clause.

#### Rock Fragment Properties via get_SDA_property()

Use
[`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)
to retrieve rock fragment properties with aggregation:

``` r
# Get rock fragment weight percentages (3-10 inch class)
frag_3_10 <- get_SDA_property(
  property = "Rock Fragments 3 - 10 inches - Rep Value",  # equivalent to 'frag3to10_r'
  method = "Dominant Component (Numeric)", # dominant component, weighted average by depth
  areasymbols = "CA630",
  top_depth = 0,
  bottom_depth = 100  # depth range for averaging
)

head(frag_3_10)
```

    ##     mukey areasymbol musym
    ## 1 2403709      CA630  1012
    ## 2 2924914      CA630  1013
    ## 3 2924833      CA630  1090
    ## 4 2924834      CA630  1091
    ## 5 2924907      CA630   128
    ## 6 2924879      CA630   206
    ##                                                                   muname
    ## 1                                                             Mined land
    ## 2      Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
    ## 3 Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
    ## 4  Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
    ## 5                            Cogna loam, 0 to 2 percent slopes, overwash
    ## 6                               Pentz sandy loam, 2 to 15 percent slopes
    ##   frag3to10_r
    ## 1          NA
    ## 2       54.39
    ## 3       13.00
    ## 4        7.24
    ## 5        0.00
    ## 6        3.00

``` r
# get fragment volume (fragvoltot_l, fragvoltot_r, fragvoltot_h)
fragvol_total <- get_SDA_property(
  property = "fragvoltot_r",
  method = "Weighted Average", # weighted by component percentage and depth
  top_depth = 0,
  bottom_depth = 100,
  areasymbols = "CA630"
)

head(fragvol_total)
```

    ##     mukey areasymbol musym
    ## 1 2403709      CA630  1012
    ## 2 2924914      CA630  1013
    ## 3 2924833      CA630  1090
    ## 4 2924834      CA630  1091
    ## 5 2924907      CA630   128
    ## 6 2924879      CA630   206
    ##                                                                   muname
    ## 1                                                             Mined land
    ## 2      Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
    ## 3 Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
    ## 4  Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
    ## 5                            Cogna loam, 0 to 2 percent slopes, overwash
    ## 6                               Pentz sandy loam, 2 to 15 percent slopes
    ##   fragvoltot_r
    ## 1           NA
    ## 2     87.40000
    ## 3     41.07137
    ## 4     43.15400
    ## 5      0.00000
    ## 6     14.00000

**Note:** Fragment weight fraction properties in `chorizon` (like
`frag3to10_r` and `fraggt10_r`) are always populated, but currently the
`fragvoltot_*` columns are not and data availability depends on the
vintage of the specific SSURGO map unit data.

------------------------------------------------------------------------

## SDA Query Limits and Strategies

### SDA Query Limitations

The SDA system has important hard limits:

1.  **Record Limit**: Queries that return more than **100,000 records**
    are rejected

2.  **Result Size**: Response serialization limited to approximately
    **32 Mb**

3.  **Memory Limit**: Queries generally cannot exceed approximately
    **300 Mb** memory usage

4.  **Timeouts and Complexity**: Long-running queries may timeout or hit
    other internal limits during query planning

**Note on Local vs. Remote:** These limits primarily apply to the
**Remote SDA API**. When querying a **local SQLite database**
(e.g. created via
[`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
or
[`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)),
you are generally unrestricted by record counts or timeouts. Complex
tabular queries can often be run on entire states or the whole country
locally without the need for chunking or simplification.

### Strategies for Large Queries

1.  **Add WHERE clause** to reduce result set:

``` r
# Bad: will return >100k records across all of SSURGO
q <- "SELECT * FROM component"
   
# Good: specific component name pattern in WHERE clause
q <- "SELECT * FROM component 
      WHERE compname LIKE 'Miami%'"
```

2.  **Use TOP clause** to limit number of results (useful for testing):

``` r
# Test with small result set first
q <- "SELECT TOP 100 * FROM chorizon"
result <- SDA_query(q)
```

TOP can be used after sorting with `ORDER BY` to easily find the values
associated with an ordered sequence. For instance, if you
`ORDER BY comppct_r DESC` (order by component percentage in descending
order), then take `TOP 1` you will get the records associated with the
dominant component. This is a very common aggregation scheme available
as an option in most tools that use SSURGO data.

TOP is also generally useful for testing queries, or checking table
result structure for a few resulting rows. If testing is successful,
remove TOP, add filtering with WHERE or in JOIN condition (see below),
or increase the TOP limit.

3.  **Aggregate and apply JOIN constraints early** in the query:

Efficient queries often perform filtering operations as part of their
JOIN statements. This can improve server-side query planning and reduce
overhead that is not needed for the final result.

``` r
# Return aggregated results (fewer rows) instead of raw data
#  and constrain musym in join to mapunit instead of WHERE
q <- "
SELECT mu.mukey, 
  COUNT(*) AS n_hz,
  AVG(ch.claytotal_r) AS avg_clay
FROM component co
INNER JOIN mapunit mu ON mu.mukey = co.mukey AND mu.musym = '101'
INNER JOIN chorizon ch ON ch.cokey = co.cokey
GROUP BY mu.mukey
WHERE compname LIKE 'Musick%'
"
```

4.  **Explicitly** select the columns you need:

Efficient queries also are explicit about columns in the SELECT clause.

While `SELECT *` is fine to use for testing, it is prone to issues if
schemas change, as well as returning more data than is necessary in most
cases.

``` r
# `SELECT *` will return all columns, usually including many columns you do not need
q <- "SELECT TOP 1 *
      FROM mapunit
      INNER JOIN component ON mapunit.mukey = component.mukey
WHERE compname LIKE 'Musick%'
"
```

``` r
# Use `SELECT column1, column2, ...` for concise results, with no duplicated columns in complex joins
q <- "
SELECT TOP 1 mapunit.mukey, musym, muname, 
             compname, localphase, comppct_r, majcompflag, compkind, cokey
FROM mapunit
INNER JOIN component ON mapunit.mukey = component.mukey
WHERE compname LIKE 'Musick%'
"
```

If a column name occurs multiple times in different tables, you need
specify which one you want it to come from with
`<tablename>.<columname>` syntax.

If you do need all columns from a specific table use `<tablename>.*`
syntax, for example here we get all columns from `chfrags`, and specific
columns from `chorizon` and `component`:

``` r
q <- "
SELECT hzname, chfrags.*, component.cokey
FROM chfrags
INNER JOIN chorizon ON chorizon.chkey = chfrags.chkey
INNER JOIN component ON component.cokey = chorizon.cokey
WHERE compname LIKE 'Musick%'
"
```

5.  **Use
    [`makeChunks()`](http://ncss-tech.github.io/soilDB/reference/makeChunks.md)
    for large vectors** (see next section)

[`makeChunks()`](http://ncss-tech.github.io/soilDB/reference/makeChunks.md)
is a helper function in the \`soilDB\`\` package. It takes a vector of
values as input, and assigns each value a group (chunk) number. You can
then calculate the unique chunk numbers and iterate over them to process
data in bite-sized pieces. This is covered extensively in the next
section.

## SQL Dialect Handling

### T-SQL Syntax Specifics

#### NULL Handling

``` r
# Check and filter for non-NULL - SDA uses IS NULL / IS NOT NULL
q <- "SELECT TOP 10 * FROM chorizon WHERE claytotal_r IS NOT NULL"

# Use ISNULL() to provide defaults e.g. convert missing clay content to 0
q <- "SELECT TOP 10 chkey, claytotal_r, ISNULL(claytotal_r, 0) AS clay FROM chorizon"
```

#### Type Casting

``` r
# CAST for explicit type conversion
q <- "
SELECT TOP 10 
  claytotal_r,
  CAST(claytotal_r AS INT) AS clay_int,
  CAST(chkey AS CHAR(10)) AS chkey_str
FROM chorizon
WHERE claytotal_r != CAST(claytotal_r AS INT) 
"
```

**Note:** in example above, casting to INT is equivalent to truncating
the value by removing decimals, it does not follow the rounding rules
that R
[`round()`](https://rspatial.github.io/terra/reference/math-generics.html)
does, for example.

#### String Operations

``` r
# Wildcards with LIKE
q <- "SELECT * FROM mapunit WHERE muname LIKE '%clay%'"

# String concatenation with +
q <- "SELECT areasymbol + '-' + musym AS areamusym FROM mapunit"
```

------------------------------------------------------------------------

## Query Chunking for Large Datasets

### Why “Chunking”?

When you need data for many map units, components, soil survey areas, or
other entities, a single query can exceed SDA’s limits.

SDA is a shared public resource, so it is generally encouraged to make
requests that are as small and efficient as possible.

The
[`makeChunks()`](http://ncss-tech.github.io/soilDB/reference/makeChunks.md)
function divides a large vector into smaller chunks for iterative
querying.

Before implementing any chunked query approach, you should also be
certain that your query can run properly on a small set, for example
just single chunk, or a result that is truncated using a `TOP` clause.

### Using `makeChunks()`

Suppose you have 5000 map unit keys to query. We can break that set up
into chunks of 1000 map units, and iterate over each chunk using
[`lapply()`](https://rdrr.io/r/base/lapply.html)

``` r
large_mukey_list <- 461994:466995

# Divide into chunks of 1000
chunks <- soilDB::makeChunks(large_mukey_list, 1000)

# Now loop through chunks
results_list <- lapply(split(large_mukey_list, chunks), function(chunk_keys) {
  
  # Build IN clause
  in_clause <- soilDB::format_SQL_in_statement(chunk_keys) 
  
  # construct query 
  q <- sprintf(
    "SELECT mukey, musym, muname FROM mapunit WHERE mukey IN %s", in_clause
  )
    cat(sprintf("Chunk mukey %d to %d\n", min(chunk_keys), max(chunk_keys))) 
  SDA_query(q)
})
```

    ## Chunk mukey 461994 to 462993

    ## Chunk mukey 462994 to 463993

    ## Chunk mukey 463994 to 464993

    ## Chunk mukey 464994 to 465993

    ## Chunk mukey 465994 to 466993

    ## Chunk mukey 466994 to 466995

``` r
# Combine results
all_results <- do.call(rbind, results_list)
```

The above approach is readily converted to parallel processing, for
example using `future::future.lapply()`, or other custom iterator
functions that allow for progress reporting and other diagnostics.

If you do choose to use parallel processing, you should limit the number
of concurrent connections made to the API to a reasonable number, say, 2
to 5 requests at a time, with a maximum of about 10. You should also
implement proper error handling and retry mechanisms (see Handling
Errors section above).

### Example Chunked Property Query

Here we query soil properties for 6 soil survey areas in chunks of 2.

``` r
test_areasymbols <- c("CA067", "CA077", "CA632", "CA644", "CA630", "CA649")

# Chunk into groups of 5
chunks <- soilDB::makeChunks(test_areasymbols, 2)

results_list <- lapply(split(test_areasymbols, chunks), function(chunk_keys) {
  
  # Build IN clause
  in_clause <- soilDB::format_SQL_in_statement(chunk_keys) 
  
  q <- sprintf("
    SELECT
      mu.mukey,
      mu.musym,
      mu.muname,
      co.cokey,
      co.compname,
      SUM(hzdepb_r - hzdept_r) AS sum_hz_thickness
    FROM legend AS leg
    INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
    INNER JOIN component AS co ON co.mukey = mu.mukey
    INNER JOIN chorizon AS ch ON ch.cokey = co.cokey
    WHERE leg.areasymbol IN %s
    GROUP BY mu.mukey, mu.musym, mu.muname, co.cokey, co.compname
  ", in_clause)
  
  cat(sprintf("Chunk %s completed\n", paste0(chunk_keys, collapse = ", ")))
  SDA_query(q)
})
```

    ## Chunk CA067, CA077 completed

    ## Chunk CA632, CA644 completed

    ## Chunk CA630, CA649 completed

``` r
all_results <- do.call(rbind, results_list)
head(all_results)
```

    ##      mukey musym                                           muname    cokey
    ## 1.1 461845   101 Amador-Gillender complex, 2 to 15 percent slopes 26327297
    ## 1.2 461845   101 Amador-Gillender complex, 2 to 15 percent slopes 26327298
    ## 1.3 461845   101 Amador-Gillender complex, 2 to 15 percent slopes 26327299
    ## 1.4 461845   101 Amador-Gillender complex, 2 to 15 percent slopes 26327300
    ## 1.5 461845   101 Amador-Gillender complex, 2 to 15 percent slopes 26327301
    ## 1.6 461845   101 Amador-Gillender complex, 2 to 15 percent slopes 26327302
    ##       compname sum_hz_thickness
    ## 1.1     Amador               73
    ## 1.2  Gillender               43
    ## 1.3  Jennylind              200
    ## 1.4     Peters               63
    ## 1.5     Pardee              200
    ## 1.6 Ranchoseco              200

**Note:** The above sample query is **not** a good way to determine soil
thickness, as it does not exclude thickness of non-soil layers
(e.g. bedrock) that should not be counted as part of the soil depth. As
an exercise, use the logic from the previous sections to improve this
example to filter out bedrock.

### Recommended Chunk Sizes

- **Typical queries**: 1000-10000 items per chunk (safe for most
  queries)

- **Simple lookups**: 10000-100000 items per chunk (less data per item)

- **Complex joins**: 500-1000 items per chunk (more processing per item)

Monitor your query results and adjust based on SDA response times.

You may get an error:

> Invalid query: The query processor ran out of internal resources and
> could not produce a query plan. This is a rare event and only expected
> for extremely complex queries or queries that reference a very large
> number of tables or partitions. Please simplify the query. If you
> believe you have received this message in error, contact Customer
> Support Services for more information.

If you see this, first confirm that your query syntax is correct for the
result you are trying to obtain, for instance by trying it on a minimal
small set of data. Critically evaluate the need for joins, subqueries,
and complex filtering logic. Identify any areas of repeated logic and
refactor to simplify redundant steps. If your query works as expected on
a small data set, and you have optimized it, then consider breaking your
big query up into (more) chunks.

------------------------------------------------------------------------

## `get_SDA_property()`: Component and Horizon Properties

The
[`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)
function queries the “SSURGO On Demand” service to retrieve soil
component and horizon properties with automatic aggregation to the map
unit level.

By default
[`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)
includes minor components (`include_minors=TRUE`) but excludes
miscellaneous areas `miscellaneous_areas=FALSE`. Even miscellaneous
areas that have some soil data are excluded.

### Available Properties

SDA has 80+ horizon properties and 40+ component properties.

Here are some common examples.

**Component properties:**

- `"Taxonomic Order"`, `"Taxonomic Suborder"`,
  `"Taxonomic Temperature Regime"`
- `"Drainage Class"`, `"Hydrologic Group"`
- `"Corrosion of Steel"`, `"Corrosion of Concrete"`
- `"Range Production - Favorable/Normal/Unfavorable Year"`

**Horizon properties:**

- **Water**: `"Available Water Capacity - Rep Value"`,
  `"0.1 bar H2O - Rep Value"`, `"0.33 bar H2O - Rep Value"`,
  `"15 bar H2O - Rep Value"`

- **Bulk Density**: `"Bulk Density 0.1 bar H2O - Rep Value"`,
  `"Bulk Density 0.33 bar H2O - Rep Value"`,
  `"Bulk Density 15 bar H2O - Rep Value"`,
  `"Bulk Density oven dry - Rep Value"`

- **Texture**: `"Total Sand - Rep Value"`, `"Total Silt - Rep Value"`,
  `"Total Clay - Rep Value"`, and sand/silt sub-fractions

- **Chemistry**: `"pH 1:1 water - Rep Value"`,
  `"Electrical Conductivity - Rep Value"`,
  `"Cation Exchange Capacity - Rep Value"`,
  `"Sodium Adsorption Ratio - Rep Value"`,
  `"Calcium Carbonate - Rep Value"`, `"Gypsum - Rep Value"`

- **Other**: `"Saturated Hydraulic Conductivity - Rep Value"`,
  `"Organic Matter - Rep Value"`, `"Free Iron - Rep Value"`,
  `"Oxalate Iron - Rep Value"`

Alternately, you can directly use physical column names from the SSURGO
schema like `"claytotal_r"`, `"ph1to1h2o_r"`, `"ksat_r"`, etc.

### Method: Dominant Component (Category)

Dominant component (category) get the categorical value from the
dominant soil component. This query is a distinct method as categorical
data need to be handled differently from numeric data; the numeric
method is described below.

``` r
# Get taxonomic classification from dominant component
tax_order <- get_SDA_property(
  property = "Taxonomic Suborder",
  method = "Dominant Component (Category)",
  areasymbols = "CA630"
)

head(tax_order)
```

    ##     mukey areasymbol musym                                         muname
    ## 1 2924907      CA630   128    Cogna loam, 0 to 2 percent slopes, overwash
    ## 2 2924879      CA630   206       Pentz sandy loam, 2 to 15 percent slopes
    ## 3 2924880      CA630   207      Pentz sandy loam, 15 to 50 percent slopes
    ## 4 2924881      CA630   208 Pentz cobbly sandy loam, 2 to 8 percent slopes
    ## 5 2924882      CA630   209  Pentz-Bellota complex, 2 to 15 percent slopes
    ## 6 2924883      CA630   212             Peters clay, 2 to 8 percent slopes
    ##   taxsuborder
    ## 1     Xerolls
    ## 2     Xerolls
    ## 3     Xerolls
    ## 4     Xerolls
    ## 5     Xerolls
    ## 6     Xerolls

### Method: Dominant Component (Numeric)

Dominant component (numeric) retrieves properties from the dominant
component, optionally averaged over a depth range (in centimeters):

``` r
# Get bulk density at 0-25 cm depth
bulk_density <- get_SDA_property(
  property = c("dbthirdbar_l", "dbthirdbar_r", "dbthirdbar_h"),
  method = "Dominant Component (Numeric)",
  areasymbols = "CA630",
  top_depth = 0,
  bottom_depth = 25
)

head(bulk_density)
```

    ##     mukey areasymbol musym
    ## 1 2403709      CA630  1012
    ## 2 2924914      CA630  1013
    ## 3 2924833      CA630  1090
    ## 4 2924834      CA630  1091
    ## 5 2924907      CA630   128
    ## 6 2924879      CA630   206
    ##                                                                   muname
    ## 1                                                             Mined land
    ## 2      Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
    ## 3 Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
    ## 4  Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
    ## 5                            Cogna loam, 0 to 2 percent slopes, overwash
    ## 6                               Pentz sandy loam, 2 to 15 percent slopes
    ##   dbthirdbar_l dbthirdbar_r dbthirdbar_h
    ## 1           NA           NA           NA
    ## 2       1.5028       1.5416       1.5792
    ## 3       1.3288       1.4504       1.5724
    ## 4       1.4468       1.5216       1.5964
    ## 5       1.4300       1.4800       1.5400
    ## 6       1.3760       1.4800       1.5820

### Method: Weighted Average

The weighted average method retrieves component percentage and
depth-weighted average across all components in a map unit. In this
example we explicitly exclude minor components by changing the default
`include_minors` argument.

``` r
# Get weighted average clay content (0-50 cm)
clay_avg <- get_SDA_property(
  property = "Total Clay - Rep Value",
  method = "Weighted Average",
  areasymbols = "CA630",
  top_depth = 0,
  bottom_depth = 50,
  include_minors = FALSE
)

head(clay_avg)
```

    ##     mukey areasymbol musym
    ## 1 2403709      CA630  1012
    ## 2 2924914      CA630  1013
    ## 3 2924833      CA630  1090
    ## 4 2924834      CA630  1091
    ## 5 2924907      CA630   128
    ## 6 2924879      CA630   206
    ##                                                                   muname
    ## 1                                                             Mined land
    ## 2      Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
    ## 3 Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
    ## 4  Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
    ## 5                            Cogna loam, 0 to 2 percent slopes, overwash
    ## 6                               Pentz sandy loam, 2 to 15 percent slopes
    ##   claytotal_r
    ## 1          NA
    ## 2     2.58000
    ## 3    18.02000
    ## 4    20.40400
    ## 5    13.00000
    ## 6    14.47368

### Method: Min/Max

Method \`“Min/Max” is used to get minimum or maximum values across all
components in a mapunit.

``` r
# Get maximum sand content in any component
sand_max <- get_SDA_property(
  property = "Total Sand - Rep Value",
  method = "Min/Max",
  areasymbols = "CA630",
  FUN = "MAX",
  top_depth = 0,
  bottom_depth = 50
)

head(sand_max)
```

    ##     mukey areasymbol musym
    ## 1 1865918      CA630  3046
    ## 2 1865926      CA630  7088
    ## 3 1865927      CA630  7155
    ## 4 1865928      CA630  7156
    ## 5 1865929      CA630  8033
    ## 6 1865930      CA630  8034
    ##                                                          muname sandtotal_r
    ## 1   Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes          55
    ## 2       Loafercreek-Gopheridge complex, 30 to 60 percent slopes          50
    ## 3            Crimeahouse-Sixbit complex, 3 to 15 percent slopes          51
    ## 4           Crimeahouse-Sixbit complex, 15 to 30 percent slopes          58
    ## 5  Copperopolis-Whiterock complex, 2 to 8 percent slopes, rocky          45
    ## 6 Copperopolis-Whiterock complex, 3 to 15 percent slopes, rocky          45

### Method: Dominant Condition

Dominant condition aggregates components with the same category value
for the properties of interest, then picks the dominant percentage. This
gives extra weight to categories where multiple soils in the same map
unit have the same value.

``` r
# Get dominant drainage class condition
drain_dominant <- get_SDA_property(
  property = "Drainage Class",
  method = "Dominant Condition",
  areasymbols = "CA630"
)

head(drain_dominant)
```

    ##     mukey areasymbol musym
    ## 1 1865918      CA630  3046
    ## 2 1865926      CA630  7088
    ## 3 1865927      CA630  7155
    ## 4 1865928      CA630  7156
    ## 5 1865929      CA630  8033
    ## 6 1865930      CA630  8034
    ##                                                          muname
    ## 1   Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes
    ## 2       Loafercreek-Gopheridge complex, 30 to 60 percent slopes
    ## 3            Crimeahouse-Sixbit complex, 3 to 15 percent slopes
    ## 4           Crimeahouse-Sixbit complex, 15 to 30 percent slopes
    ## 5  Copperopolis-Whiterock complex, 2 to 8 percent slopes, rocky
    ## 6 Copperopolis-Whiterock complex, 3 to 15 percent slopes, rocky
    ##                drainagecl
    ## 1 Moderately well drained
    ## 2            Well drained
    ## 3            Well drained
    ## 4            Well drained
    ## 5            Well drained
    ## 6            Well drained

### Method: None

Method `"none"` returns one row per component or component horizon,
depending on the property selected, with no map unit aggregation. This
generally returns much more data, but allows for custom aggregation
outside of the SQL query, and deeper inspection of the source data.

You cannot mix component-level and horizon-level properties with this
method. Run it twice, once for horizon-level and once for
component-level, then join the component data to the horizon data using
the `cokey` column to get a single data frame.

Also, the `top_depth` and `bottom_depth` arguments are deliberately
ignored with this method. Apply your own filtering to the result if you
only need a specific depth range.

``` r
# Get all pH values by component and horizon
ph_all <- get_SDA_property(
  property = "pH 1:1 water - Rep Value",
  method = "None",
  areasymbols = "CA630"
)

head(ph_all)
```

    ##     mukey    cokey areasymbol musym
    ## 1 1865918 26411696      CA630  3046
    ## 2 1865918 26411696      CA630  3046
    ## 3 1865918 26411695      CA630  3046
    ## 4 1865918 26411695      CA630  3046
    ## 5 1865918 26411695      CA630  3046
    ## 6 1865918 26411698      CA630  3046
    ##                                                        muname          compname
    ## 1 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes          Goldwall
    ## 2 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes          Goldwall
    ## 3 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes            Toomes
    ## 4 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes            Toomes
    ## 5 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes            Toomes
    ## 6 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes Ultic Argixerolls
    ##             compkind comppct_r majcompflag    chkey hzdept_r hzdepb_r
    ## 1             Series        45         Yes 78806274        0       15
    ## 2             Series        45         Yes 78806273       15      200
    ## 3             Series        28         Yes 78806272        0        2
    ## 4             Series        28         Yes 78806271        2       33
    ## 5             Series        28         Yes 78806270       33      200
    ## 6 Taxon above family         5         No  78806281        0       25
    ##   ph1to1h2o_r
    ## 1         5.5
    ## 2          NA
    ## 3         5.2
    ## 4         5.5
    ## 5          NA
    ## 6         6.0

### Inspecting Generated Queries

We can use `query_string = TRUE` to see the SQL generated by
[`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md):

``` r
q <- get_SDA_property(
  property = "Taxonomic Suborder",
  method = "Dominant Component (Category)",
  areasymbols = "CA630",
  query_string = TRUE
)

# View first 500 characters of query
cat(substr(q, 1, 500), "...\n")
```

    ## SELECT mapunit.mukey, areasymbol, musym, muname, taxsuborder AS taxsuborder
    ##              FROM legend
    ##              INNER JOIN mapunit ON mapunit.lkey = legend.lkey AND legend.areasymbol IN ('CA630')
    ##              INNER JOIN component ON component.mukey = mapunit.mukey AND
    ##                                      component.cokey = (SELECT TOP 1 c1.cokey FROM component AS c1
    ##                                                 INNER JOIN mapunit AS mu ON component.mukey = mu.mukey AND
    ##                         ...

------------------------------------------------------------------------

## `get_SDA_interpretation()`: Soil Interpretations

Soil interpretations are ratings that assess how suitable or limited a
soil is for various land uses. An interpretation for a specific
component or map unit includes a rating class, and a numeric value
(“fuzzy rating”). The
[`get_SDA_interpretation()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_interpretation.md)
function retrieves these ratings with multiple aggregation methods.

### Available Interpretations

SDA contains 600+ interpretations organized by category:

| Category                                  | Examples                                                                                   |
|-------------------------------------------|--------------------------------------------------------------------------------------------|
| `FOR` (Forestry)                          | Potential Seedling Mortality, Road Suitability, Harvest Suitability, Equipment Operability |
| `AGR` (Agriculture)                       | Crop Yield, Irrigation Suitability, Pesticide Pollution Potential                          |
| `ENG` (Engineering)                       | Septic Tank Absorption Fields, Construction Roads, Dwelling Foundations, Dam Sites         |
| `URB/REC` (Urban/Recreation)              | Trails, Playgrounds, Picnic Areas, Recreational Development                                |
| `AWM` (Animal Waste Management)           | Wastewater Application Sites, Manure Storage                                               |
| `WMS` (Water Management System)           | Irrigation Suitability, Drainage, Wetland Rating                                           |
| `CPI` (Commodity Crop Productivity Index) | National Commodity Crop Productivity Index                                                 |

### Method: Dominant Component

Get the rating from the dominant soil component:

``` r
# Get forestry ratings for dominant component
for_ratings <- get_SDA_interpretation(
  rulename = c("FOR - Potential Seedling Mortality",
               "FOR - Road Suitability (Natural Surface)"),
  method = "Dominant Component",
  areasymbols = "CA630"
)

head(for_ratings)
```

    ##     mukey    cokey areasymbol musym
    ## 1 2924907 26412202      CA630   128
    ## 2 2924879 26412137      CA630   206
    ## 3 2924880 26412145      CA630   207
    ## 4 2924881 26412157      CA630   208
    ## 5 2924882 26412161      CA630   209
    ## 6 2924883 26412164      CA630   212
    ##                                           muname compname compkind comppct_r
    ## 1    Cogna loam, 0 to 2 percent slopes, overwash    Cogna   Series        85
    ## 2       Pentz sandy loam, 2 to 15 percent slopes    Pentz   Series        85
    ## 3      Pentz sandy loam, 15 to 50 percent slopes    Pentz   Series        85
    ## 4 Pentz cobbly sandy loam, 2 to 8 percent slopes    Pentz   Series        85
    ## 5  Pentz-Bellota complex, 2 to 15 percent slopes    Pentz   Series        55
    ## 6             Peters clay, 2 to 8 percent slopes   Peters   Series        85
    ##   majcompflag rating_FORPotentialSeedlingMortality
    ## 1         Yes                                0.660
    ## 2         Yes                                0.903
    ## 3         Yes                                0.935
    ## 4         Yes                                0.977
    ## 5         Yes                                0.929
    ## 6         Yes                                0.845
    ##   class_FORPotentialSeedlingMortality
    ## 1                            Moderate
    ## 2                            Moderate
    ## 3                            Moderate
    ## 4                            Moderate
    ## 5                            Moderate
    ## 6                            Moderate
    ##                                       reason_FORPotentialSeedlingMortality
    ## 1 FOR - Available Water Limitation "Moderately low moisture supply" (0.66)
    ## 2           FOR - Available Water Limitation "Moisture supply low" (0.903)
    ## 3           FOR - Available Water Limitation "Moisture supply low" (0.935)
    ## 4           FOR - Available Water Limitation "Moisture supply low" (0.977)
    ## 5           FOR - Available Water Limitation "Moisture supply low" (0.929)
    ## 6           FOR - Available Water Limitation "Moisture supply low" (0.845)
    ##   rating_FORRoadSuitabilityNaturalSurface
    ## 1                                   0.039
    ## 2                                   0.763
    ## 3                                   1.000
    ## 4                                   0.080
    ## 5                                   0.215
    ## 6                                   1.000
    ##   class_FORRoadSuitabilityNaturalSurface
    ## 1                            Well suited
    ## 2                      Moderately suited
    ## 3                          Poorly suited
    ## 4                            Well suited
    ## 5                            Well suited
    ## 6                          Poorly suited
    ##                                                                                       reason_FORRoadSuitabilityNaturalSurface
    ## 1                                                                        FOR - Strength Limitation (2) "Low strength" (0.039)
    ## 2                                                                        FOR - Slope Limitation (<6% to >12%) "Slope" (0.763)
    ## 3                                                                            FOR - Slope Limitation (<6% to >12%) "Slope" (1)
    ## 4           FOR - Rock Fragments Limitation (1) "Rock fragments" (0.08); FOR - Slope Limitation (<6% to >12%) "Slope" (0.054)
    ## 5                  FOR - Slope Limitation (<6% to >12%) "Slope" (0.215); FOR - Strength Limitation (2) "Low strength" (0.055)
    ## 6 FOR - Strength Limitation (2) "Low strength" (1); FOR - Stickiness Limitation (1) "Stickiness; high plasticity index" (0.5)

### Method: Dominant Condition

Aggregate similar ratings, then return the dominant:

``` r
# Get dominant engineering interpretation
eng_ratings <- get_SDA_interpretation(
  rulename = "ENG - Septic Tank Absorption Fields",
  method = "Dominant Condition",
  areasymbols = "CA649"
)

head(eng_ratings)
```

    ##    mukey areasymbol musym
    ## 1 463229      CA649   AaC
    ## 2 463230      CA649   AaD
    ## 3 463231      CA649   AbE
    ## 4 463232      CA649   AcE
    ## 5 463233      CA649   AdG
    ## 6 463234      CA649   AeD
    ##                                                            muname
    ## 1                      Ahwahnee sandy loam, 2 to 9 percent slopes
    ## 2                     Ahwahnee sandy loam, 9 to 15 percent slopes
    ## 3           Ahwahnee-Auberry sandy loams, 15 to 30 percent slopes
    ## 4      Ahwahnee-Auberry rocky sandy loams, 9 to 30 percent slopes
    ## 5 Ahwahnee-Auberry very rocky sandy loams, 30 to 75 percent slope
    ## 6                      Auberry sandy loam, 9 to 15 percent slopes
    ##   rating_ENGSepticTankAbsorptionFields
    ## 1                                    1
    ## 2                                    1
    ## 3                                    1
    ## 4                                    1
    ## 5                                    1
    ## 6                                    1
    ##   total_comppct_ENGSepticTankAbsorptionFields
    ## 1                                          85
    ## 2                                          85
    ## 3                                          90
    ## 4                                          90
    ## 5                                          88
    ## 6                                          85
    ##   class_ENGSepticTankAbsorptionFields
    ## 1                        Very limited
    ## 2                        Very limited
    ## 3                        Very limited
    ## 4                        Very limited
    ## 5                        Very limited
    ## 6                        Very limited
    ##                                                                                                                     reason_ENGSepticTankAbsorptionFields
    ## 1                                    Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Seepage Bottom Layer, Not Aridic "Seepage, bottom layer" (1)
    ## 2  Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Seepage Bottom Layer, Not Aridic "Seepage, bottom layer" (1); Slope 8 to > 15% "Slope" (0.633)
    ## 3      Slope 8 to > 15% "Slope" (1); Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Seepage Bottom Layer, Not Aridic "Seepage, bottom layer" (1)
    ## 4      Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Slope 8 to > 15% "Slope" (1); Seepage Bottom Layer, Not Aridic "Seepage, bottom layer" (1)
    ## 5      Slope 8 to > 15% "Slope" (1); Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Seepage Bottom Layer, Not Aridic "Seepage, bottom layer" (1)
    ## 6 Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Slope 8 to > 15% "Slope" (0.633); Percolation 60 - 180cm (24-72") "Slow water movement" (0.498)

### Method: Weighted Average

Component-percentage-weighted average of ratings:

``` r
# Get weighted average agricultural rating
agr_weighted <- get_SDA_interpretation(
  rulename = "AGR - Winter Wheat Yield (MT)",
  method = "Weighted Average",
  areasymbols = "MT041",
  include_minors = TRUE
)

head(agr_weighted)
```

    ##   areasymbol musym                                       muname  mukey
    ## 1      MT041   13A         McKenzie clay, 0 to 1 percent slopes 343852
    ## 2      MT041   16B          Degrand loam, 0 to 4 percent slopes 343858
    ## 3      MT041   22E  Hillon-Joplin loams, 8 to 25 percent slopes 343880
    ## 4      MT041   22F         Hillon loam, 15 to 60 percent slopes 343881
    ## 5      MT041   24A Hanly loamy fine sand, 0 to 2 percent slopes 343883
    ## 6      MT041   27B          Attewan loam, 0 to 4 percent slopes 343891
    ##   rating_AGRWinterWheatYieldMT class_AGRWinterWheatYieldMT
    ## 1                         0.02                        <NA>
    ## 2                         0.25                        <NA>
    ## 3                         0.26                        <NA>
    ## 4                         0.05                        <NA>
    ## 5                         0.22                        <NA>
    ## 6                         0.25                        <NA>
    ##                                                                                                                                                                                                                                              reason_AGRWinterWheatYieldMT
    ## 1                                                                                                                                     -UNKNOWN- (0.301); -UNKNOWN- (0.107); -UNKNOWN- (0.107); -UNKNOWN- (0.107); -UNKNOWN- (0.041); -UNKNOWN- (0.107); -UNKNOWN- (0.107)
    ## 2                                                                            -UNKNOWN- (0.206); -UNKNOWN- (0.107); -UNKNOWN- (0.238); -UNKNOWN- (0.107); -UNKNOWN- (0.256); -UNKNOWN- (0.107); -UNKNOWN- (0.219); -UNKNOWN- (0.107); -UNKNOWN- (0.205); -UNKNOWN- (0.107)
    ## 3 -UNKNOWN- (0.281); -UNKNOWN- (0.107); -UNKNOWN- (0.223); -UNKNOWN- (0.107); -UNKNOWN- (0.27); -UNKNOWN- (0.107); -UNKNOWN- (0.237); -UNKNOWN- (0.107); -UNKNOWN- (0.107); -UNKNOWN- (0.098); -UNKNOWN- (0.197); -UNKNOWN- (0.107); -UNKNOWN- (0.231); -UNKNOWN- (0.107)
    ## 4                                                         -UNKNOWN- (0.231); -UNKNOWN- (0.107); -UNKNOWN- (0.107); -UNKNOWN- (0.188); -UNKNOWN- (0.107); -UNKNOWN- (0.281); -UNKNOWN- (0.107); -UNKNOWN- (0.107); -UNKNOWN- (0.223); -UNKNOWN- (0.107); -UNKNOWN- (0.107)
    ## 5                                                                            -UNKNOWN- (0.234); -UNKNOWN- (0.107); -UNKNOWN- (0.196); -UNKNOWN- (0.107); -UNKNOWN- (0.164); -UNKNOWN- (0.107); -UNKNOWN- (0.196); -UNKNOWN- (0.107); -UNKNOWN- (0.196); -UNKNOWN- (0.107)
    ## 6                                                                                               -UNKNOWN- (0.133); -UNKNOWN- (0.107); -UNKNOWN- (0.164); -UNKNOWN- (0.107); -UNKNOWN- (0.107); -UNKNOWN- (0.219); -UNKNOWN- (0.107); -UNKNOWN- (0.219); -UNKNOWN- (0.107)

**Note:** in this example, a regional (Montana-specific) interpretation
is used, so we used a Montana areasymbol (MT041). Some interpretations
are only exported for specific states, regions, or soil survey areas.

### Method: None

Return one row per component with no map unit aggregation:

``` r
# Get all component-level ratings
all_ratings <- get_SDA_interpretation(
  rulename = "FOR - Mechanical Planting Suitability",
  method = "None",
  areasymbols = "CA630"
)

head(all_ratings)
```

    ##     mukey    cokey areasymbol musym                                      muname
    ## 1 2924907 26412197      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash
    ## 2 2924907 26412198      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash
    ## 3 2924907 26412199      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash
    ## 4 2924907 26412200      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash
    ## 5 2924907 26412201      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash
    ## 6 2924907 26412202      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash
    ##     compname   compkind comppct_r majcompflag
    ## 1   Columbia     Series         1         No 
    ## 2    Veritas     Series         3         No 
    ## 3 Archerdale     Series         6         No 
    ## 4       Nord Taxadjunct         4         No 
    ## 5     Honcut     Series         1         No 
    ## 6      Cogna     Series        85         Yes
    ##   rating_FORMechanicalPlantingSuitability
    ## 1                                    0.00
    ## 2                                    0.00
    ## 3                                    0.75
    ## 4                                    0.00
    ## 5                                    0.00
    ## 6                                    0.00
    ##   class_FORMechanicalPlantingSuitability
    ## 1                            Well suited
    ## 2                            Well suited
    ## 3                          Poorly suited
    ## 4                            Well suited
    ## 5                            Well suited
    ## 6                            Well suited
    ##                                                                                                           reason_FORMechanicalPlantingSuitability
    ## 1                                                                                                                                            <NA>
    ## 2                                                                                                                                            <NA>
    ## 3 Stickiness Limitation (4) (Updated) "Stickiness; high plasticity index" (0.75); Rock Fragments Limitation (5) (Updated) "Rock fragments" (0.55)
    ## 4                                                                                                                                            <NA>
    ## 5                                                                                                                                            <NA>
    ## 6                                                                                                                                            <NA>

### Understanding Output Columns

Use `wide_reason = TRUE` to pivot reason columns into separate sub-rule
columns:

``` r
# Get detailed ratings with reasons pivoted
detailed <- get_SDA_interpretation(
  rulename = "FOR - Mechanical Planting Suitability",
  method = "Dominant Component",
  areasymbols = "CA630",
  wide_reason = TRUE
)

head(detailed)
```

    ##     mukey    cokey areasymbol musym
    ## 1 2924907 26412202      CA630   128
    ## 2 2924879 26412137      CA630   206
    ## 3 2924880 26412145      CA630   207
    ## 4 2924881 26412157      CA630   208
    ## 5 2924882 26412161      CA630   209
    ## 6 2924883 26412164      CA630   212
    ##                                           muname compname compkind comppct_r
    ## 1    Cogna loam, 0 to 2 percent slopes, overwash    Cogna   Series        85
    ## 2       Pentz sandy loam, 2 to 15 percent slopes    Pentz   Series        85
    ## 3      Pentz sandy loam, 15 to 50 percent slopes    Pentz   Series        85
    ## 4 Pentz cobbly sandy loam, 2 to 8 percent slopes    Pentz   Series        85
    ## 5  Pentz-Bellota complex, 2 to 15 percent slopes    Pentz   Series        55
    ## 6             Peters clay, 2 to 8 percent slopes   Peters   Series        85
    ##   majcompflag rating_FORMechanicalPlantingSuitability
    ## 1         Yes                                   0.000
    ## 2         Yes                                   0.620
    ## 3         Yes                                   0.987
    ## 4         Yes                                   1.000
    ## 5         Yes                                   0.620
    ## 6         Yes                                   0.750
    ##   class_FORMechanicalPlantingSuitability
    ## 1                            Well suited
    ## 2                          Poorly suited
    ## 3                               Unsuited
    ## 4                               Unsuited
    ## 5                          Poorly suited
    ## 6                          Poorly suited
    ##                                                                                          reason_FORMechanicalPlantingSuitability
    ## 1                                                                                                                           <NA>
    ## 2                  Rock Fragments Limitation (5) (Updated) "Rock fragments" (0.62); Slope Limitation (<5% to >35%) "Slope" (0.4)
    ## 3                Slope Limitation (<5% to >35%) "Slope" (0.987); Rock Fragments Limitation (5) (Updated) "Rock fragments" (0.62)
    ## 4                   Rock Fragments Limitation (5) (Updated) "Rock fragments" (1); Slope Limitation (<5% to >35%) "Slope" (0.112)
    ## 5                Rock Fragments Limitation (5) (Updated) "Rock fragments" (0.62); Slope Limitation (<5% to >35%) "Slope" (0.203)
    ## 6 Stickiness Limitation (4) (Updated) "Stickiness; high plasticity index" (0.75); Slope Limitation (<5% to >35%) "Slope" (0.002)
    ##   rating_reason_FORMechanicalPlantingSuitability_Notrated
    ## 1                                                      NA
    ## 2                                                      NA
    ## 3                                                      NA
    ## 4                                                      NA
    ## 5                                                      NA
    ## 6                                                      NA
    ##   rating_reason_FORMechanicalPlantingSuitability_RockFragmentsLimitation5Updated
    ## 1                                                                           <NA>
    ## 2                                                                           0.62
    ## 3                                                                           0.62
    ## 4                                                                              1
    ## 5                                                                           0.62
    ## 6                                                                           <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_SlopeLimitationLT5toGT35
    ## 1                                                                    <NA>
    ## 2                                                                     0.4
    ## 3                                                                   0.987
    ## 4                                                                   0.112
    ## 5                                                                   0.203
    ## 6                                                                   0.002
    ##   rating_reason_FORMechanicalPlantingSuitability_StickinessLimitation4UpdatedStickiness
    ## 1                                                                                  <NA>
    ## 2                                                                                  <NA>
    ## 3                                                                                  <NA>
    ## 4                                                                                  <NA>
    ## 5                                                                                  <NA>
    ## 6                                                                                  <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_highplasticityindex075
    ## 1                                                                  <NA>
    ## 2                                                                  <NA>
    ## 3                                                                  <NA>
    ## 4                                                                  <NA>
    ## 5                                                                  <NA>
    ## 6                                                                  <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_SandLimitation3Updated
    ## 1                                                                  <NA>
    ## 2                                                                  <NA>
    ## 3                                                                  <NA>
    ## 4                                                                  <NA>
    ## 5                                                                  <NA>
    ## 6                                                                  <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_ComponentKindMiscandCHIIDisnull
    ## 1                                                                           <NA>
    ## 2                                                                           <NA>
    ## 3                                                                           <NA>
    ## 4                                                                           <NA>
    ## 5                                                                           <NA>
    ## 6                                                                           <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_NullHorizonData
    ## 1                                                           <NA>
    ## 2                                                           <NA>
    ## 3                                                           <NA>
    ## 4                                                           <NA>
    ## 5                                                           <NA>
    ## 6                                                           <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_RestrictiveLayerLimitation2Updated
    ## 1                                                                              <NA>
    ## 2                                                                              <NA>
    ## 3                                                                              <NA>
    ## 4                                                                              <NA>
    ## 5                                                                              <NA>
    ## 6                                                                              <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_highplasticityindex008
    ## 1                                                                  <NA>
    ## 2                                                                  <NA>
    ## 3                                                                  <NA>
    ## 4                                                                  <NA>
    ## 5                                                                  <NA>
    ## 6                                                                  <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_highplasticityindex0224
    ## 1                                                                   <NA>
    ## 2                                                                   <NA>
    ## 3                                                                   <NA>
    ## 4                                                                   <NA>
    ## 5                                                                   <NA>
    ## 6                                                                   <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_highplasticityindex0154
    ## 1                                                                   <NA>
    ## 2                                                                   <NA>
    ## 3                                                                   <NA>
    ## 4                                                                   <NA>
    ## 5                                                                   <NA>
    ## 6                                                                   <NA>
    ##   rating_reason_FORMechanicalPlantingSuitability_highplasticityindex0358
    ## 1                                                                   <NA>
    ## 2                                                                   <NA>
    ## 3                                                                   <NA>
    ## 4                                                                   <NA>
    ## 5                                                                   <NA>
    ## 6                                                                   <NA>

------------------------------------------------------------------------

## `get_SDA_hydric()`: Hydric Soil Classifications

Hydric soils are inundated or saturated long enough during the growing
season to develop anaerobic conditions. The
[`get_SDA_hydric()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_hydric.md)
function evaluates the proportion of hydric components in a map unit.

### Method: MAPUNIT (Default)

Returns an overall classification for each map unit:

``` r
hydric_class <- get_SDA_hydric(
  areasymbols = c("CA077", "CA630"),
  method = "MAPUNIT"
)

head(hydric_class)
```

    ##    mukey areasymbol musym
    ## 1 461993      CA077   101
    ## 2 461994      CA077   102
    ## 3 461995      CA077   103
    ## 4 461996      CA077   104
    ## 5 461998      CA077   106
    ## 6 461999      CA077   107
    ##                                                               muname
    ## 1                           Acampo sandy loam, 0 to 2 percent slopes
    ## 2                                  Alamo clay, 0 to 2 percent slopes
    ## 3               Alo-Vaquero complex, 8 to 30 percent slopes, MLRA 15
    ## 4              Alo-Vaquero complex, 30 to 50 percent slopes, MLRA 15
    ## 5 Archerdale very fine sandy loam, 0 to 2 percent slopes, overwashed
    ## 6                        Archerdale clay loam, 0 to 2 percent slopes
    ##   total_comppct hydric_majors hydric_inclusions           HYDRIC_RATING
    ## 1           100             0                 4 Predominantly Nonhydric
    ## 2           100            85                 8    Predominantly Hydric
    ## 3           100             0                 0               Nonhydric
    ## 4           100             0                 0               Nonhydric
    ## 5           100             0                 4 Predominantly Nonhydric
    ## 6           100             0                 0               Nonhydric

``` r
# Check unique ratings
unique(hydric_class$HYDRIC_RATING)
```

    ## [1] "Predominantly Nonhydric" "Predominantly Hydric"   
    ## [3] "Nonhydric"               "Hydric"                 
    ## [5] "Partially Hydric"

Possible classifications:

- `"Nonhydric"` - No hydric components

- `"Hydric"` - All major components are hydric

- `"Predominantly Hydric"` - Hydric components \>= 50%

- `"Partially Hydric"` - One or more major components are hydric

- `"Predominantly Nonhydric"` - Hydric components \< 50%

The output includes:

- `hydric_majors` - Percentage of hydric major components

- `hydric_inclusions` - Percentage of hydric minor/inclusion components

- `total_comppct` - Total component percentage (should sum to 100)

### Method: DOMINANT COMPONENT

Get hydric status of the dominant component only:

``` r
hydric_dom <- get_SDA_hydric(
  areasymbols = "CA630",
  method = "DOMINANT COMPONENT"
)

head(hydric_dom)
```

    ##   areasymbol musym                                         muname   mukey
    ## 1      CA630   128    Cogna loam, 0 to 2 percent slopes, overwash 2924907
    ## 2      CA630   206       Pentz sandy loam, 2 to 15 percent slopes 2924879
    ## 3      CA630   207      Pentz sandy loam, 15 to 50 percent slopes 2924880
    ## 4      CA630   208 Pentz cobbly sandy loam, 2 to 8 percent slopes 2924881
    ## 5      CA630   209  Pentz-Bellota complex, 2 to 15 percent slopes 2924882
    ## 6      CA630   212             Peters clay, 2 to 8 percent slopes 2924883
    ##      cokey compname compkind comppct_r majcompflag hydricrating
    ## 1 26412202    Cogna   Series        85         Yes           No
    ## 2 26412137    Pentz   Series        85         Yes           No
    ## 3 26412145    Pentz   Series        85         Yes           No
    ## 4 26412157    Pentz   Series        85         Yes           No
    ## 5 26412161    Pentz   Series        55         Yes           No
    ## 6 26412164   Peters   Series        85         Yes           No

### Method: DOMINANT CONDITION

Aggregate by hydric condition (Yes/No), then pick the dominant:

``` r
hydric_cond <- get_SDA_hydric(
  mukeys = c(461994, 461995, 462205),
  method = "DOMINANT CONDITION"
)

head(hydric_cond)
```

    ##   areasymbol musym
    ## 1      CA077   102
    ## 2      CA077   103
    ## 3      CA624   AzE
    ##                                                                   muname  mukey
    ## 1                                      Alamo clay, 0 to 2 percent slopes 461994
    ## 2                   Alo-Vaquero complex, 8 to 30 percent slopes, MLRA 15 461995
    ## 3 Auburn cobbly clay loam, heavy subsoil variant, 9 to 50 percent slopes 462205
    ##   hydricrating
    ## 1          Yes
    ## 2           No
    ## 3           No

### Method: NONE

Return one row per component (component-level hydric status):

``` r
hydric_all <- get_SDA_hydric(
  areasymbols = "CA630",
  method = "NONE"
)

head(hydric_all)
```

    ##   areasymbol musym                                      muname   mukey    cokey
    ## 1      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash 2924907 26412197
    ## 2      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash 2924907 26412198
    ## 3      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash 2924907 26412199
    ## 4      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash 2924907 26412200
    ## 5      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash 2924907 26412201
    ## 6      CA630   128 Cogna loam, 0 to 2 percent slopes, overwash 2924907 26412202
    ##     compname   compkind comppct_r majcompflag hydricrating
    ## 1   Columbia     Series         1         No            No
    ## 2    Veritas     Series         3         No            No
    ## 3 Archerdale     Series         6         No            No
    ## 4       Nord Taxadjunct         4         No            No
    ## 5     Honcut     Series         1         No            No
    ## 6      Cogna     Series        85         Yes           No

------------------------------------------------------------------------

## `get_SDA_muaggatt()`: Map Unit Aggregate Attributes

Map unit aggregate attributes are pre-computed statistics summarizing
the components in a map unit. These include surface texture, slope
range, permeability, and other physical properties.

``` r
muagg <- get_SDA_muaggatt(
  areasymbols = "CA630"
)

head(muagg)
```

    ##     mukey musym                                         muname mustatus
    ## 1 2924907   128    Cogna loam, 0 to 2 percent slopes, overwash     <NA>
    ## 2 2924879   206       Pentz sandy loam, 2 to 15 percent slopes     <NA>
    ## 3 2924880   207      Pentz sandy loam, 15 to 50 percent slopes     <NA>
    ## 4 2924881   208 Pentz cobbly sandy loam, 2 to 8 percent slopes     <NA>
    ## 5 2924882   209  Pentz-Bellota complex, 2 to 15 percent slopes     <NA>
    ## 6 2924883   212             Peters clay, 2 to 8 percent slopes     <NA>
    ##   slopegraddcp slopegradwta brockdepmin wtdepannmin wtdepaprjunmin flodfreqdcd
    ## 1            1          1.0          NA          NA             NA        Rare
    ## 2           10          9.3          38          NA             NA        None
    ## 3           33         29.7          38          NA             NA        None
    ## 4            7          6.6          33          NA             NA        None
    ## 5            8          6.6          36          NA             NA        None
    ## 6            5          5.1          38          NA             NA        None
    ##   flodfreqmax pondfreqprs aws025wta aws050wta aws0100wta aws0150wta
    ## 1        Rare           0      3.52      7.05      14.72      22.92
    ## 2        None           0      3.17      5.03       5.22       5.22
    ## 3        None           0      3.15      4.99       5.14       5.14
    ## 4        None           0      2.84      3.94       4.04       4.04
    ## 5        None           0      3.14      5.20       6.27       6.51
    ## 6        None           0      3.56      5.47       5.62       5.62
    ##     drclassdcd drclasswettest hydgrpdcd iccdcd iccdcdpct niccdcd niccdcdpct
    ## 1 Well drained   Well drained         B      1        89       3        100
    ## 2 Well drained   Well drained         D      7        85       7         85
    ## 3 Well drained   Well drained         D      7        85       7         85
    ## 4 Well drained   Well drained         D      7        85       7         85
    ## 5 Well drained   Well drained         D      7        55       7         55
    ## 6 Well drained   Well drained         D      6        92       6         92
    ##         engdwobdcd    engdwbdcd         engdwbll     engdwbml       engstafdcd
    ## 1     Very limited Very limited      Not limited Very limited Somewhat limited
    ## 2 Somewhat limited Very limited Somewhat limited Very limited     Very limited
    ## 3     Very limited Very limited        Not rated Very limited     Very limited
    ## 4 Somewhat limited Very limited     Very limited Very limited     Very limited
    ## 5 Somewhat limited Very limited Somewhat limited Very limited     Very limited
    ## 6     Very limited Very limited     Very limited Very limited     Very limited
    ##          engstafll    engstafml     engsldcd     engsldcp        englrsdcd
    ## 1 Somewhat limited Very limited Very limited Very limited Somewhat limited
    ## 2     Very limited Very limited Very limited Very limited Somewhat limited
    ## 3        Not rated Very limited Very limited Very limited     Very limited
    ## 4     Very limited Very limited Very limited Very limited Somewhat limited
    ## 5     Very limited Very limited Very limited Very limited Somewhat limited
    ## 6     Very limited Very limited Very limited Very limited     Very limited
    ##   engcmssdcd engcmssmp      urbrecptdcd urbrecptwta             forpehrtdcp
    ## 1       Poor      Fair Somewhat limited       0.248   Erosion hazard slight
    ## 2       Fair      Fair Somewhat limited       0.106 Erosion hazard moderate
    ## 3       Fair Not rated     Very limited       0.924   Erosion hazard severe
    ## 4       Fair      Fair Somewhat limited       0.122   Erosion hazard slight
    ## 5       Poor      Poor Somewhat limited       0.350 Erosion hazard moderate
    ## 6       Poor      Fair Somewhat limited       0.505 Erosion hazard moderate
    ##   hydclprs awmmfpwwta
    ## 1        0      0.102
    ## 2        2      1.000
    ## 3        0      1.000
    ## 4        2      1.000
    ## 5        0      0.996
    ## 6        3      1.000

``` r
str(muagg)
```

    ## 'data.frame':    129 obs. of  40 variables:
    ##  $ mukey         : int  2924907 2924879 2924880 2924881 2924882 2924883 2924913 2924908 2924884 2924885 ...
    ##  $ musym         : chr  "128" "206" "207" "208" ...
    ##  $ muname        : chr  "Cogna loam, 0 to 2 percent slopes, overwash" "Pentz sandy loam, 2 to 15 percent slopes" "Pentz sandy loam, 15 to 50 percent slopes" "Pentz cobbly sandy loam, 2 to 8 percent slopes" ...
    ##  $ mustatus      : chr  NA NA NA NA ...
    ##  $ slopegraddcp  : num  1 10 33 7 8 5 1 1 3 8 ...
    ##  $ slopegradwta  : num  1 9.3 29.7 6.6 6.6 5.1 1.1 1.1 3.9 6.8 ...
    ##  $ brockdepmin   : int  NA 38 38 33 36 38 NA NA 35 31 ...
    ##  $ wtdepannmin   : int  NA NA NA NA NA NA 43 NA NA NA ...
    ##  $ wtdepaprjunmin: int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ flodfreqdcd   : chr  "Rare" "None" "None" "None" ...
    ##  $ flodfreqmax   : chr  "Rare" "None" "None" "None" ...
    ##  $ pondfreqprs   : chr  "0" "0" "0" "0" ...
    ##  $ aws025wta     : num  3.52 3.17 3.15 2.84 3.14 3.56 3 3.96 3.39 3.26 ...
    ##  $ aws050wta     : num  7.05 5.03 4.99 3.94 5.2 5.47 5.51 7.98 5.21 4.49 ...
    ##  $ aws0100wta    : num  14.72 5.22 5.14 4.04 6.27 ...
    ##  $ aws0150wta    : num  22.92 5.22 5.14 4.04 6.51 ...
    ##  $ drclassdcd    : chr  "Well drained" "Well drained" "Well drained" "Well drained" ...
    ##  $ drclasswettest: chr  "Well drained" "Well drained" "Well drained" "Well drained" ...
    ##  $ hydgrpdcd     : chr  "B" "D" "D" "D" ...
    ##  $ iccdcd        : chr  "1" "7" "7" "7" ...
    ##  $ iccdcdpct     : int  89 85 85 85 55 92 85 74 60 88 ...
    ##  $ niccdcd       : chr  "3" "7" "7" "7" ...
    ##  $ niccdcdpct    : int  100 85 85 85 55 92 85 95 60 88 ...
    ##  $ engdwobdcd    : chr  "Very limited" "Somewhat limited" "Very limited" "Somewhat limited" ...
    ##  $ engdwbdcd     : chr  "Very limited" "Very limited" "Very limited" "Very limited" ...
    ##  $ engdwbll      : chr  "Not limited" "Somewhat limited" "Not rated" "Very limited" ...
    ##  $ engdwbml      : chr  "Very limited" "Very limited" "Very limited" "Very limited" ...
    ##  $ engstafdcd    : chr  "Somewhat limited" "Very limited" "Very limited" "Very limited" ...
    ##  $ engstafll     : chr  "Somewhat limited" "Very limited" "Not rated" "Very limited" ...
    ##  $ engstafml     : chr  "Very limited" "Very limited" "Very limited" "Very limited" ...
    ##  $ engsldcd      : chr  "Very limited" "Very limited" "Very limited" "Very limited" ...
    ##  $ engsldcp      : chr  "Very limited" "Very limited" "Very limited" "Very limited" ...
    ##  $ englrsdcd     : chr  "Somewhat limited" "Somewhat limited" "Very limited" "Somewhat limited" ...
    ##  $ engcmssdcd    : chr  "Poor" "Fair" "Fair" "Fair" ...
    ##  $ engcmssmp     : chr  "Fair" "Fair" "Not rated" "Fair" ...
    ##  $ urbrecptdcd   : chr  "Somewhat limited" "Somewhat limited" "Very limited" "Somewhat limited" ...
    ##  $ urbrecptwta   : num  0.248 0.106 0.924 0.122 0.35 0.505 0.624 0.368 0.489 0.46 ...
    ##  $ forpehrtdcp   : chr  "Erosion hazard slight" "Erosion hazard moderate" "Erosion hazard severe" "Erosion hazard slight" ...
    ##  $ hydclprs      : int  0 2 0 2 0 3 2 0 0 0 ...
    ##  $ awmmfpwwta    : num  0.102 1 1 1 0.996 1 1 0.89 0.995 1 ...
    ##  - attr(*, "SDA_id")= chr "Table"

This function returns a data frame with one row per map unit, containing
dozens of pre-calculated aggregate properties. See the
[`get_SDA_muaggatt()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_muaggatt.md)
documentation for the full list of available columns.

### Filtering Results

You can filter by area symbol, map unit key, or custom WHERE clause:

``` r
# Get aggregate attributes for specific mukeys
muagg_filtered <- get_SDA_muaggatt(
  mukeys = c(461994, 461995, 463264)
)

head(muagg_filtered)
```

    ##    mukey musym                                               muname mustatus
    ## 1 461994   102                    Alamo clay, 0 to 2 percent slopes     <NA>
    ## 2 461995   103 Alo-Vaquero complex, 8 to 30 percent slopes, MLRA 15     <NA>
    ## 3 463264   DbE     Daulton very rocky loam, 15 to 30 percent slopes     <NA>
    ##   slopegraddcp slopegradwta brockdepmin wtdepannmin wtdepaprjunmin flodfreqdcd
    ## 1            1          1.0          NA          15             15        Rare
    ## 2           23         23.0          64          NA             NA        None
    ## 3           23         24.3           0          NA             NA        None
    ##   flodfreqmax pondfreqprs aws025wta aws050wta aws0100wta aws0150wta
    ## 1        Rare           0      3.75      7.30      12.06      12.06
    ## 2        None           0      3.74      7.52      11.43      11.43
    ## 3        None           0      4.25      6.12       6.12       6.12
    ##       drclassdcd drclasswettest hydgrpdcd iccdcd iccdcdpct niccdcd niccdcdpct
    ## 1 Poorly drained Poorly drained         D      3        85       4         85
    ## 2   Well drained   Well drained         D   <NA>       100       4         85
    ## 3   Well drained   Well drained         D      6        65       6         65
    ##     engdwobdcd    engdwbdcd  engdwbll     engdwbml   engstafdcd engstafll
    ## 1 Very limited Very limited Not rated Very limited Very limited Not rated
    ## 2 Very limited Very limited Not rated Very limited Very limited Not rated
    ## 3 Very limited Very limited Not rated Very limited Very limited Not rated
    ##      engstafml     engsldcd     engsldcp    englrsdcd engcmssdcd engcmssmp
    ## 1 Very limited Very limited Very limited Very limited       Poor Not rated
    ## 2 Very limited Very limited Very limited Very limited       Poor Not rated
    ## 3 Very limited Very limited Very limited Very limited       Poor Not rated
    ##        urbrecptdcd urbrecptwta                forpehrtdcp hydclprs awmmfpwwta
    ## 1     Very limited        1.00      Erosion hazard slight       93          1
    ## 2 Somewhat limited        0.92    Erosion hazard moderate        0          1
    ## 3     Very limited        1.00 Erosion hazard very severe        0          1

------------------------------------------------------------------------

## `get_SDA_pmgroupname()`: Parent Material Groups

Parent material groups classify the primary geological or organic origin
of soil material. The
[`get_SDA_pmgroupname()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_pmgroupname.md)
function retrieves parent material classifications by component.

### Method: DOMINANT COMPONENT

Get parent material from the dominant component:

``` r
pm_dom <- get_SDA_pmgroupname(
  areasymbols = "CA630",
  method = "DOMINANT COMPONENT",
  simplify = FALSE
)

head(pm_dom)
```

    ##     mukey areasymbol musym
    ## 1 3225132      CA630  7065
    ## 2 3356289      CA630  6206
    ## 3 2600458      CA630  7212
    ## 4 2450845      CA630  3058
    ## 5 2924913      CA630   220
    ## 6 2403696      CA630  5100
    ##                                                                                            muname
    ## 1                          Bonanza-Loafercreek complex, 3 to 15 percent slopes, low precipitation
    ## 2                                                    Musick-Hotaw complex, 8 to 30 percent slopes
    ## 3                                           Wardsferry-Millvilla complex, 30 to 60 percent slopes
    ## 4                                          Shawsflat-Angelscreek complex, 25 to 60 percent slopes
    ## 5                                               Redding gravelly loam, 0 to 8 percent slopes, dry
    ## 6 Mokelumne-Buenavista-Aquultic Haploxeralfs, occasionally ponded complex, 1 to 12 percent slopes
    ##     compname compkind comppct_r majcompflag
    ## 1    Bonanza   Series        70         Yes
    ## 2     Musick   Series        64         Yes
    ## 3 Wardsferry   Series        50         Yes
    ## 4  Shawsflat   Series        65         Yes
    ## 5    Redding   Series        85         Yes
    ## 6  Mokelumne   Series        40         Yes
    ##                                                                                                                                                                                                                       pmgroupname
    ## 1                                                                                                                                                                              colluvium over residuum derived from metavolcanics
    ## 2                                                                                                                                                                                    colluvium over residuum derived from diorite
    ## 3                                                                                                                                                                       colluvium over residuum derived from metasedimentary rock
    ## 4                                                                                                                                                        colluvium derived from latite over residuum weathered from volcanic rock
    ## 5 loamy alluvium derived from igneous, metamorphic and sedimentary rock over clayey alluvium derived from igneous, metamorphic and sedimentary rock over cemented alluvium derived from igneous, metamorphic and sedimentary rock
    ## 6                                                                                                                                                    slope alluvium derived from sandstone over residuum weathered from sandstone

### Method: DOMINANT CONDITION

Aggregate parent materials by group, then return the dominant:

``` r
pm_cond <- get_SDA_pmgroupname(
  mukeys = c(461994, 461995, 462205),
  method = "DOMINANT CONDITION"
)

head(pm_cond)
```

    ##    mukey areasymbol musym
    ## 1 461994      CA077   102
    ## 2 461995      CA077   103
    ## 3 462205      CA624   AzE
    ##                                                                   muname
    ## 1                                      Alamo clay, 0 to 2 percent slopes
    ## 2                   Alo-Vaquero complex, 8 to 30 percent slopes, MLRA 15
    ## 3 Auburn cobbly clay loam, heavy subsoil variant, 9 to 50 percent slopes
    ##                           pmgroupname
    ## 1 Waterlaid (or Transported) Deposits
    ## 2  In-Place Deposits (nontransported)
    ## 3  In-Place Deposits (nontransported)

### Simplify Parameter

By default, `simplify = TRUE` groups parent materials into broader
categories. Set `simplify = FALSE` to get detailed group names:

``` r
# Simplified (broader groups)
pm_simple <- get_SDA_pmgroupname(
  mukeys = c(461994, 461995),
  simplify = TRUE
)

head(pm_simple)
```

    ##    mukey areasymbol musym                                               muname
    ## 1 461994      CA077   102                    Alamo clay, 0 to 2 percent slopes
    ## 2 461995      CA077   103 Alo-Vaquero complex, 8 to 30 percent slopes, MLRA 15
    ##   compname compkind comppct_r majcompflag                         pmgroupname
    ## 1    Alamo   Series        85         Yes Waterlaid (or Transported) Deposits
    ## 2      Alo   Series        45         Yes  In-Place Deposits (nontransported)

``` r
# Detailed (specific groups)
pm_detailed <- get_SDA_pmgroupname(
  mukeys = c(461994, 461995),
  simplify = FALSE
)

head(pm_detailed)
```

    ##    mukey areasymbol musym                                               muname
    ## 1 461995      CA077   103 Alo-Vaquero complex, 8 to 30 percent slopes, MLRA 15
    ## 2 461994      CA077   102                    Alamo clay, 0 to 2 percent slopes
    ##   compname compkind comppct_r majcompflag
    ## 1      Alo   Series        45         Yes
    ## 2    Alamo   Series        85         Yes
    ##                                   pmgroupname
    ## 1 residuum weathered from sandstone and shale
    ## 2            alluvium from mixed rock sources

------------------------------------------------------------------------

## `get_SDA_coecoclass()`: Component Ecological Classes

Ecological classes describe potential natural vegetation and ecological
conditions. The
[`get_SDA_coecoclass()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_coecoclass.md)
function retrieves ecological site classifications by component.

A worked example of this function is available in the [“Dominant
Ecological Site”
vignette](http://ncss-tech.github.io/soilDB/articles/dominant-es.md).

### Method: NONE (Default)

Return component-level ecological classes without aggregation:

``` r
eco_none <- get_SDA_coecoclass(
  method = "None",
  areasymbols = "CA630"
)

head(eco_none)
```

    ##     mukey areasymbol  lkey                                      muname    cokey
    ## 1 2924907      CA630 14103 Cogna loam, 0 to 2 percent slopes, overwash 26412197
    ## 2 2924907      CA630 14103 Cogna loam, 0 to 2 percent slopes, overwash 26412198
    ## 3 2924907      CA630 14103 Cogna loam, 0 to 2 percent slopes, overwash 26412199
    ## 4 2924907      CA630 14103 Cogna loam, 0 to 2 percent slopes, overwash 26412200
    ## 5 2924907      CA630 14103 Cogna loam, 0 to 2 percent slopes, overwash 26412201
    ## 6 2924907      CA630 14103 Cogna loam, 0 to 2 percent slopes, overwash 26412202
    ##   coecoclasskey comppct_r majcompflag   compname           localphase
    ## 1            NA         1         No    Columbia occasionally flooded
    ## 2            NA         3         No     Veritas      fine sandy loam
    ## 3            NA         6         No  Archerdale            clay loam
    ## 4            NA         4         No        Nord                 loam
    ## 5            NA         1         No      Honcut           sandy loam
    ## 6      12406649        85         Yes      Cogna                 <NA>
    ##     compkind   ecoclassid                   ecoclassname    ecoclasstypename
    ## 1     Series Not assigned                   Not assigned        Not assigned
    ## 2     Series Not assigned                   Not assigned        Not assigned
    ## 3     Series Not assigned                   Not assigned        Not assigned
    ## 4 Taxadjunct Not assigned                   Not assigned        Not assigned
    ## 5     Series Not assigned                   Not assigned        Not assigned
    ## 6     Series  R017XY905CA Dry Alluvial Fans and Terraces NRCS Rangeland Site
    ##                            ecoclassref
    ## 1                         Not assigned
    ## 2                         Not assigned
    ## 3                         Not assigned
    ## 4                         Not assigned
    ## 5                         Not assigned
    ## 6 Ecological Site Description Database

The default behavior targets the NRCS Ecological Site database-related
entries, but there are many other ecological/vegetation class types in
SSURGO.

Columns include:

- `ecoclassid` - Ecological Class ID

- `ecoclassname` - Full ecological class name

- `ecoclasstypename` - Type (e.g., `"NRCS Rangeland Site"`,
  `"NRCS Forestland Site"`)

- `ecoclassref` - Reference document

### Method: DOMINANT COMPONENT

Get ecological site from the dominant component only:

``` r
eco_dom <- get_SDA_coecoclass(
  method = "Dominant Component",
  areasymbols = "CA630"
)

head(eco_dom)
```

    ##      mukey areasymbol  lkey                                         muname
    ## 6  2924907      CA630 14103    Cogna loam, 0 to 2 percent slopes, overwash
    ## 7  2924879      CA630 14103       Pentz sandy loam, 2 to 15 percent slopes
    ## 15 2924880      CA630 14103      Pentz sandy loam, 15 to 50 percent slopes
    ## 27 2924881      CA630 14103 Pentz cobbly sandy loam, 2 to 8 percent slopes
    ## 31 2924882      CA630 14103  Pentz-Bellota complex, 2 to 15 percent slopes
    ## 34 2924883      CA630 14103             Peters clay, 2 to 8 percent slopes
    ##       cokey coecoclasskey comppct_r majcompflag compname localphase compkind
    ## 6  26412202      12406649        85         Yes    Cogna       <NA>   Series
    ## 7  26412137      12406603        85         Yes    Pentz       <NA>   Series
    ## 15 26412145      12406611        85         Yes    Pentz       <NA>   Series
    ## 27 26412157      12406621        85         Yes    Pentz       <NA>   Series
    ## 31 26412161      12406625        55         Yes    Pentz  silt loam   Series
    ## 34 26412164      12406626        85         Yes   Peters       <NA>   Series
    ##     ecoclassid                   ecoclassname    ecoclasstypename
    ## 6  R017XY905CA Dry Alluvial Fans and Terraces NRCS Rangeland Site
    ## 7  R018XI163CA      Thermic Low Rolling Hills NRCS Rangeland Site
    ## 15 R018XI163CA      Thermic Low Rolling Hills NRCS Rangeland Site
    ## 27 R018XI163CA      Thermic Low Rolling Hills NRCS Rangeland Site
    ## 31 R018XI163CA      Thermic Low Rolling Hills NRCS Rangeland Site
    ## 34 R018XI164CA        Clayey Dissected Swales NRCS Rangeland Site
    ##                             ecoclassref
    ## 6  Ecological Site Description Database
    ## 7  Ecological Site Description Database
    ## 15 Ecological Site Description Database
    ## 27 Ecological Site Description Database
    ## 31 Ecological Site Description Database
    ## 34 Ecological Site Description Database

### Method: DOMINANT CONDITION

Aggregate ecological classes by ID, then return the dominant:

``` r
eco_cond <- get_SDA_coecoclass(
  method = "Dominant Condition",
  mukeys = c(461994, 461995, 462205),
  ecoclasstypename = "NRCS Forestland Site"
)

head(eco_cond)
```

    ##     mukey areasymbol  lkey
    ## 5  461994      CA077 14086
    ## 11 461995      CA077 14086
    ## 12 462205      CA624 14101
    ##                                                                    muname
    ## 5                                       Alamo clay, 0 to 2 percent slopes
    ## 11                   Alo-Vaquero complex, 8 to 30 percent slopes, MLRA 15
    ## 12 Auburn cobbly clay loam, heavy subsoil variant, 9 to 50 percent slopes
    ##       cokey coecoclasskey comppct_r majcompflag compname localphase compkind
    ## 5  27537144            NA        85         Yes    Alamo       <NA>   Series
    ## 11 27537150            NA        45         Yes      Alo       <NA>   Series
    ## 12 26397614      12405152        85         Yes   Auburn    variant  Variant
    ##      ecoclassid                      ecoclassname     ecoclasstypename
    ## 5  Not assigned                      Not assigned NRCS Forestland Site
    ## 11 Not assigned                      Not assigned NRCS Forestland Site
    ## 12  F018XI201CA Moderately Deep Thermic Foothills NRCS Forestland Site
    ##                             ecoclassref ecoclasspct_r
    ## 5                          Not assigned           100
    ## 11                         Not assigned           100
    ## 12 Ecological Site Description Database            85

### Method: ALL

Return all ecological sites per map unit in wide format:

``` r
eco_all <- get_SDA_coecoclass(
  method = "All",
  mukeys = c(461994, 461995),
  threshold = 5
)

head(eco_all)
```

    ##     mukey                                               muname nationalmusym
    ##     <int>                                               <char>        <char>
    ## 1: 461994                    Alamo clay, 0 to 2 percent slopes          hhr1
    ## 2: 461995 Alo-Vaquero complex, 8 to 30 percent slopes, MLRA 15         2tyzk
    ##          site1                site1name site1compname site1pct_r
    ##         <char>                   <char>        <char>      <int>
    ## 1: R017XY902CA     Duripan Vernal Pools         Alamo         85
    ## 2: R015XE001CA Clayey Hills 10-14" p.z.  Alo, Vaquero         85
    ##                                                      site1link        site2
    ##                                                         <char>       <char>
    ## 1: https://edit.jornada.nmsu.edu/catalogs/esd/017X/R017XY902CA Not assigned
    ## 2: https://edit.jornada.nmsu.edu/catalogs/esd/015X/R015XE001CA Not assigned
    ##       site2name                        site2compname site2pct_r site2link
    ##          <char>                               <char>      <int>    <char>
    ## 1: Not assigned  Alamo, Madera, San Joaquin, Redding         15      <NA>
    ## 2: Not assigned Arburua, Wisflat, San Timoteo, Calla         15      <NA>

Filtering by ecological class type:

``` r
# Get rangeland sites only
eco_range <- get_SDA_coecoclass(
  method = "Dominant Condition",
  areasymbols = "CA630",
  ecoclasstypename = "NRCS Rangeland Site"
)

head(eco_range)
```

    ##      mukey areasymbol  lkey                                         muname
    ## 6  2924907      CA630 14103    Cogna loam, 0 to 2 percent slopes, overwash
    ## 7  2924879      CA630 14103       Pentz sandy loam, 2 to 15 percent slopes
    ## 15 2924880      CA630 14103      Pentz sandy loam, 15 to 50 percent slopes
    ## 27 2924881      CA630 14103 Pentz cobbly sandy loam, 2 to 8 percent slopes
    ## 31 2924882      CA630 14103  Pentz-Bellota complex, 2 to 15 percent slopes
    ## 34 2924883      CA630 14103             Peters clay, 2 to 8 percent slopes
    ##       cokey coecoclasskey comppct_r majcompflag compname localphase compkind
    ## 6  26412202      12406649        85         Yes    Cogna       <NA>   Series
    ## 7  26412137      12406603        85         Yes    Pentz       <NA>   Series
    ## 15 26412145      12406611        85         Yes    Pentz       <NA>   Series
    ## 27 26412157      12406621        85         Yes    Pentz       <NA>   Series
    ## 31 26412161      12406625        55         Yes    Pentz  silt loam   Series
    ## 34 26412164      12406626        85         Yes   Peters       <NA>   Series
    ##     ecoclassid                   ecoclassname    ecoclasstypename
    ## 6  R017XY905CA Dry Alluvial Fans and Terraces NRCS Rangeland Site
    ## 7  R018XI163CA      Thermic Low Rolling Hills NRCS Rangeland Site
    ## 15 R018XI163CA      Thermic Low Rolling Hills NRCS Rangeland Site
    ## 27 R018XI163CA      Thermic Low Rolling Hills NRCS Rangeland Site
    ## 31 R018XI163CA      Thermic Low Rolling Hills NRCS Rangeland Site
    ## 34 R018XI164CA        Clayey Dissected Swales NRCS Rangeland Site
    ##                             ecoclassref ecoclasspct_r
    ## 6  Ecological Site Description Database            85
    ## 7  Ecological Site Description Database            94
    ## 15 Ecological Site Description Database            96
    ## 27 Ecological Site Description Database            92
    ## 31 Ecological Site Description Database            88
    ## 34 Ecological Site Description Database            85

------------------------------------------------------------------------

## `get_SDA_cosurfmorph()`: Component Surface Morphometry

Surface morphometry describes the three-dimensional shape and position
of a landscape. The
[`get_SDA_cosurfmorph()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_cosurfmorph.md)
function returns component-level geomorphic classifications aggregated
in various ways.

### Available Tables

SDA contains four cosurfmorph tables:

| Table            | Variables                                                   | Description                           |
|------------------|-------------------------------------------------------------|---------------------------------------|
| `cosurfmorphgc`  | `geomposmntn`, `geomposhill`, `geomposflats`, `geompostrce` | 3D geomorphic classification          |
| `cosurfmorphhpp` | `hillslopeprof`                                             | 2D hillslope position profile         |
| `cosurfmorphss`  | `shapeacross`, `shapedown`                                  | Surface shape (convex/concave/linear) |
| `cosurfmorphmr`  | `geomicrorelief`                                            | Microrelief (slope complexity)        |

### Example: 3D Geomorphic Classification

``` r
# Get geomorphic position by component name
geom_3d <- get_SDA_cosurfmorph(
  table = "cosurfmorphgc",
  areasymbols = "CA630"
)

head(geom_3d)
```

    ##     mukey geomposmntn geomposmntn_n p_geomposmntn geomposhill geomposhill_n
    ## 1 1865918        <NA>             0             0  Interfluve             4
    ## 2 1865926        <NA>             0             0  Side Slope             5
    ## 3 1865927        <NA>             0             0  Side Slope             3
    ## 4 1865928        <NA>             0             0  Side Slope             2
    ## 5 1865929        <NA>             0             0  Interfluve             1
    ## 6 1865929        <NA>             0             0  Nose Slope             1
    ##   p_geomposhill geomposflats geomposflats_n p_geomposflats geompostrce
    ## 1           1.0         <NA>              0              0        <NA>
    ## 2           1.0         <NA>              0              0        <NA>
    ## 3           1.0         <NA>              0              0        <NA>
    ## 4           1.0         <NA>              0              0        <NA>
    ## 5           0.5         <NA>              0              0        <NA>
    ## 6           0.5         <NA>              0              0        <NA>
    ##   geompostrce_n p_geompostrce total
    ## 1             0             0     4
    ## 2             0             0     5
    ## 3             0             0     3
    ## 4             0             0     2
    ## 5             0             0     2
    ## 6             0             0     2

Output includes columns like:

- `compname` - Component name

- `geomposmntn`, `geomposmntn_n` - Mountain position (count)

- `p_geomposmntn` - Mountain position proportion

### Example: Hillslope Position by Area

``` r
# Get hillslope position aggregated by area symbol
geom_hill <- get_SDA_cosurfmorph(
  table = "cosurfmorphhpp",
  by = "areasymbol",
  areasymbols = "CA630"
)

head(geom_hill)
```

    ##   areasymbol hillslopeprof hillslopeprof_n p_hillslopeprof total
    ## 1      CA630     Backslope             260            0.48   546
    ## 2      CA630      Shoulder             114            0.21   546
    ## 3      CA630     Footslope              78            0.14   546
    ## 4      CA630        Summit              76            0.14   546
    ## 5      CA630      Toeslope              18            0.03   546

### Example: Surface Shape

``` r
# Get surface shape classes
geom_shape <- get_SDA_cosurfmorph(
  table = "cosurfmorphss",
  areasymbols = "CA649"
)

head(geom_shape)
```

    ##    mukey shapeacross shapeacross_n p_shapeacross shapedown shapedown_n
    ## 1 463229      Convex             1             1   Concave           1
    ## 2 463230      Convex             1             1   Concave           1
    ## 3 463231      Convex             3             1   Concave           3
    ## 4 463232      Convex             3             1   Concave           3
    ## 5 463233      Convex             3             1   Concave           3
    ## 6 463234      Convex             2             1   Concave           2
    ##   p_shapedown   surfaceshape surfaceshape_n p_surfaceshape total
    ## 1           1 Convex/Concave              1              1     1
    ## 2           1 Convex/Concave              1              1     1
    ## 3           1 Convex/Concave              3              1     3
    ## 4           1 Convex/Concave              3              1     3
    ## 5           1 Convex/Concave              3              1     3
    ## 6           1 Convex/Concave              2              1     2

### Example: Microrelief

``` r
# Get microrelief by component name
geom_micro <- get_SDA_cosurfmorph(
  table = "cosurfmorphmr",
  areasymbols = "CA630"
)

head(geom_micro)
```

    ##     mukey geomicrorelief geomicrorelief_n p_geomicrorelief total
    ## 1 1865918      Microhigh                3             0.75     4
    ## 2 1865918       Microlow                1             0.25     4
    ## 3 2403696       Microlow                1             1.00     1
    ## 4 2600537      Microhigh                1             1.00     1
    ## 5 2766838      Microhigh                1             0.50     2
    ## 6 2766838       Microlow                1             0.50     2

------------------------------------------------------------------------

## `fetchSDA()`: Get Soil Profile Collection

The
[`fetchSDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
function is a high-level wrapper around
[`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
that simplifies the process of downloading and assembling soil profile
data into a `SoilProfileCollection` object from the package `aqp`. It
automatically handles the complex joins between map unit, component, and
horizon tables.

### Basic Usage

You can fetch all components and horizons for a specific area or set of
map units using the `WHERE` argument:

``` r
library(aqp)

# Query soil components by areasymbol and musym
s <- fetchSDA(WHERE = "areasymbol = 'IN005' AND musym = 'MnpB2'")

# The result is a SoilProfileCollection
print(s)
```

    ## SoilProfileCollection with 4 profiles and 20 horizons
    ## profile ID: cokey  |  horizon ID: chkey 
    ## Depth range: 200 - 200 cm
    ## 
    ## ----- Horizons (6 / 20 rows  |  10 / 64 columns) -----
    ##     cokey    chkey hzdept_r hzdepb_r hzname texture           texcl lieutex
    ##  26474693 78997358        0       25     Ap     sil       Silt loam    <NA>
    ##  26474693 78997359       25       35      A     sil       Silt loam    <NA>
    ##  26474693 78997360       35       91   Btg1    sicl Silty clay loam    <NA>
    ##  26474693 78997361       91      150  2Btg2       l            Loam    <NA>
    ##  26474693 78997362      150      200     2C       l            Loam    <NA>
    ##  26474694 78997365        0       41     Ap    sicl Silty clay loam    <NA>
    ##  fragvol_l fragvol_r
    ##          0         0
    ##          0         0
    ##          0         0
    ##          1         5
    ##          1         6
    ##         NA        NA
    ## [... more horizons ...]
    ## 
    ## ----- Sites (4 / 4 rows  |  10 / 101 columns) -----
    ##  mukey nationalmusym    cokey  compname comppct_r compkind majcompflag
    ##     NA         2zss9 26474693    Treaty         2   Series         No 
    ##     NA         2zss9 26474694 Brookston         4   Series         No 
    ##     NA         2zss9 26474695     Miami        85   Series         Yes
    ##     NA         2zss9 26474696    Crosby         9   Series         No 
    ##                  localphase              drainagecl hydricrating
    ##  frequently ponded, drained          Poorly drained          Yes
    ##                        <NA>          Poorly drained          Yes
    ##                      eroded Moderately well drained           No
    ##                      eroded Somewhat poorly drained           No
    ## 
    ## Spatial Data:
    ## [EMPTY]

``` r
# Check the horizon data
head(horizons(s))
```

    ##      chkey hzname hzdept_r hzdepb_r texture           texcl lieutex fragvol_l
    ## 1 78997358     Ap        0       25     sil       Silt loam    <NA>         0
    ## 2 78997359      A       25       35     sil       Silt loam    <NA>         0
    ## 3 78997360   Btg1       35       91    sicl Silty clay loam    <NA>         0
    ## 4 78997361  2Btg2       91      150       l            Loam    <NA>         1
    ## 5 78997362     2C      150      200       l            Loam    <NA>         1
    ## 6 78997365     Ap        0       41    sicl Silty clay loam    <NA>        NA
    ##   fragvol_r fragvol_h sandtotal_l sandtotal_r sandtotal_h silttotal_l
    ## 1         0         2           5          17          20          40
    ## 2         0         2           5          16          20          40
    ## 3         0         2           5          12          15          50
    ## 4         5         9          15          30          45          22
    ## 5         6        13          30          40          60          22
    ## 6        NA        NA           5          13          19          50
    ##   silttotal_r silttotal_h claytotal_l claytotal_r claytotal_h om_l om_r om_h
    ## 1          59          80           9          24          40  3.0 4.50    6
    ## 2          58          80           9          26          40  2.0 4.00    6
    ## 3          55          66          24          33          35  1.0 1.50    2
    ## 4          44          65          20          26          35  0.5 0.75    1
    ## 5          42          50          10          18          20  0.0 0.50    1
    ## 6          59          67          28          28          35  3.0 4.00    6
    ##   dbthirdbar_l dbthirdbar_r dbthirdbar_h ksat_l ksat_r ksat_h awc_l awc_r awc_h
    ## 1         1.34         1.38         1.43   4.23   9.17  14.11  0.17  0.21  0.24
    ## 2         1.26         1.31         1.35   4.23   9.17  14.11  0.17  0.21  0.24
    ## 3         1.33         1.35         1.37   4.23   9.17  14.11  0.14  0.16  0.21
    ## 4         1.38         1.48         1.58   4.23   9.17  14.11  0.07  0.14  0.21
    ## 5         1.37         1.46         1.56   1.41   2.82   4.23  0.01  0.08  0.15
    ## 6         1.30         1.45         1.60   4.23   9.17  14.11  0.15  0.17  0.25
    ##   lep_r sar_r ec_r cec7_r sumbases_r ph1to1h2o_l ph1to1h2o_r ph1to1h2o_h
    ## 1   3.4     0    0   20.7       19.1         5.6         6.5         7.3
    ## 2   3.8     0    0   22.2       20.3         5.6         6.5         7.3
    ## 3   5.3     0    0   26.5       26.0         6.1         7.0         7.8
    ## 4   3.3     0    0   20.3       59.0         6.6         7.5         8.4
    ## 5   1.5     0    0   13.0       51.7         7.4         7.9         8.4
    ## 6   4.3     0    0   23.7       21.3         5.6         6.5         7.3
    ##   caco3_l caco3_r caco3_h kwfact kffact    cokey nasischiid fine_gravel gravel
    ## 1       0       0       0    .32    .32 26474693   11469785           0      0
    ## 2       0       0       0    .24    .24 26474693   11469786           0      0
    ## 3       0       0       0    .32    .32 26474693   11469787           0      0
    ## 4       0      13      25    .37    .37 26474693   11469788           3      5
    ## 5      15      28      40    .43    .43 26474693   11469784           0      2
    ## 6       0       0       0    .28    .28 26474694   11469791           0      0
    ##   cobbles stones boulders channers flagstones parafine_gravel paragravel
    ## 1       0      0        0        0          0               0          0
    ## 2       0      0        0        0          0               0          0
    ## 3       0      0        0        0          0               0          0
    ## 4       0      0        0        0          0               0          0
    ## 5       1      0        0        3          0               0          0
    ## 6       0      0        0        0          0               0          0
    ##   paracobbles parastones paraboulders parachanners paraflagstones unspecified
    ## 1           0          0            0            0              0           0
    ## 2           0          0            0            0              0           0
    ## 3           0          0            0            0              0           0
    ## 4           0          0            0            0              0           0
    ## 5           0          0            0            0              0           0
    ## 6           0          0            0            0              0           0
    ##   total_frags_pct_nopf total_frags_pct hzID
    ## 1                    0               0    1
    ## 2                    0               0    2
    ## 3                    0               0    3
    ## 4                    5               5    4
    ## 5                    6               6    5
    ## 6                    0               0    6

This function is very powerful for getting a quick snapshot of the soil
profiles in an area.

It is worth noting that
[`fetchSDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
returns data from *both* SSURGO and STATSGO2. To limit to SSURGO, use
`WHERE = "areasymbol != 'US'"` (see the Data Filtering section), or
specify a SSURGO area symbol.

### Handling Duplicates

The `duplicates` argument (defaulting to `FALSE`) controls how
components that appear in multiple survey areas are handled.

- **`duplicates = FALSE` (Default)**: Returns only one instance of a
  component per `nationalmusym`. This eliminates “duplication” where
  multiple survey area (legend) map units are linked to the same source
  `mapunit` and `component` records. This is generally preferred for
  statistical analysis of soil properties.
- **`duplicates = TRUE`**: Returns a record for each unique map unit key
  (`mukey`). This is useful for visualization or when you need to
  account for every occurrence of a component across different survey
  area correlations.

------------------------------------------------------------------------

## `fetchLDM()`: Get Laboratory Data

The
[`fetchLDM()`](http://ncss-tech.github.io/soilDB/reference/fetchLDM.md)
function provides access to the Kellogg Soil Survey Laboratory (KSSL)
Data Mart via Soil Data Access. It allows for querying laboratory data
by various criteria and returning it as a `SoilProfileCollection`.

### Basic Usage

You can fetch lab data by specifying an identifier (like an area symbol)
and the column it corresponds to (`what` argument):

``` r
# Fetch KSSL data for a specific soil survey area (CA630)
# 'what' argument specifies we are searching by 'area_code'
# 'tables' argument specifies which data tables to include (defaults to core tables)
ldm_data <- fetchLDM("CA630", what = "area_code")

# The result is a SoilProfileCollection
print(ldm_data)
```

    ## SoilProfileCollection with 175 profiles and 800 horizons
    ## profile ID: pedon_key  |  horizon ID: hzID 
    ## Depth range: 13 - 321 cm
    ## 
    ## ----- Horizons (6 / 800 rows  |  10 / 432 columns) -----
    ##  pedon_key hzID hzn_top hzn_bot hzn_desgn layer_key labsampnum project_key
    ##      34047    1       0       3        Oi    206399   07N03070        4919
    ##      34047    2       3       5        Oe    206400   07N03071        4919
    ##      34047    3       5      15        A1    206401   07N03072        4919
    ##      34047    4      15      28        A2    206402   07N03073        4919
    ##      34047    5      28      46        BE    206403   07N03074        4919
    ##      34047    6      46      71      2Bt1    206404   07N03075        4919
    ##  layer_sequence layer_type
    ##               1    horizon
    ##               2    horizon
    ##               3    horizon
    ##               4    horizon
    ##               5    horizon
    ##               6    horizon
    ## [... more horizons ...]
    ## 
    ## ----- Sites (6 / 175 rows  |  10 / 126 columns) -----
    ##  pedon_key site_key pedlabsampnum pedoniid      upedonid labdatadescflag
    ##      34047    33850       07N0468   242796 S2007CA009001               1
    ##      34048    33851       07N0469   242808 S2007CA009002               1
    ##      34049    33852       07N0470   242805 S2007CA109001               1
    ##      34050    33853       07N0471   242797 S2007CA109002               1
    ##      34051    33854       07N0472   242798 S2007CA109003               1
    ##      34052    33855       07N0473   207254 S2007CA109004               1
    ##  priority priority2     samp_name samp_class_type
    ##         B         A      Sitesnot          series
    ##         B         A    Gopheridge          series
    ##         B         A    Motherlode          series
    ##         B         A    Hennekenot          series
    ##         B         A     Whiterock          series
    ##         B         A Musick (fine)          series
    ## [... more sites ...]
    ## 
    ## Spatial Data:
    ## [EMPTY]

``` r
# Inspect site data
head(site(ldm_data))
```

    ##   pedon_key site_key pedlabsampnum pedoniid      upedonid labdatadescflag
    ## 1     34047    33850       07N0468   242796 S2007CA009001               1
    ## 2     34048    33851       07N0469   242808 S2007CA009002               1
    ## 3     34049    33852       07N0470   242805 S2007CA109001               1
    ## 4     34050    33853       07N0471   242797 S2007CA109002               1
    ## 5     34051    33854       07N0472   242798 S2007CA109003               1
    ## 6     34052    33855       07N0473   207254 S2007CA109004               1
    ##   priority priority2     samp_name samp_class_type        samp_classdate
    ## 1        B         A      Sitesnot          series  4/5/2007 12:00:00 AM
    ## 2        B         A    Gopheridge          series  4/2/2007 12:00:00 AM
    ## 3        B         A    Motherlode          series  4/5/2007 12:00:00 AM
    ## 4        B         A    Hennekenot          series 4/11/2007 12:00:00 AM
    ## 5        B         A     Whiterock          series 3/29/2007 12:00:00 AM
    ## 6        B         A Musick (fine)          series 11/2/2006 12:00:00 AM
    ##                                         samp_classification_name samp_taxorder
    ## 1                     Fine, parasesquic, mesic Xeric Haplohumult      ultisols
    ## 2 Loamy-skeletal, mixed, superactive, thermic Mollic Haploxeralf      alfisols
    ## 3           Fine, mixed, superactive, thermic Mollic Haploxeralf      alfisols
    ## 4            Loamy-skeletal, magnesic, thermic Lithic Argixeroll     mollisols
    ## 5          Loamy, mixed, superactive, thermic Lithic Haploxerept   inceptisols
    ## 6        Fine-loamy, mixed, superactive, mesic Ultic Haploxeralf      alfisols
    ##   samp_taxsuborder samp_taxgrtgroup      samp_taxsubgrp samp_taxpartsize
    ## 1          humults     haplohumults  xeric haplohumults             fine
    ## 2          xeralfs     haploxeralfs mollic haploxeralfs   loamy-skeletal
    ## 3          xeralfs     haploxeralfs mollic haploxeralfs             fine
    ## 4          xerolls      argixerolls  lithic argixerolls   loamy-skeletal
    ## 5          xerepts     haploxerepts lithic haploxerepts            loamy
    ## 6          xeralfs     haploxeralfs  ultic haploxeralfs       fine-loamy
    ##   samp_taxpartsizemod samp_taxceactcl samp_taxreaction samp_taxtempcl
    ## 1                <NA>            <NA>             <NA>          mesic
    ## 2                <NA>     superactive             <NA>        thermic
    ## 3                <NA>     superactive             <NA>        thermic
    ## 4                <NA>            <NA>             <NA>        thermic
    ## 5                <NA>     superactive             <NA>        thermic
    ## 6                <NA>     superactive             <NA>          mesic
    ##   samp_taxmoistscl samp_taxtempregime samp_taxminalogy samp_taxother
    ## 1             <NA>               <NA>             <NA>          <NA>
    ## 2             <NA>               <NA>             <NA>          <NA>
    ## 3             <NA>               <NA>             <NA>          <NA>
    ## 4             <NA>               <NA>             <NA>          <NA>
    ## 5             <NA>               <NA>             <NA>          <NA>
    ## 6             <NA>               <NA>             <NA>          <NA>
    ##   samp_osdtypelocflag  corr_name corr_class_type        corr_classdate
    ## 1                   0      Sites          series  1/3/2018 12:00:00 AM
    ## 2                   0 Gopheridge          series 1/24/2012 12:00:00 AM
    ## 3                   0       <NA>            <NA>                  <NA>
    ## 4                   0     Sixbit          series 7/19/2018 12:00:00 AM
    ## 5                   0  Whiterock      taxadjunct 1/24/2012 12:00:00 AM
    ## 6                   0   Wukusick          series  6/9/2017 12:00:00 AM
    ##                                       corr_classification_name corr_taxorder
    ## 1                  Fine, parasesquic, mesic Xeric Haplohumults      ultisols
    ## 2    Loamy-skeletal, mixed, active, thermic Ultic Haploxeralfs      alfisols
    ## 3 Coarse-loamy, mixed, superactive, thermic Typic Haploxerepts      alfisols
    ## 4         Loamy-skeletal, magnesic, thermic Lithic Argixerolls     mollisols
    ## 5       Loamy, mixed, superactive, thermic Lithic Haploxerepts   inceptisols
    ## 6              Fine, mixed, subactive, mesic Ultic Palexeralfs      alfisols
    ##   corr_taxsuborder corr_taxgrtgroup      corr_taxsubgrp corr_taxpartsize
    ## 1          humults     haplohumults  xeric haplohumults             fine
    ## 2          xeralfs     haploxeralfs  ultic haploxeralfs   loamy-skeletal
    ## 3          xeralfs     haploxerepts  typic haploxerepts     coarse-loamy
    ## 4          xerolls      argixerolls  lithic argixerolls   loamy-skeletal
    ## 5          xerepts     haploxerepts lithic haploxerepts            loamy
    ## 6          xeralfs      palexeralfs   ultic palexeralfs             fine
    ##   corr_taxpartsizemod corr_taxceactcl corr_taxreaction corr_taxtempcl
    ## 1                <NA>      semiactive             <NA>          mesic
    ## 2                <NA>          active             <NA>        thermic
    ## 3                <NA>     superactive             <NA>        thermic
    ## 4                <NA>     superactive             <NA>        thermic
    ## 5                <NA>     superactive             <NA>        thermic
    ## 6                <NA>       subactive             <NA>          mesic
    ##   corr_taxmoistscl corr_taxtempregime corr_taxminalogy corr_taxother
    ## 1            typic              mesic      parasesquic          <NA>
    ## 2            typic            thermic            mixed          <NA>
    ## 3            typic            thermic            mixed          <NA>
    ## 4            typic            thermic         magnesic          <NA>
    ## 5            typic            thermic            mixed          <NA>
    ## 6            typic              mesic            mixed          <NA>
    ##   corr_osdtypelocflag SSL_name SSL_class_type SSL_classdate
    ## 1                   0     <NA>           <NA>          <NA>
    ## 2                   1     <NA>           <NA>          <NA>
    ## 3                   0     <NA>           <NA>          <NA>
    ## 4                   1     <NA>           <NA>          <NA>
    ## 5                   0     <NA>           <NA>          <NA>
    ## 6                   1     <NA>           <NA>          <NA>
    ##   SSL_classification_name SSL_taxorder SSL_taxsuborder SSL_taxgrtgroup
    ## 1                    <NA>         <NA>            <NA>            <NA>
    ## 2                    <NA>         <NA>            <NA>            <NA>
    ## 3                    <NA>         <NA>            <NA>            <NA>
    ## 4                    <NA>         <NA>            <NA>            <NA>
    ## 5                    <NA>         <NA>            <NA>            <NA>
    ## 6                    <NA>         <NA>            <NA>            <NA>
    ##   SSL_taxsubgrp SSL_taxpartsize SSL_taxpartsizemod SSL_taxceactcl
    ## 1          <NA>            <NA>               <NA>           <NA>
    ## 2          <NA>            <NA>               <NA>           <NA>
    ## 3          <NA>            <NA>               <NA>           <NA>
    ## 4          <NA>            <NA>               <NA>           <NA>
    ## 5          <NA>            <NA>               <NA>           <NA>
    ## 6          <NA>            <NA>               <NA>           <NA>
    ##   SSL_taxreaction SSL_taxtempcl SSL_taxmoistscl SSL_taxtempregime
    ## 1            <NA>          <NA>            <NA>              <NA>
    ## 2            <NA>          <NA>            <NA>              <NA>
    ## 3            <NA>          <NA>            <NA>              <NA>
    ## 4            <NA>          <NA>            <NA>              <NA>
    ## 5            <NA>          <NA>            <NA>              <NA>
    ## 6            <NA>          <NA>            <NA>              <NA>
    ##   SSL_taxminalogy SSL_taxother SSL_osdtypelocflag siteiid    usiteid
    ## 1            <NA>         <NA>                  0  254492 07-SMM-015
    ## 2            <NA>         <NA>                  0  254491 07-JCR-002
    ## 3            <NA>         <NA>                  0  254488 07-JCR-001
    ## 4            <NA>         <NA>                  0  254487 07-SMM-009
    ## 5            <NA>         <NA>                  0  254485 07-SMM-008
    ## 6            <NA>         <NA>                  0  254484 06-JCR-006
    ##            site_obsdate latitude_decimal_degrees longitude_decimal_degrees
    ## 1  4/5/2007 12:00:00 AM                 38.37947                 -120.4417
    ## 2  4/2/2007 12:00:00 AM                 37.94286                 -120.7150
    ## 3  4/5/2007 12:00:00 AM                 37.84808                 -120.5607
    ## 4 4/11/2007 12:00:00 AM                 37.86214                 -120.5085
    ## 5 3/29/2007 12:00:00 AM                 37.83278                 -120.5163
    ## 6 11/2/2006 12:00:00 AM                 37.98614                 -120.2356
    ##   country_key state_key county_key mlra_key ssa_key npark_key nforest_key
    ## 1         244      3951       4453    11322    8103        NA          NA
    ## 2         244      3951       4453     7578    8103        NA          NA
    ## 3         244      3951       4503     7578    8103        NA          NA
    ## 4         244      3951       4503     7578    8103        NA          NA
    ## 5         244      3951       4503     7578    8103        NA          NA
    ## 6         244      3951       4503     7578    8103        NA          NA
    ##                                note samp_taxfamhahatmatcl corr_taxfamhahatmatcl
    ## 1 NASIS updated 5/4/2020 7:44:44 AM                  <NA>                  <NA>
    ## 2 NASIS updated 5/4/2020 7:44:44 AM                  <NA>                  <NA>
    ## 3 NASIS updated 5/4/2020 7:44:44 AM                  <NA>                  <NA>
    ## 4 NASIS updated 5/4/2020 7:44:44 AM                  <NA>                  <NA>
    ## 5 NASIS updated 5/4/2020 7:44:44 AM                  <NA>                  <NA>
    ## 6 NASIS updated 5/4/2020 7:44:44 AM                  <NA>                  <NA>
    ##   SSL_taxfamhahatmatcl           pedobjupdate        siteobjupdate wmiid
    ## 1                 <NA> 12/11/2018 11:05:00 PM 9/14/2018 6:17:53 PM 24530
    ## 2                 <NA> 12/27/2018 10:08:46 PM 9/14/2018 6:17:53 PM 24531
    ## 3                 <NA>    6/9/2017 7:41:53 PM 9/14/2018 6:17:53 PM 24532
    ## 4                 <NA>   7/19/2018 6:51:07 PM 8/10/2011 6:55:58 PM 24533
    ## 5                 <NA>  12/30/2015 4:03:15 PM 9/14/2018 6:17:53 PM 24534
    ## 6                 <NA>  12/7/2017 12:12:50 AM 9/14/2018 6:00:14 PM 24535
    ##       Series User_pedon_ID pedon_Key  peiid
    ## 1      Sites S2007CA009001     34047 242796
    ## 2 Gopheridge S2007CA009002     34048 242808
    ## 3 Motherlode S2007CA109001     34049 242805
    ## 4     Sixbit S2007CA109002     34050 242797
    ## 5  Whiterock S2007CA109003     34051 242798
    ## 6   Wukusick S2007CA109004     34052 207254
    ##                                            Soil_Classification
    ## 1                  Fine, parasesquic, mesic Xeric Haplohumults
    ## 2    Loamy-skeletal, mixed, active, thermic Ultic Haploxeralfs
    ## 3 Coarse-loamy, mixed, superactive, thermic Typic Haploxerepts
    ## 4         Loamy-skeletal, magnesic, thermic Lithic Argixerolls
    ## 5       Loamy, mixed, superactive, thermic Lithic Haploxerepts
    ## 6              Fine, mixed, subactive, mesic Ultic Palexeralfs
    ##                                                                        Primary_Lab_Report
    ## 1 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34047&r=1&submit1=Get+Report
    ## 2 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34048&r=1&submit1=Get+Report
    ## 3 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34049&r=1&submit1=Get+Report
    ## 4 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34050&r=1&submit1=Get+Report
    ## 5 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34051&r=1&submit1=Get+Report
    ## 6 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34052&r=1&submit1=Get+Report
    ##                                                                           Taxonomy_Report
    ## 1 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34047&r=3&submit1=Get+Report
    ## 2 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34048&r=3&submit1=Get+Report
    ## 3 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34049&r=3&submit1=Get+Report
    ## 4 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34050&r=3&submit1=Get+Report
    ## 5 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34051&r=3&submit1=Get+Report
    ## 6 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34052&r=3&submit1=Get+Report
    ##                                                                  Supplementary_Lab_Report
    ## 1 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34047&r=2&submit1=Get+Report
    ## 2 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34048&r=2&submit1=Get+Report
    ## 3 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34049&r=2&submit1=Get+Report
    ## 4 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34050&r=2&submit1=Get+Report
    ## 5 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34051&r=2&submit1=Get+Report
    ## 6 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34052&r=2&submit1=Get+Report
    ##                                                                    Water_Retention_Report
    ## 1 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34047&r=6&submit1=Get+Report
    ## 2 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34048&r=6&submit1=Get+Report
    ## 3 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34049&r=6&submit1=Get+Report
    ## 4 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34050&r=6&submit1=Get+Report
    ## 5 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34051&r=6&submit1=Get+Report
    ## 6 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34052&r=6&submit1=Get+Report
    ##                                                                        Correlation_Report
    ## 1 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34047&r=7&submit1=Get+Report
    ## 2 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34048&r=7&submit1=Get+Report
    ## 3 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34049&r=7&submit1=Get+Report
    ## 4 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34050&r=7&submit1=Get+Report
    ## 5 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34051&r=7&submit1=Get+Report
    ## 6 https://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx?p=34052&r=7&submit1=Get+Report
    ##                                                                                                                  pedon_Description_Report
    ## 1 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=Pedon_Site_Description_usepedonid&pedon_id=S2007CA009001
    ## 2 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=Pedon_Site_Description_usepedonid&pedon_id=S2007CA009002
    ## 3 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=Pedon_Site_Description_usepedonid&pedon_id=S2007CA109001
    ## 4 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=Pedon_Site_Description_usepedonid&pedon_id=S2007CA109002
    ## 5 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=Pedon_Site_Description_usepedonid&pedon_id=S2007CA109003
    ## 6 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=Pedon_Site_Description_usepedonid&pedon_id=S2007CA109004
    ##                                                                                                              Soil_Profile
    ## 1 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-profiles-by-PEIID&pedon_peiid=242796
    ## 2 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-profiles-by-PEIID&pedon_peiid=242808
    ## 3 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-profiles-by-PEIID&pedon_peiid=242805
    ## 4 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-profiles-by-PEIID&pedon_peiid=242797
    ## 5 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-profiles-by-PEIID&pedon_peiid=242798
    ## 6 https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-profiles-by-PEIID&pedon_peiid=207254
    ##                                                             Soil_web      lat
    ## 1 https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.3795,-120.442 38.37947
    ## 2 https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.9429,-120.715 37.94286
    ## 3 https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.8481,-120.561 37.84808
    ## 4 https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.8621,-120.509 37.86214
    ## 5 https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.8328,-120.516 37.83278
    ## 6 https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.9861,-120.236 37.98614
    ##        long                         latlong user_site_id horizontal_datum_name
    ## 1 -120.4417 POINT (-120.4417496 38.3794708)   07-SMM-015                 NAD83
    ## 2 -120.7150 POINT (-120.7150269 37.9428596)   07-JCR-002                 NAD83
    ## 3 -120.5607 POINT (-120.5607224 37.8480835)   07-JCR-001                 NAD83
    ## 4 -120.5085 POINT (-120.5085297 37.8621407)   07-SMM-009                 NAD83
    ## 5 -120.5163  POINT (-120.516304 37.8327789)   07-SMM-008                 NAD83
    ## 6 -120.2356  POINT (-120.235611 37.9861374)   06-JCR-006                 NAD83
    ##   latitude_direction latitude_degrees latitude_minutes latitude_seconds
    ## 1              north               38               22             46.1
    ## 2              north               37               56             34.3
    ## 3              north               37               50             53.1
    ## 4              north               37               51             43.7
    ## 5              north               37               49             58.0
    ## 6              north               37               59             10.1
    ##   longitude_direction longitude_degrees longitude_minutes longitude_seconds
    ## 1                west               120                26              30.3
    ## 2                west               120                42              54.1
    ## 3                west               120                33              38.6
    ## 4                west               120                30              30.7
    ## 5                west               120                30              58.7
    ## 6                west               120                14               8.2
    ##   latitude_std_decimal_degrees longitude_std_decimal_degrees
    ## 1                     38.37947                     -120.4417
    ## 2                     37.94286                     -120.7150
    ## 3                     37.84808                     -120.5607
    ## 4                     37.86214                     -120.5085
    ## 5                     37.83278                     -120.5163
    ## 6                     37.98614                     -120.2356
    ##                    msrepl_tran_version observation_date user_pedon_id
    ## 1 5f3780b7-ecfb-4a47-8ef5-1f74676c7bba            39177    07CA009001
    ## 2 44521b48-f3ee-416a-ba8e-2cded5536a70            39174    07CA009002
    ## 3 557d57b0-a61f-404f-a767-7e95a9c5faf2            39177    07CA109001
    ## 4 04b0939b-f735-4df7-a365-686602410635            39183    07CA109002
    ## 5 7f798df9-e551-460e-bf0a-5da3bbf1bd62            39170    07CA109003
    ## 6 fa26acf5-5cbb-49de-8cff-ea4273dff91c            39023    07CA109004
    ##   pedon_seq_num cntrl_depth_to_top cntrl_depth_to_bot fldsyb mapsyb area_key
    ## 1             1                 28                 78   <NA>   <NA>     8103
    ## 2             2                 18                 68   <NA>   <NA>     8103
    ## 3             1                 20                 70   <NA>   <NA>     8103
    ## 4             2                 12                 48   <NA>   <NA>     8103
    ## 5             3                  0                 36   <NA>   <NA>     8103
    ## 6             4                 88                140   <NA>   <NA>     8103
    ##   area_type area_sub_type parent_area_key parent_org_key area_code
    ## 1       ssa          <NA>            3951             51     CA630
    ## 2       ssa          <NA>            3951             51     CA630
    ## 3       ssa          <NA>            3951             51     CA630
    ## 4       ssa          <NA>            3951             51     CA630
    ## 5       ssa          <NA>            3951             51     CA630
    ## 6       ssa          <NA>            3951             51     CA630
    ##                                                                             area_name
    ## 1 Central Sierra Foothills Area, California, Parts of Calaveras and Tuolumne Counties
    ## 2 Central Sierra Foothills Area, California, Parts of Calaveras and Tuolumne Counties
    ## 3 Central Sierra Foothills Area, California, Parts of Calaveras and Tuolumne Counties
    ## 4 Central Sierra Foothills Area, California, Parts of Calaveras and Tuolumne Counties
    ## 5 Central Sierra Foothills Area, California, Parts of Calaveras and Tuolumne Counties
    ## 6 Central Sierra Foothills Area, California, Parts of Calaveras and Tuolumne Counties
    ##   area_abbrev area_desc
    ## 1        <NA>      <NA>
    ## 2        <NA>      <NA>
    ## 3        <NA>      <NA>
    ## 4        <NA>      <NA>
    ## 5        <NA>      <NA>
    ## 6        <NA>      <NA>

### Querying by Taxon Name

You can also search by correlated or sampled-as taxon names using a
custom `WHERE` clause:

``` r
# Fetch lab data where the correlated or sampled name is 'Musick' or 'Holland'
# CASE statement handles differences between correlated and sampled names
ldm_taxon <- fetchLDM(WHERE = "(CASE WHEN corr_name IS NOT NULL 
                                THEN LOWER(corr_name) 
                                ELSE LOWER(samp_name) 
                            END) IN ('musick', 'holland')")

print(ldm_taxon)
```

    ## SoilProfileCollection with 40 profiles and 259 horizons
    ## profile ID: pedon_key  |  horizon ID: hzID 
    ## Depth range: 107 - 371 cm
    ## 
    ## ----- Horizons (6 / 259 rows  |  10 / 432 columns) -----
    ##  pedon_key hzID hzn_top hzn_bot hzn_desgn layer_key labsampnum project_key
    ##      11218    1       0      18         A     73461   84P02435        1061
    ##      11218    2      18      36        AB     73462   84P02436        1061
    ##      11218    3      36      64       BAt     73463   84P02437        1061
    ##      11218    4      64      94       Bt1     73464   84P02438        1061
    ##      11218    5      94     152       Bt2     73465   84P02439        1061
    ##      11219    6       0      15       BAt     73466   84P02440        1061
    ##  layer_sequence layer_type
    ##               1    horizon
    ##               2    horizon
    ##               3    horizon
    ##               4    horizon
    ##               5    horizon
    ##               1    horizon
    ## [... more horizons ...]
    ## 
    ## ----- Sites (6 / 40 rows  |  10 / 126 columns) -----
    ##  pedon_key site_key pedlabsampnum pedoniid   upedonid labdatadescflag priority
    ##      11218    11218       84P0457  1313398 83CA019301               0        B
    ##      11219    11219       84P0458  1313407 83CA019302               0        B
    ##      18148    18148       91P0725  1161605 90CA109107               1        B
    ##      18161    18161       91P0738  1161598 90CA109120               1        B
    ##      19043    19043       92P0420    65114 92CA039101               0        B
    ##      19044    19044       92P0421    65115 92CA039102               0        B
    ##  priority2 samp_name samp_class_type
    ##          C   Holland          series
    ##          C    Musick          series
    ##          A    Musick          series
    ##          A   Holland          series
    ##          C   Holland          series
    ##          C   Holland          series
    ## [... more sites ...]
    ## 
    ## Spatial Data:
    ## [EMPTY]

### Advanced Usage: Specific Tables

By default,
[`fetchLDM()`](http://ncss-tech.github.io/soilDB/reference/fetchLDM.md)
retrieves a standard set of tables. You can request specific data, such
as physical properties, using the `tables` argument:

``` r
# Fetch physical properties for soils correlated as "Typic Argialbolls"
ldm_phys <- fetchLDM(x = "Typic Argialbolls", 
                     what = "corr_taxsubgrp", 
                     tables = "lab_physical_properties")

# Inspect the available horizon data columns
names(horizons(ldm_phys))
```

    ##   [1] "layer_key"                         "labsampnum"                       
    ##   [3] "project_key"                       "pedon_key"                        
    ##   [5] "layer_sequence"                    "layer_type"                       
    ##   [7] "layer_field_label_1"               "layer_field_label_2"              
    ##   [9] "layer_field_label_3"               "hzn_top"                          
    ##  [11] "hzn_bot"                           "hzn_desgn_old"                    
    ##  [13] "hzn_desgn"                         "hzn_discontinuity"                
    ##  [15] "hzn_master"                        "hzn_prime"                        
    ##  [17] "hzn_vert_subdvn"                   "hzn_desgn_other"                  
    ##  [19] "non_hzn_desgn"                     "stratified_textures_flag"         
    ##  [21] "texture_description"               "result_source_key"                
    ##  [23] "prep_code"                         "texture_lab"                      
    ##  [25] "particle_size_method"              "clay_total"                       
    ##  [27] "silt_total"                        "sand_total"                       
    ##  [29] "clay_fine"                         "clay_caco3"                       
    ##  [31] "silt_fine"                         "silt_coarse"                      
    ##  [33] "sand_very_fine"                    "sand_fine"                        
    ##  [35] "sand_medium"                       "sand_coarse"                      
    ##  [37] "sand_very_coarse"                  "frag_2_5_mm_wt_pct_lt_75"         
    ##  [39] "frag__2_20_mm_wt_pct_lt_75"        "frag_5_20_mm_wt_pct_lt_75"        
    ##  [41] "frag_20_75_mm_wt_pct_lt_75"        "total_frag_wt_pct_gt_2_mm_ws"     
    ##  [43] "wt_pct_1_tenth_to_75_mm"           "bulk_density_tenth_bar"           
    ##  [45] "bulk_density_tenth_bar_method"     "bulk_density_third_bar"           
    ##  [47] "bulk_density_third_bar_method"     "bulk_density_oven_dry"            
    ##  [49] "bulk_density_oven_dry_method"      "bulk_density_lt_2_mm_air_dry"     
    ##  [51] "bulk_density_air_dry_method"       "bd_third_bar_lt2_reconstituted"   
    ##  [53] "bd_thirdbar_reconstituted_method"  "bulk_den_ovendry_reconstituted"   
    ##  [55] "bulk_de_odreconstituted_method"    "bulk_density_field_moist"         
    ##  [57] "bulk_density_field_moist_metho"    "particle_density_less_than_2mm"   
    ##  [59] "particle_density_lt_2mm_method"    "particle_density_gt_2_mm"         
    ##  [61] "particle_density_gt_2mm_method"    "cole_whole_soil"                  
    ##  [63] "cole_whole_soil_method"            "le_third_fifteen_lt2_mm"          
    ##  [65] "le_third_fifteen_lt2_metho"        "le_third_ovendry_lt_2_mm"         
    ##  [67] "le_third_ovendry_lt_2_mm_metho"    "le_field_moist_to_oben_dry"       
    ##  [69] "le_fm_to_od_method"                "water_retention_0_bar_sieve"      
    ##  [71] "water_retention_0_bar_method"      "water_retention_6_hundredths"     
    ##  [73] "water_retention_6_hund_method"     "water_retention_10th_bar"         
    ##  [75] "water_retention_10th_bar_meth"     "water_retention_third_bar"        
    ##  [77] "water_retention_thirdbar_metho"    "water_retention_1_bar"            
    ##  [79] "water_retention_1_bar_method"      "water_retention_2_bar"            
    ##  [81] "water_retention_2_bar_method"      "water_retention_3_bar_sieve"      
    ##  [83] "water_retention_3_bar_method"      "water_retention_5_bar_sieve"      
    ##  [85] "water_retention_5_bar_method"      "water_retention_15_bar"           
    ##  [87] "water_retention_15_bar_method"     "water_retention_field_state"      
    ##  [89] "water_retention_field_state_me"    "airdry_ovendry_ratio"             
    ##  [91] "atterberg_liquid_limit"            "atterberg_liquid_limit_method"    
    ##  [93] "atterberg_plasticity_index"        "plastic_limit"                    
    ##  [95] "plastic_limit_method"              "aggregate_stability_05_2_mm"      
    ##  [97] "aggregate_stability_05_2_metho"    "le_to_clay_third_bar_to_ovendr"   
    ##  [99] "water_15_bar_to_clay_ratio"        "cec7_clay_ratio"                  
    ## [101] "effective_cec_to_clay_ratio"       "psda_ethanol_dispersion_method"   
    ## [103] "sand_total_ethanol_dispersible"    "silt_total_ethanol_dispersible"   
    ## [105] "clay_total_ethanol_dispersible"    "sand_very_fine_ethanol_dispers"   
    ## [107] "sand_fine_ethanol_dispersible"     "sand_medium_ethanol_dispersibl"   
    ## [109] "sand_coarse_ethanol_dispersibl"    "sand_very_coarse_ethanol_disp"    
    ## [111] "water_dispersible_fraction_method" "clay_tot_h2o_dispersible"         
    ## [113] "clay_fine_h2o_dispersible"         "clay_co3_h2o_dispersible"         
    ## [115] "silt_total_h2o_dispersible"        "silt_fine_h2o_dispersible"        
    ## [117] "silt_coarse_h2o_dispersible"       "sand_total_h2o_dispersible"       
    ## [119] "sand_vf_h2o_dispersible"           "sand_fine_h2o_dispersible"        
    ## [121] "sand_medium_h2o_dispersible"       "sand_coarse_h2o_dispersible"      
    ## [123] "sand_vc_h2o_dispersible"           "color_pyrophosphate_extract"      
    ## [125] "color_pyrophosphate_method"        "bd_thirdbar_before_rewet_organ"   
    ## [127] "bd_before_rewet_organic_method"    "bd_thirdbar_rewet_organic_soil"   
    ## [129] "bd_third_rewet_organic_method"     "bulk_den_rewet_oven_dry"          
    ## [131] "bulk_den_rewet_oven_dry_method"    "mineral_content_loss_on_igniti"   
    ## [133] "mineral_content_loi_method"        "estimated_organic_matter"         
    ## [135] "estimated_om_plus_mineral"         "fiber_analysis_method"            
    ## [137] "fiber_unrubbed"                    "fiber_rubbed"                     
    ## [139] "decomposition_state"               "limnic_material_type"             
    ## [141] "hzID"

`fetchLDM` supports retrieving data from various KSSL tables, including
chemical properties, mineralogy, and x-ray analysis.

------------------------------------------------------------------------

## Spatial Queries

This document mainly focuses on details of custom spatial queries using
the low-level
[`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
function, but there are several functions that provide high-level
convenience interface for spatial queries in `soilDB`. First we will
discuss converting spatial data to Well-Known Text for use in generating
custom queries. Then we will discuss the high- and low-level interfaces
in `soilDB`.

### Converting sf Objects to WKT

**Important:** SDA requires spatial inputs (WKT) to be in **WGS84
(EPSG:4326)** geographic coordinates.

Passing projected coordinates (like UTM or Albers) will result in query
failures or zero results.

SDA does store an alternate projected version of the geometry data (e.g.
`mupolygonproj`, rather than `mupolygongeo`). The projection is a Web
Mercator projection (`"EPSG:3857"`) so it is best avoided in analysis,
though it is an option for visualization web map applications that lack
capability to project the data.

You can use `sf` to define a geometry, such as a bounding recatangle,
and convert it to WKT (Well-Known Text) for spatial queries:

``` r
library(sf)

# Define a bounding box for a region of interest
# (xmin, ymin, xmax, ymax)
bbox <- c(-120.9, 37.7, -120.8, 37.8)
bbox_sf <- st_as_sfc(st_bbox(c(
  xmin = bbox[1],
  ymin = bbox[2],
  xmax = bbox[3],
  ymax = bbox[4]
), crs = 4326))

wkt <- st_as_text(bbox_sf)

wkt
```

    ## [1] "POLYGON ((-120.9 37.7, -120.8 37.7, -120.8 37.8, -120.9 37.8, -120.9 37.7))"

### High-level Functions

The soilDB package provides two higher-level functions
([`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
and
[`fetchSDA_spatial()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md))
that make obtaining spatial data easier. Both of these functions
internally use
[`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
and are often a better option for most use cases compared to crafting a
custom spatial query.

- **[`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)**:
  spatial inputs (e.g. point location, AOI rectangle) =\> spatial or
  tabular outputs

- **[`fetchSDA_spatial()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md)**:
  tabular inputs (e.g. map unit key, area symbol, ecological site ID)
  =\> spatial outputs

The functions take different inputs, and support various outputs
including SSURGO and STATSGO2 map unit polygon geometry, special feature
points/lines/polygons, soil survey area boundaries, Major Land Resource
Area (MLRA) boundaries, or simple tabular information using spatial data
as input. These functions will handle projection to `"EPSG:4326"`
internally as long as the input data has the correct CRS metadata.

``` r
# Example: Retrieve map unit polygons that intersect a bounding box
# This handles the WKT conversion and query construction automatically
# geomIntersection = TRUE clips the polygons to the bounding box

polygons <- SDA_spatialQuery(bbox_sf, what = "mupolygon", geomIntersection = TRUE)
polygons
```

    ## Simple feature collection with 334 features and 2 fields
    ## Geometry type: GEOMETRY
    ## Dimension:     XY
    ## Bounding box:  xmin: -120.9 ymin: 37.7 xmax: -120.8 ymax: 37.8
    ## Geodetic CRS:  WGS 84
    ## First 10 features:
    ##      mukey   area_ac                           geom
    ## 1  1403416  26.74260 POLYGON ((-120.8998 37.7873...
    ## 2  1403416 325.55235 POLYGON ((-120.8872 37.7893...
    ## 3  1403425  53.06925 POLYGON ((-120.8024 37.7863...
    ## 4  1403413 108.53249 POLYGON ((-120.8788 37.7704...
    ## 5  1403412 527.89351 POLYGON ((-120.8945 37.7695...
    ## 6  1403420  82.85632 POLYGON ((-120.8021 37.7895...
    ## 7  1403421  15.05385 POLYGON ((-120.8565 37.7781...
    ## 8  1403414  22.79950 POLYGON ((-120.851 37.77987...
    ## 9  1403416  31.63604 MULTIPOLYGON (((-120.8985 3...
    ## 10 1403413  20.90871 POLYGON ((-120.8963 37.7686...

This section will be expanded in the future, but for now an example
application of the two higher-level function is available in the
[“Dominant Ecological Site”
vignette](http://ncss-tech.github.io/soilDB/articles/dominant-es.md).

### Spatial Queries: The Two-Step Pattern

For optimal performance, separate your spatial and tabular queries.
Fetch the list of intersecting map unit keys (`mukey`) first, then use
those keys to query attribute tables.

This avoids joining heavy geometry columns with complex attribute
tables, which can trigger the 32Mb serialization limit. Then we can use
the WKT in a custom SDA query to get map units intersecting the bounding
box:

``` r
# Step 1: Get the mukeys that intersect the bounding box
q <- sprintf("
  SELECT DISTINCT mu.mukey
  FROM mapunit AS mu
  INNER JOIN mupolygon AS mp ON mp.mukey = mu.mukey
  WHERE mp.mupolygongeo.STIntersects(geometry::STGeomFromText('%s', 4326)) = 1
", wkt)

spatial_result <- SDA_query(q)
head(spatial_result, 5)
```

    ##    mukey
    ## 1 462527
    ## 2 462554
    ## 3 462555
    ## 4 462558
    ## 5 462566

This query uses special geospatial functions defined in SQL Server to
perform the geometric operations (in this case “intersection”)

### SDA Spatial Helper Functions

In addition to standard T-SQL spatial functions, SDA provides
pre-defined **Table-Valued Functions** (TVFs) optimized for
intersections.

- `SDA_Get_Mukey_from_intersection_with_WktWgs84(wkt)`: Returns `mukey`
  values intersecting the input WKT.
- `SDA_Get_MupolygonWktWgs84_from_Mukey(mukey)`: Returns WKT geometry
  for a specific `mukey`.

This makes the logic much simpler for the common operations of finding
`mukey` given a spatial location or geometry:

``` r
# Input MUST be WKT in WGS84 (EPSG:4326)
q <- "SELECT * FROM SDA_Get_Mukey_from_intersection_with_WktWgs84('POINT(-120.9 37.7)')"
result <- SDA_query(q)
```

Also, you can do the inverse (map unit key to geometry):

``` r
# Step 2: Get Geometry from Map Unit Key (mukey)
# Useful for retrieving the polygon boundary for a specific map unit
target_mukey <- 461994

q <- sprintf("SELECT * FROM SDA_Get_MupolygonWktWgs84_from_Mukey('%s')", target_mukey)

# Result contains the mukey and the geometry in WKT format
geometry_df <- SDA_query(q)

head(geometry_df)
```

    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           MupolygonWktWgs84
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 POLYGON ((-121.032455555662 37.9592152603984, -121.03285202503 37.9591849129885, -121.033072907654 37.9591081653858, -121.033328454278 37.9589770304221, -121.033728769482 37.9588035660117, -121.033900712322 37.9587435334631, -121.034102285527 37.9587105626138, -121.034226937322 37.9587131055087, -121.034372855748 37.9587426135551, -121.034498021914 37.9588254436972, -121.034637122971 37.9588730126591, -121.034725363384 37.9588563818132, -121.034868509489 37.9590389438022, -121.034869562297 37.9591653228325, -121.03475359382 37.9593522473175, -121.034173981163 37.9596399606279, -121.033980610935 37.9597719381858, -121.032316040062 37.961123123151, -121.032060957959 37.9612624734441, -121.031769961277 37.9613566706344, -121.031605303085 37.9614890992556, -121.031372905269 37.961926794296, -121.031278725938 37.962186772691, -121.031253880828 37.9624202209759, -121.030979949929 37.963433991072, -121.030853664442 37.964009330753, -121.030613352925 37.9642666362517, -121.029817684926 37.9645320941986, -121.029575343951 37.9644922710971, -121.029447384597 37.9642917390781, -121.029472874344 37.963967598828, -121.029415124536 37.9637773179969, -121.029447904803 37.9635255769229, -121.029781358193 37.9629641602046, -121.029924150065 37.9626145365407, -121.030001933825 37.9621565455648, -121.029944756724 37.9620021837882, -121.029825937826 37.9618108279156, -121.029767043665 37.9615487094568, -121.029807213431 37.9613148261005, -121.029769648729 37.9610353050336, -121.029996887309 37.9608500418744, -121.030561206747 37.9604805411635, -121.030732846467 37.9602311429487, -121.03109388638 37.9595881670532, -121.031321316698 37.9593760614507, -121.031430649793 37.9592968000317, -121.031652007924 37.9592282724891, -121.031900776861 37.9591872716173, -121.032455555662 37.9592152603984))
    ## 2 POLYGON ((-120.977440277027 37.8400202039719, -120.977735828159 37.8398271562088, -120.977918761599 37.8396863165759, -120.978089096334 37.8394815881836, -120.978189004401 37.839275889939, -120.978286303194 37.8389081212846, -120.978279774157 37.8385389812703, -120.978301713885 37.8381250774638, -120.97823247016 37.8377635179397, -120.978084402863 37.8372290429494, -120.977901893339 37.8370002134407, -120.977557934853 37.8364901013732, -120.97725676047 37.8359525569539, -120.977205941479 37.8357266595, -120.977202148334 37.8355371188211, -120.97746648077 37.834821175063, -120.977614815185 37.8346524635618, -120.977758216532 37.8344385863328, -120.97803793374 37.8342719453342, -120.978248634626 37.8341049638797, -120.978397090106 37.8339537805788, -120.978723965594 37.8335629096373, -120.978804868982 37.8333297714404, -120.978860203523 37.8329162686792, -120.978716374806 37.8325176537409, -120.978649511054 37.8324170767535, -120.978572686261 37.8320644960726, -120.97856319208 37.8318207102925, -120.978589049034 37.8316138758532, -120.978824752425 37.8312032773233, -120.978988121178 37.8309990766935, -120.979217329881 37.8306692963475, -120.979303459649 37.8304821847855, -120.979301423206 37.8302118906905, -120.979187675825 37.830020415361, -120.979112886686 37.8299381289761, -120.978948686889 37.8298274341976, -120.978779132488 37.8296531828803, -120.978529093434 37.8293423995408, -120.978491819954 37.8291247549243, -120.978544129924 37.8289188513919, -120.978646331634 37.8286683190097, -120.978677244149 37.8285241791737, -120.978954614895 37.8278082658793, -120.978977475239 37.8276648871392, -120.978967419536 37.8274572529753, -120.978799771258 37.827147705694, -120.978630303077 37.8270011561598, -120.978480434303 37.826835720976, -120.978024032673 37.8264674195749, -120.977824916161 37.8263381624385, -120.9777613674 37.8261029186895, -120.977793060477 37.8258957866904, -120.97794896038 37.825718095457, -120.978220141663 37.8256779676964, -120.978467354419 37.8257814498629, -120.978692923239 37.8259116369393, -120.978891697534 37.826050203977, -120.979159355796 37.8261715808214, -120.979490920784 37.8262407743618, -120.979836341971 37.8262470092522, -120.980204216951 37.8262254440749, -120.980815245795 37.8261548276568, -120.981098727404 37.8261777203038, -120.981387006712 37.8263003203597, -120.981520421313 37.826572681762, -120.981545320834 37.8266995967704, -120.981532882231 37.8269692269045, -120.981391196465 37.8275347861969, -120.981257240825 37.8279647516823, -120.981217960856 37.8282529354896, -120.98124735985 37.828478696968, -120.981311127445 37.8287591700524, -120.981384021796 37.8289767510116, -120.981387961865 37.8291117492463, -120.981096081815 37.8298371765903, -120.981003698192 37.8300061963217, -120.980539929736 37.830674278857, -120.980135048729 37.8312173090063, -120.98005338666 37.8314413687206, -120.979986486042 37.8320171399379, -120.979941980294 37.8321872243406, -120.979944579446 37.8324213665481, -120.9799007269 37.8325109295029, -120.979918930859 37.8326734324694, -120.979970534168 37.8328363366954, -120.980057707982 37.8330093513418, -120.980312904842 37.8334105279939, -120.980536646829 37.8336658343484, -120.980686751617 37.8338044263142, -120.980912131345 37.8339614495489, -120.981266005293 37.8343456682067, -120.9813572608 37.8345991374574, -120.981449934201 37.8347811646841, -120.981501530914 37.8350161398622, -120.981454115343 37.8352773826639, -120.981359406411 37.8355633074607, -120.981126752857 37.836082299963, -120.980943137716 37.8363138387317, -120.980752540785 37.8365080317902, -120.980296812808 37.8367530942482, -120.980202533009 37.8369853371646, -120.979995036867 37.8373161153888, -120.979824756851 37.8375106736617, -120.979476083896 37.8379827951916, -120.979337943284 37.8383046000667, -120.979065401171 37.8387598625711, -120.978901454605 37.8390002147039, -120.978771674242 37.8392500476565, -120.978666996045 37.8396720268917, -120.978645618909 37.8400497789281, -120.978725416744 37.8402770044867, -120.978556685386 37.840345576608, -120.977994970428 37.8403633352033, -120.977780754313 37.8403416371155, -120.977663765668 37.8403125285352, -120.977602251142 37.8402755073615, -120.977508129714 37.8401030205027, -120.977440277027 37.8400202039719))
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      POLYGON ((-120.985523088218 37.9059362513452, -120.985891567239 37.9059598999602, -120.986299486427 37.9060390524287, -120.986459355563 37.9060508909253, -120.986639996869 37.9060094145871, -120.987041369765 37.9058533983687, -120.987208918771 37.905811887389, -120.987458299277 37.9057800557708, -120.987652493843 37.9058189001273, -120.988233780422 37.906216589848, -120.98843454242 37.9062743869922, -120.988843574818 37.9063532990626, -120.989217084966 37.9064498037916, -120.989389701677 37.9065430539036, -120.989583343109 37.9066180464921, -120.989722527941 37.9067108974974, -120.989854249232 37.906830256895, -120.989915949246 37.9069568710404, -120.989825322146 37.9071449150441, -120.989491922873 37.9073015057732, -120.989436188876 37.9072641929805, -120.989346680326 37.9072357926623, -120.989124330051 37.9073044762055, -120.988895623832 37.9073994359224, -120.988222545241 37.907766589578, -120.987944370208 37.9078423266246, -120.987785562334 37.907840430903, -120.987543137214 37.9077275919775, -120.987376313195 37.9077162847434, -120.987182918211 37.9077484171905, -120.986960133373 37.9078707769975, -120.986835303725 37.907977269445, -120.98664837129 37.9082168655059, -120.986480550665 37.9083674591968, -120.986258536355 37.9084268263362, -120.986092945979 37.9084328143834, -120.985711622244 37.9083721196174, -120.985516856291 37.908441494137, -120.985420000626 37.9085291988148, -120.985342444448 37.908789344077, -120.985127005318 37.9091457732408, -120.984973312415 37.9095225516516, -120.984402502985 37.9105935975282, -120.984131761949 37.9107869589363, -120.984034685975 37.9105513221075, -120.98390352642 37.9103958052679, -120.98346043993 37.9100366434719, -120.983259762883 37.9099344691991, -120.983108389692 37.9097961142513, -120.983018134086 37.909614488707, -120.982984170679 37.9094340275789, -120.982970467749 37.9092262309921, -120.983005464254 37.9090285780751, -120.983069584541 37.908849781913, -120.983118665735 37.9085623317296, -120.983208742576 37.9084825146724, -120.984131089911 37.9082822469208, -120.984526413222 37.9082254475723, -120.984686087165 37.9081199871405, -120.984756252956 37.9079861228242, -120.984707787014 37.9078049983975, -120.984576505041 37.9076319532808, -120.984383351417 37.9074210291711, -120.984286486804 37.9072306229818, -120.984224290109 37.907057913864, -120.984301713878 37.9068523109911, -120.98438517861 37.9067637115338, -120.984550908203 37.9066311133469, -120.984752369443 37.906526152798, -120.985002185842 37.9064406459829, -120.985183304963 37.9063353188541, -120.985294574459 37.9062205763299, -120.985377954947 37.9061042745238, -120.985440400018 37.905961860195, -120.985523088218 37.9059362513452))
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       POLYGON ((-120.98392012538 37.9120092608573, -120.983920116288 37.9120813314766, -120.983877330105 37.9121706626422, -120.983863795059 37.9122963792718, -120.983890819501 37.9124052990935, -120.983876490532 37.9127381482558, -120.983897239166 37.9129010459885, -120.984000810323 37.9130558648315, -120.984068854303 37.9132282760937, -120.984061793966 37.9134173159296, -120.983999205165 37.9136142710604, -120.983970756462 37.9139750195284, -120.983990431669 37.9142000469498, -120.983920114666 37.9144605229553, -120.983733834268 37.9144033823299, -120.983512151504 37.9143813626691, -120.983276402284 37.9143049985795, -120.98315192843 37.9141859642132, -120.983138874876 37.913897647622, -120.983027640279 37.9136520350459, -120.982827089637 37.9134953184344, -120.98263433916 37.9133748528511, -120.982281177519 37.9130624826414, -120.981983911426 37.9125887446493, -120.981831632573 37.9124958505786, -120.981554726659 37.9124826032185, -120.981187656278 37.9125214691775, -120.980937266457 37.9125710487223, -120.980701936796 37.9125851407342, -120.980507772493 37.9125361126669, -120.980501406417 37.9123563496652, -120.980578329891 37.9121767278483, -120.980550867476 37.9119154482948, -120.980717599403 37.9117650946736, -120.981404770214 37.9117406093536, -120.981805901404 37.9117657645214, -120.98213205474 37.9118250747223, -120.982450263095 37.9119747450009, -120.982838978153 37.9120533107828, -120.98311617122 37.9121394874135, -120.983378532244 37.9121622400764, -120.983565925004 37.9121470787842, -120.983726240308 37.9121052406898, -120.98392012538 37.9120092608573))
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                POLYGON ((-120.976618752919 37.8950135458249, -120.976335244308 37.8948917321003, -120.976321844539 37.8947568666454, -120.97644103155 37.8943078568389, -120.976406106564 37.8941451541604, -120.97632346141 37.8940266151981, -120.97582601447 37.8935134340163, -120.975695104895 37.8934031386057, -120.975515164428 37.8932920075564, -120.975141114774 37.8931318394823, -120.974835950129 37.893036726673, -120.974399641239 37.8929302273106, -120.974137351298 37.8929074553091, -120.973776312456 37.8929016327187, -120.97339529942 37.8928140592313, -120.973090825587 37.8927003197221, -120.972862223499 37.8925069662265, -120.972606115165 37.8923406135282, -120.972392314346 37.8922827548895, -120.972128697888 37.8922870504914, -120.971414098739 37.8923828538791, -120.97071429361 37.8926141490595, -120.970346506972 37.8927880449445, -120.970089249558 37.8927660604654, -120.970090747333 37.8926123758252, -120.970194007042 37.8924882346052, -120.970312007212 37.8923998167057, -120.970659111067 37.8922348638255, -120.971117728715 37.8919902380063, -120.971339361943 37.8919402093411, -120.971651463785 37.8919008329444, -120.971922028594 37.891896009406, -120.972254025244 37.8919106798158, -120.972593814151 37.8919616014651, -120.973140557808 37.8921605087601, -120.973556944765 37.8922850354301, -120.973806065242 37.892280071726, -120.974054984254 37.8922298767936, -120.974444063149 37.8921106474734, -120.974707678843 37.8921063468797, -120.975143842044 37.8922673870532, -120.976831996534 37.8932153157039, -120.976970295455 37.8932714003401, -120.97713624351 37.8934279569021, -120.977364047673 37.8938284355726, -120.977314296968 37.8941964046634, -120.977237165285 37.894402865158, -120.977251411437 37.8945745116158, -120.977465674558 37.8947126503714, -120.977803670201 37.8948544825232, -120.978116546977 37.8950302087813, -120.978205525081 37.8952666121033, -120.978108538929 37.8954088530654, -120.977989558961 37.8955150392216, -120.97784419553 37.8955759250091, -120.977635660701 37.8955539294061, -120.977484187976 37.8954701090048, -120.97694512638 37.8950737316129, -120.976618752919 37.8950135458249))
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             POLYGON ((-120.958148228164 37.8929009967531, -120.958257566342 37.89339922832, -120.958242554141 37.8937682268486, -120.958152399799 37.893992167427, -120.958207050548 37.8941913968848, -120.958255178651 37.894299599785, -120.958337901348 37.8943737818444, -120.958406631387 37.8945552874435, -120.95827453497 37.8946614175666, -120.957956155354 37.8946826657254, -120.957588087146 37.8946496217747, -120.957192552933 37.8946334017232, -120.956721843593 37.894544192444, -120.956410676599 37.8944659975247, -120.956058396377 37.8940730302104, -120.956079378789 37.8938931079596, -120.956371088193 37.8938256981606, -120.956911733464 37.8938799710885, -120.95711167439 37.8939287402782, -120.957326802893 37.8939595538745, -120.957569551839 37.8939190001486, -120.957736981051 37.8937500551254, -120.957757993901 37.8934980617213, -120.957702914505 37.8932185433083, -120.957663591713 37.8927130748242, -120.957693027833 37.8921451979903, -120.957590286763 37.8915938506515, -120.957549456741 37.8914758081609, -120.957486835947 37.8913669379693, -120.957135291263 37.89088328039, -120.957128531351 37.8907571957377, -120.957185224437 37.8906049191925, -120.957268166577 37.8904804183495, -120.957282434107 37.8903360819168, -120.957138383743 37.8899642861266, -120.956883152836 37.8894909945394, -120.957056677266 37.8893946863583, -120.957257940983 37.8894163835871, -120.957568148277 37.8896740078802, -120.957623281227 37.8897474865796, -120.957809306399 37.8901650217306, -120.957896776288 37.8913561222369, -120.957875227461 37.8915721961394, -120.957929495688 37.8918528078143, -120.957962159765 37.8925555331955, -120.958010396191 37.8927912105813, -120.958148228164 37.8929009967531))

Using the `sf` (or `terra`) package, you can convert the resulting text
WKT column to a spatial object:

``` r
if (requireNamespace("sf", quietly = TRUE) &&
   !is.null(geometry_df)) {
  # Parse WKT column (usually named 'mupolygongeo' in mupolygon table, but 'MupolygonWktWgs84' in TVF result)
  poly_sf <- sf::st_as_sfc(geometry_df$MupolygonWktWgs84, crs = 4326)
  
  plot(poly_sf, main = paste("Geometry for mukey=", target_mukey))
}
```

![](sda_files/figure-html/unnamed-chunk-1-1.png)

**Note:** The TVF function name implies the CRS is WGS84 (authority:code
`EPSG:4326`, SRS ID `4326`; or, more correctly, `OGC:CRS84`).

See the [SDA Advanced Query
Guide](https://sdmdataaccess.nrcs.usda.gov/documents/AdvancedQueries.html)
for more details on TVF functions and other high-level features built
into SDA.

### Spatial and Tabular Property Integration

We can use the result of our spatial filtering with property queries.

Here we extract the unique map unit keys and pass as `mukeys` argument
to
[`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)

``` r
# Step 2: Use the mukeys to fetch tabular data
# First, get mukeys in bounding box
spatial_mukeys <- unique(spatial_result$mukey)

# Then query properties for those mukeys
if (length(spatial_mukeys) > 0) {
  clay_in_bbox <- get_SDA_property(
    property = "Total Clay - Rep Value",
    method = "Weighted Average",
    mukeys = spatial_mukeys,
    top_depth = 0,
    bottom_depth = 50
  )
  
  head(clay_in_bbox)
}
```

    ##     mukey areasymbol musym
    ## 1 1403410      CA632   127
    ## 2 1403412      CA632   130
    ## 3 1403413      CA632   131
    ## 4 1403414      CA632   134
    ## 5 2766111      CA632   142
    ## 6 1403416      CA632   157
    ##                                                                                muname
    ## 1                                           Chuloak sandy loam, 0 to 2 percent slopes
    ## 2                 Columbia sandy loam, drained, 0 to 2 percent slopes, rarely flooded
    ## 3 Columbia sandy loam, partially drained, 0 to 2 percent slopes, occasionally flooded
    ## 4                                            Cometa sandy loam, 2 to 8 percent slopes
    ## 5                                    Delhi loamy sand, 0 to 2 percent slopes, MLRA 17
    ## 6                                       Exeter sandy clay loam, 0 to 2 percent slopes
    ##   claytotal_r
    ## 1       17.80
    ## 2       17.08
    ## 3       15.38
    ## 4       20.00
    ## 5        2.22
    ## 6       18.60

This shows how to get the data, see the [Local SSURGO
vignette](http://ncss-tech.github.io/soilDB/articles/local-ssurgo.md)
for more spatial visualization examples using `sf`.

------------------------------------------------------------------------

## Integration Examples: Combining Multiple Functions

### Example 1: Multi-Method Property Comparison

Compare the same property using different aggregation methods:

``` r
# Get clay content using three different methods
methods <- c("Dominant Component (Numeric)", "Weighted Average", "Max")

clay_results <- data.frame()

for (method in methods) {
  result <- get_SDA_property(
    property = "Total Clay - Rep Value",
    method = method,
    areasymbols = "CA630",
    top_depth = 0,
    bottom_depth = 50
  )
  
  result$method <- method
  clay_results <- rbind(clay_results, result)
}

# compare methods for a single map unit
subset(clay_results, mukey == 1865918)
```

    ##       mukey areasymbol musym
    ## 17  1865918      CA630  3046
    ## 146 1865918      CA630  3046
    ## 259 1865918      CA630  3046
    ##                                                          muname claytotal_r
    ## 17  Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes    10.00000
    ## 146 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes    10.76712
    ## 259 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes    12.00000
    ##                           method
    ## 17  Dominant Component (Numeric)
    ## 146             Weighted Average
    ## 259                          Max

### Example 2: Properties and Interpretations for Land Use Planning

Combine soil properties and interpretations for a land use suitability
assessment:

``` r
# Get drainage class and hydrologic group
drain_hydro <- get_SDA_property(
  property = c("Drainage Class", "Hydrologic Group"),
  method = "Dominant Condition",
  areasymbols = "CA630"
)

# Get an engineering interpretation
eng_interp <- get_SDA_interpretation(
  rulename = "ENG - Septic Tank Absorption Fields",
  method = "Dominant Condition",
  areasymbols = "CA630"
)

# Explicitly merge on mukey (and other shared columns to avoid duplication)
suitability <- merge(drain_hydro, eng_interp, 
                     by = c("mukey", "areasymbol", "musym", "muname"))

head(suitability)
```

    ##     mukey areasymbol musym
    ## 1 1865918      CA630  3046
    ## 2 1865926      CA630  7088
    ## 3 1865927      CA630  7155
    ## 4 1865928      CA630  7156
    ## 5 1865929      CA630  8033
    ## 6 1865930      CA630  8034
    ##                                                          muname
    ## 1   Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes
    ## 2       Loafercreek-Gopheridge complex, 30 to 60 percent slopes
    ## 3            Crimeahouse-Sixbit complex, 3 to 15 percent slopes
    ## 4           Crimeahouse-Sixbit complex, 15 to 30 percent slopes
    ## 5  Copperopolis-Whiterock complex, 2 to 8 percent slopes, rocky
    ## 6 Copperopolis-Whiterock complex, 3 to 15 percent slopes, rocky
    ##                drainagecl hydgrp rating_ENGSepticTankAbsorptionFields
    ## 1 Moderately well drained      D                                    1
    ## 2            Well drained      C                                    1
    ## 3            Well drained      C                                    1
    ## 4            Well drained      C                                    1
    ## 5            Well drained      D                                    1
    ## 6            Well drained      D                                    1
    ##   total_comppct_ENGSepticTankAbsorptionFields
    ## 1                                          80
    ## 2                                          94
    ## 3                                          97
    ## 4                                          94
    ## 5                                          88
    ## 6                                          90
    ##   class_ENGSepticTankAbsorptionFields
    ## 1                        Very limited
    ## 2                        Very limited
    ## 3                        Very limited
    ## 4                        Very limited
    ## 5                        Very limited
    ## 6                        Very limited
    ##                                                                                                                                                                                                      reason_ENGSepticTankAbsorptionFields
    ## 1                                                                   Ponded > 4 hours "Ponding" (1); Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Wet, Ground Water Near the Surface (120 - 180cm) "Depth to saturated zone" (1)
    ## 2                                                                                                                                                     Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Slope 8 to > 15% "Slope" (1)
    ## 3 Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Slope 8 to > 15% "Slope" (0.633); Percolation 60 - 180cm (24-72") "Slow water movement" (0.498); Large Stones - Fract. >3in, 24.9 to >50%, Wght. Av. 0-40 "Large stones" (0.004)
    ## 4                                                                     Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Slope 8 to > 15% "Slope" (1); Large Stones - Fract. >3in, 24.9 to >50%, Wght. Av. 0-40 "Large stones" (0.13)
    ## 5                                                                                                                                                                                   Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1)
    ## 6                                                                                                                                                 Shallow to Bedrock 100 - 180cm "Depth to bedrock" (1); Slope 8 to > 15% "Slope" (0.367)

### Example 3: Hydric Status and Interpretations

Combine hydric classifications with wetland-related interpretations:

``` r
# Get a generalized mapunit-level hydric classification
# see ?get_SDA_hydric for details on method="mapunit"
hydric_stat <- get_SDA_hydric(
  areasymbols = "CA077",
  method = "MAPUNIT"
)

wet_interp <- get_SDA_interpretation(
  rulename = "WMS - Excavated Ponds (Aquifer-fed)",
  method = "Dominant Condition",
  areasymbols = "CA077"
)

wetland_assess <- merge(hydric_stat, wet_interp,
                        by = c("mukey", "areasymbol", "musym", "muname"),
                        all.x = TRUE)

subset(wetland_assess, rating_WMSExcavatedPondsAquiferfed < 0.6)
```

    ##      mukey areasymbol musym
    ## 66  462051      CA077   159
    ## 111 462096      CA077   204
    ## 112 462097      CA077   205
    ## 131 462116      CA077   224
    ## 132 462117      CA077   225
    ## 137 462122      CA077   230
    ## 138 462123      CA077   231
    ## 140 462125      CA077   233
    ## 168 462153      CA077   261
    ## 170 462155      CA077   263
    ## 171 462156      CA077   264
    ##                                                                                          muname
    ## 66                              Fluvaquents, 0 to 2 percent slopes, frequently flooded, MLRA 16
    ## 111                  Peltier mucky clay loam, partially drained, 0 to 2 percent slopes, MLRA 16
    ## 112       Peltier mucky clay loam, organic substratum, partially drained, 0 to 2 percent slopes
    ## 131                   Rindge mucky silt loam, partially drained, 0 to 2 percent slopes, MLRA 16
    ## 132                              Rindge muck, 0 to 2 percent slopes, partially drained, MLRA 16
    ## 137                           Ryde clay loam, partially drained, 0 to 2 percent slopes, MLRA 16
    ## 138 Ryde silty clay loam, organic substratum, partially drained, 0 to 2 percent slopes, MLRA 16
    ## 140                     Ryde-Peltier complex, partially drained, 0 to 2 percent slopes, MLRA 16
    ## 168     Valdez silt loam, organic substratum, partially drained, 0 to 2 percent slopes, MLRA 16
    ## 170                Venice mucky silt loam, partially drained, 0 to 2 percent slopes, overwashed
    ## 171                              Venice muck, partially drained, 0 to 2 percent slopes, MLRA 16
    ##     total_comppct hydric_majors hydric_inclusions        HYDRIC_RATING
    ## 66            100            85                15               Hydric
    ## 111           100            85                15               Hydric
    ## 112           100            85                12 Predominantly Hydric
    ## 131           100            85                15               Hydric
    ## 132           100            85                15               Hydric
    ## 137           100            85                15               Hydric
    ## 138           100            85                15               Hydric
    ## 140           100            85                15               Hydric
    ## 168           100            85                15               Hydric
    ## 170           100            85                15               Hydric
    ## 171           100            85                15               Hydric
    ##     rating_WMSExcavatedPondsAquiferfed
    ## 66                                0.30
    ## 111                               0.54
    ## 112                               0.54
    ## 131                               0.48
    ## 132                               0.10
    ## 137                               0.54
    ## 138                               0.54
    ## 140                               0.54
    ## 168                               0.54
    ## 170                               0.54
    ## 171                               0.54
    ##     total_comppct_WMSExcavatedPondsAquiferfed class_WMSExcavatedPondsAquiferfed
    ## 66                                         96                  Somewhat limited
    ## 111                                        85                  Somewhat limited
    ## 112                                        85                  Somewhat limited
    ## 131                                        93                  Somewhat limited
    ## 132                                        85                  Somewhat limited
    ## 137                                        85                  Somewhat limited
    ## 138                                        89                  Somewhat limited
    ## 140                                        85                  Somewhat limited
    ## 168                                        85                  Somewhat limited
    ## 170                                        85                  Somewhat limited
    ## 171                                       100                  Somewhat limited
    ##                                                                                                                                                                                            reason_WMSExcavatedPondsAquiferfed
    ## 66                                                                                                                                                  Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.1)
    ## 111 Deep to Apparent Water Table "Depth to saturated zone" (0.543); Percolation Rate (Layers Within an Apparent Water Table) "Slow refill" (0.296); Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.1)
    ## 112                                                                                 Deep to Apparent Water Table "Depth to saturated zone" (0.543); Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.5)
    ## 131                                                                                                                                                 Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.1)
    ## 132                                                                                                                                                 Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.1)
    ## 137 Deep to Apparent Water Table "Depth to saturated zone" (0.543); Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.5); Percolation Rate (Layers Within an Apparent Water Table) "Slow refill" (0.296)
    ## 138                                                                                 Deep to Apparent Water Table "Depth to saturated zone" (0.543); Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.5)
    ## 140 Deep to Apparent Water Table "Depth to saturated zone" (0.543); Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.5); Percolation Rate (Layers Within an Apparent Water Table) "Slow refill" (0.296)
    ## 168                                                                                 Deep to Apparent Water Table "Depth to saturated zone" (0.543); Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.5)
    ## 170                                                                                 Deep to Apparent Water Table "Depth to saturated zone" (0.543); Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.1)
    ## 171                                                                                 Deep to Apparent Water Table "Depth to saturated zone" (0.543); Cutbank Caving and Apparent Water Table "Unstable excavation walls" (0.1)

### Example 4: Aggregating Across Multiple Areas

Here we use base R
[`aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
(feel free to swap in your favorite R aggregation method e.g. `dplyr`,
`data.table`, or `collapse`) to summarize across survey areas:

``` r
# Get properties for two areas
props_ca630 <- get_SDA_property(
  property = "Total Clay - Rep Value",
  method = "Dominant Component (Numeric)",
  areasymbols = "CA630"
)

props_ca649 <- get_SDA_property(
  property = "Total Clay - Rep Value",
  method = "Dominant Component (Numeric)",
  areasymbols = "CA649"
)

# Combine
all_props <- rbind(props_ca630, props_ca649)

# Aggregate by area symbol
area_summary <- aggregate(
  claytotal_r ~ areasymbol,
  data = all_props,
  FUN = function(x) {
    c(
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      n = sum(!is.na(x))
    )
  }
)

area_summary
```

    ##   areasymbol claytotal_r.mean claytotal_r.median claytotal_r.sd claytotal_r.n
    ## 1      CA630        23.646480          23.283968       8.779233    126.000000
    ## 2      CA649        24.484864          24.710145       8.016048     85.000000

### Example 5: Component-Level Analysis

Use `method = "None"` to perform custom component-level analysis:

``` r
# Get all component properties without aggregation
clay_by_comp <- get_SDA_property(
  property = "Total Clay - Rep Value",
  method = "None",
  areasymbols = "CA630",
  top_depth = 0,
  bottom_depth = 25,
  include_minors = TRUE
)

head(clay_by_comp)
```

    ##     mukey    cokey areasymbol musym
    ## 1 1865918 26411696      CA630  3046
    ## 2 1865918 26411696      CA630  3046
    ## 3 1865918 26411695      CA630  3046
    ## 4 1865918 26411695      CA630  3046
    ## 5 1865918 26411695      CA630  3046
    ## 6 1865918 26411698      CA630  3046
    ##                                                        muname          compname
    ## 1 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes          Goldwall
    ## 2 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes          Goldwall
    ## 3 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes            Toomes
    ## 4 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes            Toomes
    ## 5 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes            Toomes
    ## 6 Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes Ultic Argixerolls
    ##             compkind comppct_r majcompflag    chkey hzdept_r hzdepb_r
    ## 1             Series        45         Yes 78806274        0       15
    ## 2             Series        45         Yes 78806273       15      200
    ## 3             Series        28         Yes 78806272        0        2
    ## 4             Series        28         Yes 78806271        2       33
    ## 5             Series        28         Yes 78806270       33      200
    ## 6 Taxon above family         5         No  78806281        0       25
    ##   claytotal_r
    ## 1          10
    ## 2          NA
    ## 3          12
    ## 4          12
    ## 5          NA
    ## 6          15

``` r
# Calculate average clay by major vs. minor components
clay_summary <- aggregate(
  claytotal_r ~ majcompflag,
  data = clay_by_comp,
  FUN = mean
)

clay_summary
```

    ##   majcompflag claytotal_r
    ## 1         No     22.94837
    ## 2         Yes    22.63659

------------------------------------------------------------------------

## Summary and Best Practices

### Key Takeaways

1.  **Use
    [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)**
    for custom SQL queries when predefined functions don’t fit your
    needs
2.  **T-SQL syntax**: Use `TOP`, `ISNULL()`, `CAST()` for SDA queries
3.  **Query limits**: Keep result sets \< 100k records and \< 32 Mb; use
    chunking for large datasets
4.  **Aggregation methods**: Choose an appropriate method
    (`Dominant Component`, `Weighted Average`, `None`, etc.) for your
    analysis
5.  **Local vs. remote**: Use `dsn` parameter to query local SSURGO
    databases (see [Local SSURGO
    vignette](http://ncss-tech.github.io/soilDB/articles/local-ssurgo.md))
6.  **Spatial queries**: Combine WKT geometry with
    [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
    for location-based searches (or use
    [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md))

### Recommended Workflow

1.  Start with small test queries using e.g. `TOP 10`, or a small number
    of inputs, to verify syntax and result table structure
2.  Use `query_string = TRUE` to inspect generated SQL
3.  Scale up with `areasymbols` or `mukeys` parameters
4.  Use chunking for “large” data sets (greater than ~10,000 items, see
    [`makeChunks()`](http://ncss-tech.github.io/soilDB/reference/makeChunks.md)
    section for suggested chunk sizes)
5.  Cache results locally if repeating queries
6.  Refer to official SDA documentation for table/column details

### SDA Resources

- **SDA Table Relationships Diagram:**
  <https://sdmdataaccess.sc.egov.usda.gov/documents/TableRelationshipsDiagram.pdf>
- **Column Descriptions:**
  <https://www.nrcs.usda.gov/sites/default/files/2022-08/SSURGO-Metadata-Table-Column-Descriptions-Report.pdf>
- [SDA Query Help](https://sdmdataaccess.nrcs.usda.gov/queryhelp.aspx)
- [SDA Query
  Samples](https://sdmdataaccess.nrcs.usda.gov/queryhelp.aspx#Samples)
- [SDA Table/Column
  Descriptions](https://sdmdataaccess.nrcs.usda.gov/documents/TableColumnDescriptionsReport.pdf)
- [soilDB Reference
  Index](https://ncss-tech.github.io/soilDB/reference/index.html)
- [Local SSURGO
  Vignette](http://ncss-tech.github.io/soilDB/articles/local-ssurgo.md) -
  For creating and querying local SSURGO databases
- [Dominant Ecological Sites
  Vignette](http://ncss-tech.github.io/soilDB/articles/dominant-es.md) -
  For a fully worked example of combining spatial data with tabular
  (Ecological Site) data to make thematic maps

------------------------------------------------------------------------

## Appendix: Common SDA Tables and Columns

### Core Tables

| Table        | Primary Key     | Parent Table | Description                      |
|--------------|-----------------|--------------|----------------------------------|
| `legend`     | `lkey`          | N/A          | Soil survey area (e.g., CA630)   |
| `mapunit`    | `mukey`         | `legend`     | Delineated soil mapping unit     |
| `component`  | `cokey`         | `mapunit`    | Soil component within a map unit |
| `chorizon`   | `chkey`         | `component`  | Soil horizon within a component  |
| `cointerp`   | `cointerpkey`   | `component`  | Component-level interpretations  |
| `coecoclass` | `coecoclasskey` | `component`  | Component ecological classes     |
| `copmgrp`    | `copmgrpkey`    | `component`  | Component parent material groups |
| `mupolygon`  | `mupolygonkey`  | N/A          | Map unit polygons                |

### Common Columns

| Column         | Table     | Description                                                                      | Example                                           |
|----------------|-----------|----------------------------------------------------------------------------------|---------------------------------------------------|
| `areasymbol`   | legend    | Soil survey area code                                                            | “CA630”                                           |
| `musym`        | mapunit   | Map unit symbol                                                                  | “7159”                                            |
| `muname`       | mapunit   | Map unit name                                                                    | “Sierra coarse sandy loam, 3 to 8 percent slopes” |
| `compname`     | component | Component/soil series name                                                       | “Sierra”                                          |
| `comppct_r`    | component | Component percentage (representative)                                            | 85                                                |
| `majcompflag`  | component | Major component flag                                                             | “Yes”/“No”                                        |
| `claytotal_r`  | chorizon  | Total clay percentage                                                            | 12.5                                              |
| `hzdept_r`     | chorizon  | Horizon depth top (cm)                                                           | 0                                                 |
| `hzdepb_r`     | chorizon  | Horizon depth bottom (cm)                                                        | 25                                                |
| `hydricrating` | component | Hydric soil status                                                               | “Yes”/“No”                                        |
| `mrulename`    | cointerp  | Interpretation rule name                                                         | “FOR - Mechanical Planting Suitability”           |
| `interphr`     | cointerp  | Interpretation rating/class                                                      | “Somewhat Limited”                                |
| `mupolygongeo` | mupolygon | Map unit polygon geometry (WKT; WGS84 geographic coordinates in decimal degrees) | “POLYGON (…)”                                     |

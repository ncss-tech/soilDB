# Changelog

## soilDB 2.8.14 (development)

- [`taxaExtent()`](http://ncss-tech.github.io/soilDB/reference/taxaExtent.md)
  updates:
  - Added family mineralogy class grids
  - Added argument `type` for selecting the type of query; replaces
    deprecated `formativeElement`

## soilDB 2.8.13 (2025-09-26)

CRAN release: 2025-09-27

- Added
  [`get_SDA_NASIS_key()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_NASIS_key.md)
  for obtaining NASIS record IDs for component and component horizon
  data from Soil Data Access
  ([\#409](https://github.com/ncss-tech/soilDB/issues/409))
- [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
  fixed a bug with new `addFields` argument and `geomIntersection=TRUE`
  ([\#414](https://github.com/ncss-tech/soilDB/issues/414))
- [`get_OSD()`](http://ncss-tech.github.io/soilDB/reference/get_OSD.md)
  replace old base URL for `result='html'`
- [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
  include soilDB package version in User-Agent string
- Vignette updates

## soilDB 2.8.12 (2025-08-29)

CRAN release: 2025-08-29

- **New Vignettes**:
  - Added vignette for thematic maps of dominant ecological site using
    Soil Data Access via
    [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
    and
    [`get_SDA_coecoclass()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_coecoclass.md)
  - Added vignette for creating and using local SSURGO databases via
    [`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)
    and
    [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
- [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
  now supports `addFields` argument to add columns to the result from
  other non-spatial tables.
  - You can now specify columns from:
    - `"legend"`, `"mapunit"`, and `"muaggatt"` tables for `what` equal
      to `"mupolygon"`, `"mupoint"`, `"muline"` or `"mukey"`  
    - `"legend"` table for `what` equal to `"areasymbol"` or
      `"sapolygon"`
    - `"featdesc"` for `what` equal to `"featpoint"` or `"featline"`
- [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md):
  - Added `append` argument to fix `overwrite` behavior
  - Fixed bug in creating non-GeoPackage, non-spatial SQLite output
  - Improved handling of empty export files and `overwrite` logic for
    non-file-based RDBMS
- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md):
  fixed bug when `fill=TRUE` and there are no horizons in the source
  data for any profile
- [`get_component_from_SDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
  fixed bug in join of `"chorizon"` and `"chfrags"` tables, now uses
  `merge(incomparables=NA)`
- [`get_vegplot_tree_si_summary_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
  updated for new `siteindexcurve` table in data model version 7.4.3

## soilDB 2.8.11 (2025-07-11)

CRAN release: 2025-07-11

- Update to NASIS metadata for data model version 7.4.3
- [`fetchSDA_spatial()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md)
  `geom.src="mlrapolygon"` uses a new URL on the DSHub S3 bucket
- Added `"projectsubtype"` and `"milestonetype"` to the `"project"` set
  for
  [`get_NASIS_table_name_by_purpose()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_name_by_purpose.md)
- Initial phase of syntax cleanup affecting many functions

## soilDB 2.8.10 (2025-04-28)

- [`fetchVegdata()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
  add `nullFragsAreZero` argument (passed to
  [`get_site_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md))
- Added
  [`soilDB_user_dir()`](http://ncss-tech.github.io/soilDB/reference/soilDB_user_dir.md)
  for caching local copies of data for use in soilDB functions
  ([\#377](https://github.com/ncss-tech/soilDB/issues/377))
- Added
  [`fetchHWSD()`](http://ncss-tech.github.io/soilDB/reference/fetchHWSD.md)
  and
  [`get_HWSD_path()`](http://ncss-tech.github.io/soilDB/reference/fetchHWSD.md)
  for the FAO Harmonized World Soil Database
  ([\#391](https://github.com/ncss-tech/soilDB/issues/391))
- Added
  [`get_phroots_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_phroots_from_NASIS_db.md)
  for getting pedon horizon root information from NASIS
  ([\#393](https://github.com/ncss-tech/soilDB/issues/393))

## soilDB 2.8.9 (2025-04-04)

CRAN release: 2025-04-05

- soilDB now requires aqp \>= 2.1.0
- Added
  [`get_vegplot_groundsurface_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
  thanks to Greg Schmidt ([@phytoclast](https://github.com/phytoclast);
  [\#373](https://github.com/ncss-tech/soilDB/issues/373))
- Added `"transectgroundsurfcover"` table to `"vegetation"` set for
  [`get_NASIS_table_name_by_purpose()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_name_by_purpose.md)
- Added missing `fetchNASIS("components")` deprecation messages for
  maat_l, maat_r, maat_h, mast_r, ecosite_id, ecosite_name, othervegid,
  and othervegclass columns
- `fetchSDA_spatial(geom.src="mlrapolygon")`: uses an AWS (S3) source
  and Seek-optimized ZIP file (thanks to
  [@jneme910](https://github.com/jneme910))
- [`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)
  (`method` `"weighted average"` and `"min/max"`),
  [`get_SDA_hydric()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_hydric.md),
  and
  [`get_SDA_interpretation()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_interpretation.md)
  (`method` `"weighted average"`) now use Common Table Expressions
  (CTEs) and therefore are compatible with local (SQLite or other) data
  sources

## soilDB 2.8.8 (2025-02-10)

CRAN release: 2025-02-10

- [`fetchSCAN()`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md):
  Bug fix when sensor column contains all `NA`
- [`ROSETTA()`](http://ncss-tech.github.io/soilDB/reference/ROSETTA.md):
  now uses HTTPS URL for API endpoint
- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md):
  Bug fix for (now deprecated) `pedon_id` in horizon slot rather than
  site
- Rebuilt SPC datasets: `loafercreek`, `gopheridge`, `mineralKing`
- [`fetchNASISLabData()`](http://ncss-tech.github.io/soilDB/reference/fetchNASISLabData.md):
  use `ncsspedonlabdataiid` as unique pedon ID

## soilDB 2.8.7 (2025-01-16)

CRAN release: 2025-01-16

- Several aliases of NASIS physical column names have been deprecated
  and will be removed in the next minor release (2.9.x). See
  <https://ncss-tech.github.io/AQP/soilDB/bulletins/2025.01-1-soilDB-NASIS-column-aliases.html>
  for details ([\#369](https://github.com/ncss-tech/soilDB/issues/369))
- [`fetchVegdata()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
  - Added `"ecostateid"`, `"ecostatename"`, `"commphaseid"`,
    `"commphasename`” to columns from `site` and `siteobs` are now
    joined into `"vegplot"` result.
  - Fixed `"site"` join used for `"vegplot"` table result. Now using
    LEFT join to add `siteecositehistory` information
  - Sites without vegetation plots are now excluded from the result
- [`get_vegplot_trhi_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
  &
  [`get_vegplot_transect_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
  no longer join to the `pedon` table. Use the `"siteobsiid"` or
  vegetation plot `"assocuserpedonid"` to join to pedon records when
  necessary.
  - This change avoids issues with unintended duplication of records
    e.g. lab pedons that have multiple pedons per site observation.
    Thanks to Nathan Roe for suggestion.
- `get_vegplot_*()` functions use INNER join to `vegtransect` table
  where applicable, so records are only returned for vegplots with an
  associated transect. Thanks to Zach Van Abbema for suggestion.
- [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md):
  more informative error message when no export files found for import
- Improved [soilDBdata](https://github.com/brownag/soilDBdata) data sets
  used for unit tests of
  [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  and
  [`fetchVegdata()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
  when a local NASIS instance is not available
- Updated NASIS SoilProfileCollection data sets (`loafercreek`,
  `gopheridge`, `mineralKing`)

## soilDB 2.8.6 (2024-12-23)

- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  and
  [`get_site_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md)
  now return Ecological Site State and Community Phase information
  (ecostatename, ecostateid, commphasename, commphaseid columns) from
  Site Observation table
- [`createStaticNASIS()`](http://ncss-tech.github.io/soilDB/reference/createStaticNASIS.md)
  bug fixes
  - Removed workaround for {odbc}/nanoodbc VARCHAR(MAX) columns; now can
    directly use
    [`DBI::dbReadTable()`](https://dbi.r-dbi.org/reference/dbReadTable.html)
    for all tables via NASIS ODBC connection
  - Fixed error when `output_path=NULL`
- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  changed default behavior to `mixColors=FALSE` which returns dominant
  condition for each moisture state rather than mixing LAB color
  coordinates
  - [`get_colors_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_colors_from_NASIS_db.md)
    deprecate `mixColors` argument, add `method` argument with options
    “dominant”, “mixed”, and “none”. New aggregation method `"none"`
    returns long format representation of color data from phcolor table
    with no aggregation applied.
- [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
  updates:
  - Added incremental write of tabular data by table and soil survey
    area, which is much more memory efficient
  - Added `maxruledepth` argument to allow filtering of `cointerp`
    table, and set default to `0`.
    - This reduces number of `cointerp` rows by about 75% for published
      SSURGO. Generally, Web Soil Survey exports have maximum rule depth
      of `1`, but custom NASIS exports can be “deeper”
  - Updated behavior of `filename` argument when `conn` *DBIConnection*
    is specified and improved `overwrite` logic
  - Added `dissolve_field` to facilitate creating aggregate geometries
    by `"mukey"` or other spatial attribute
  - Added `include_tabular` argument to support omitting tabular data
    when building a database
  - Now `include_spatial` and `include_tabular` are allowed to be a
    *character* vectors of table names
    - `TRUE` is all tables, `FALSE` is no tables. This allows for
      subsets of the SSURGO data model to be specified with finer user
      control over database contents.
  - Now a composite `"soil_metadata"` table is made with `"areasymbol"`
    column and one row per soil survey area, rather than one table per
    soil survey area. This is more compact and scales better to larger
    databases.

## soilDB 2.8.5 (2024-11-04)

CRAN release: 2024-11-05

- [`fetchLDM()`](http://ncss-tech.github.io/soilDB/reference/fetchLDM.md)
  add support for `area_type` argument with local database connections
  (`dsn` argument)
- [`fetchSCAN()`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md)
  updates:
  - Improved SCAN, CSCAN, SNOTEL, SNOWLITE station metadata
    ([\#61](https://github.com/ncss-tech/soilDB/issues/61)) via
    [@jskovlin](https://github.com/jskovlin)
  - Timezone support for hourly data requested by
    [`fetchSCAN()`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md)
    ([\#184](https://github.com/ncss-tech/soilDB/issues/184))
  - All above-ground sensors are now returned, instead of just the first
    of each type
    ([\#359](https://github.com/ncss-tech/soilDB/issues/359))
- Added new help file on NASIS database sources; see
  [`?NASISLocalDatabase`](http://ncss-tech.github.io/soilDB/reference/NASISLocalDatabase.md)
  ([\#360](https://github.com/ncss-tech/soilDB/issues/360))
- `get_SDA_*()` function updates related to consistent parameters for
  miscellaneous areas and minor components
  ([\#361](https://github.com/ncss-tech/soilDB/issues/361))
- [`fetchSOLUS()`](http://ncss-tech.github.io/soilDB/reference/fetchSOLUS.md):
  New function for downloading data from Soil Landscapes of the United
  States 100-meter (SOLUS100) soil property maps project repository
  ([\#362](https://github.com/ncss-tech/soilDB/issues/362))
- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  more consistent QC messages and output for multiple site observations
  and lab samples
- Updated SoilProfileCollection data sets (`loafercreek`, `gopheridge`,
  `mineralKing`) for aqp 2.1.x object definition and recent changes to
  [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  ([\#363](https://github.com/ncss-tech/soilDB/issues/363))

## soilDB 2.8.4 (2024-08-17)

CRAN release: 2024-08-17

- [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
  bug fixes following changes in 2.8.3
  - generating `gpkg_contents` for GeoPackage files failed to add
    entries for “features” data_type
  - `filename` argument not properly handled in some cases
- [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
  handle another possible failure point in error handling code
- [`createStaticNASIS()`](http://ncss-tech.github.io/soilDB/reference/createStaticNASIS.md)
  use `odbc::dbListFields()` instead of
  [`odbc::odbcConnectionColumns()`](https://odbc.r-dbi.org/reference/odbcConnectionColumns.html)

## soilDB 2.8.3 (2024-06-11)

CRAN release: 2024-06-11

- Updates to SoilWeb web coverage services
  - ISSR800 authoritative grid system altered slightly
  - [`ISSR800.wcs()`](http://ncss-tech.github.io/soilDB/reference/ISSR800.wcs.md)
    and
    [`soilColor.wcs()`](http://ncss-tech.github.io/soilDB/reference/soilColor.wcs.md)
    now set color table internally when possible
  - updated source data: ISSR800 (FY24) and soil color (FY23)
- [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
  updates
  - Better messaging on error
  - Handle try-error result more gracefully in high-level functions
    ([\#352](https://github.com/ncss-tech/soilDB/issues/352))
- [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md):
  added support for creating DuckDB, PostgreSQL, and other
  DBI-compatible databases
  ([\#352](https://github.com/ncss-tech/soilDB/issues/352)) via `conn`
  argument
- [`fetchSDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
  and
  [`get_chorizon_from_SDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
  concatenate multiple `texcl` using a comma when a texture group
  contains multiple texture classes (e.g. stratified textures) (fixes
  [\#353](https://github.com/ncss-tech/soilDB/issues/353))
- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)/`.formatLandformString()`:
  append `geomfmod` column to landscape, landform, and microfeature
  strings where present; thanks to Gabriel Benitez for suggestion
- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)/[`get_extended_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_extended_data_from_NASIS_db.md):
  fix conversion of codes to labels for microrelief
- [`fetchSCAN()`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md):
  fix header format; thanks to [@dschlaep](https://github.com/dschlaep)
  for reporting and providing a fix

## soilDB 2.8.2 (2024-04-22)

CRAN release: 2024-04-22

- SoilWeb Web Coverage Service MUKEY grid data source (used for
  [`mukey.wcs()`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md))
  and metadata have been updated for FY2024
  - Note that ISSR800 WCS
    ([`ISSR800.wcs()`](http://ncss-tech.github.io/soilDB/reference/ISSR800.wcs.md)
    source) is still using FY2023/FY2022 data
- [`get_SDA_coecoclass()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_coecoclass.md)
  default data returned for methods “Dominant Component”, “Dominant
  Condition” and “None” now include `localphase` column
- [`get_soilseries_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_soilseries_from_NASIS.md)
  and
  [`get_competing_soilseries_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_soilseries_from_NASIS.md):
  add `SS` argument for parity with all other NASIS “get” methods
  - default to `FALSE` for backward compatibility/common use cases
- [`get_site_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md)
  gains Frost Free Days, MAP, REAP, MAAT, MAST, MSAT, MSST, MWAT, MWST,
  and Parent Material Group Name; thanks to Zach Van Abbema for
  suggestion
- Changes in column names related to Area table / `"areasymbol"`
  ([\#272](https://github.com/ncss-tech/soilDB/issues/272)); thanks to
  Zach Van Abbema for suggestion
  - [`get_site_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md):
    Add state, county, and MLRA areasymbol references (`"site_state"`,
    `"site_county"`, `"site_mlra"`)
  - `get_mapunit_from_NASIS_db()`: Add dominant MLRA areasymbol
    reference column `"lmapunit_mlra"`
  - [`get_soilseries_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_soilseries_from_NASIS.md):
    replace `areasymbol` column to use relationship-style name
    `"soilseries_typelocst"` (minor breaking change)
- [`fetchSDA_spatial()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md):
  Add support for querying mapunit point (`"mupoint"`), mapunit line
  (`"muline"`), special feature point (`"featpoint"`), special feature
  line (`"featline"`) by `mukey` or `featkey`, geometry type selectable
  via `geom.src` argument
- [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md):
  Add support for querying mapunit point (`"mupoint"`), mapunit line
  (`"muline"`), special feature point (`"featpoint"`), special feature
  line (`"featline"`) for a spatial extent, geometry type selectable via
  `what` argument
- [`simplifyFragmentData()`](http://ncss-tech.github.io/soilDB/reference/simplifyFragmentData.md)
  /
  [`simplifyArtifactData()`](http://ncss-tech.github.io/soilDB/reference/simplifyFragmentData.md)
  efficiency improvement when all records are missing data

## soilDB 2.8.1 (2024-01-09)

CRAN release: 2024-01-09

- [`get_mapunit_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_mapunit_from_NASIS.md),
  [`get_lmuaoverlap_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_mapunit_from_NASIS.md)
  and
  [`get_legend_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_mapunit_from_NASIS.md)
  gain `areatypename` argument used for filtering legends by
  `areatypename`.
  - Default results include `"Non-MLRA Soil Survey Area"` and
    `"MLRA Soil Survey Area"`. Set to `NULL` for no filter.
- Fixed bugs in
  [`waterDayYear()`](http://ncss-tech.github.io/soilDB/reference/waterDayYear.md)
  and
  [`summarizeSoilTemperature()`](http://ncss-tech.github.io/soilDB/reference/fetchHenry.md)
  for leap years
  ([\#333](https://github.com/ncss-tech/soilDB/issues/333))
- [`fetchSoilGrids()`](http://ncss-tech.github.io/soilDB/reference/fetchSoilGrids.md)
  upgrades
  - Fixed a bug with `data.frame` output
  - Requests for more than 5 sites now include a call to
    [`Sys.sleep()`](https://rdrr.io/r/base/Sys.sleep.html) to conform
    with ISRIC “Fair Use” policy
  - Upgraded SoilProfileCollection spatial promotion for aqp 2.0+
  - Added 10 kPa, 33 kPa and 1500 kPa water content estimates to default
    variable sets for point and grid queries
- [`fetchSDA_spatial()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md)
  gains ability to query mapunit delineations by Ecological Site ID
  (`by.col="ecoclassid"`)
- [`get_SDA_coecoclass()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_coecoclass.md)
  default `ecoclasstypename` is now
  `c("NRCS Rangeland Site", "NRCS Forestland Site")`, as this is the
  most common type of aggregation and is least prone to producing
  unusual composition-related errors due to duplications
  - Fixed bug related to merging tables/integer data type
  - Fixed bug in calculation of “Not assigned” fraction of mapunits
    which could result in negative aggregate component percentages below
    the default threshold

## soilDB 2.8.0 (2023-12-22)

CRAN release: 2023-12-22

- Minimum {aqp} version set to v2.0.2. This is due to changes in the
  namespace related to
  [`aqp::col2Munsell()`](https://ncss-tech.github.io/aqp/reference/col2Munsell.html),
  to “encourage” users to update to the more efficient routines provided
  in {aqp} 2+ (if they haven’t already), and prepare for future updates
  in the 2.x series.
- Fix bugs in
  [`get_SDA_interpretation()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_interpretation.md)
  when `dsn` refers to a local SQLite source and in concatenation of
  reason string when `wide_reason=TRUE`
- [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
  updates:
  - *Breaking change*: Now uses the standard SSURGO layer names for
    spatial data (e.g. `"mupolygon"` rather than `"soilmu_a"`).
  - Creates indices for foreign keys and other columns important for
    data analysis, dramatically improving the performance of standard
    queries on SQLite sources.
  - Now works properly on STATSGO data sets for individual states or
    CONUS. Previously tabular data would be transferred but spatial data
    were not.
- [`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)
  gains `db` argument which gives ability to download STATSGO by state
  or all of US from Web Soil Survey. Thanks to Meghan Krueger for
  suggestion.
- [`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md):
  weighted average/dominant component numeric methods now return `mukey`
  in first column position; for parity with other `get_SDA*` methods
  recently updated/already doing this, making it easier to use these
  columns for raster attribute tables via `terra::set.levels()`
- `fetchNASIS(lab=TRUE)` fixed a bug when multiple `phlabresults`
  records are present for a single horizon, but one or both is missing
  bottom depth. Thanks to Meyer Bohn
  ([@MollicMeyer](https://github.com/MollicMeyer)) for reporting.
- SoilProfileCollection objects now include a time stamp in their
  metadata, accessed as: `metadata(x)$created`
  ([\#235](https://github.com/ncss-tech/soilDB/issues/235))
- Added `area_type` argument to
  [`fetchLDM()`](http://ncss-tech.github.io/soilDB/reference/fetchLDM.md)
  for non-SSA area queries
  ([\#328](https://github.com/ncss-tech/soilDB/issues/328))
- Added `grid` argument to
  [`fetchSoilGrids()`](http://ncss-tech.github.io/soilDB/reference/fetchSoilGrids.md)
  for downloading Cloud-Optimized GeoTIFF subsets for spatial extent
  ([\#329](https://github.com/ncss-tech/soilDB/issues/329))
- [`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)
  now automatically batches requests into sets of 100 series, allowing
  arbitrarily large requests to be made
  ([\#239](https://github.com/ncss-tech/soilDB/issues/239))

## soilDB 2.7.10 (2023-11-16)

CRAN release: 2023-11-16

- [`fetchSDA_spatial()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md)
  gains `geom.src="mlrapolygon"` for obtaining Major Land Resource Area
  (MLRA) polygon boundaries. When using this geometry source `x` is a
  vector of `MLRARSYM` (MLRA Symbols).

  - The geometry source is the MLRA Geographic Database v5.2 (2022)
    which is not (yet) part of Soil Data Access. Instead of SDA, GDAL
    utilities are used to read a zipped ESRI Shapefile from a remote
    URL:
    <https://www.nrcs.usda.gov/sites/default/files/2022-10/MLRA_52_2022.zip>.
    Therefore, most additional
    [`fetchSDA_spatial()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md)
    arguments are *not* currently supported for the MLRA geometry
    source. In the future a `mlrapolygon` table may be added to SDA
    (analogous to `mupolygon` and `sapolygon`), and the function will be
    updated accordingly at that time.

- Web coverage services and related raster attribute tables provided by
  SoilWeb
  ([`mukey.wcs()`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md)
  etc.) are now using the SoilWeb load-balancer URL

- [`get_SDA_coecoclass()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_coecoclass.md)
  gains `method="all"` for aggregating information about ecological
  sites and related components. The method performs a condition-based
  aggregation for each ecological site condition in the map unit,
  producing a “wide” data.frame result with as many columns as needed to
  portray all site conditions.

- [`fetchLDM()`](http://ncss-tech.github.io/soilDB/reference/fetchLDM.md)
  gains new argument `WHERE` for supplying a custom SQL where clause for
  selecting sites of interest. For example:
  `fetchLDM(WHERE = "CASE WHEN corr_name IS NOT NULL THEN LOWER(corr_name) ELSE LOWER(samp_name) END = 'musick'")`

## soilDB 2.7.9 (2023-09-01)

- Added new `method` options for
  [`fetchSDA_spatial()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md).
  Aggregation grouping is controlled by the `by.col` argument. This
  works for mapunit and survey area polygon geometries, aggregating all
  polygons in the group for each `mukey`, `nationalmusym`, `lkey`, or
  `areasymbol` extent.
  - `method="extent"` method calculates a bounding rectangle  
  - `method="convexhull"` calculates the convex hull
  - `method="union"` returns a MULTIPOLYGON
  - `method="collection"` returns a GEOMETRYCOLLECTION

### Bug Fixes

- Bug fix for
  [`get_vegplot_transpoints_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md);
  using wrong record ID for transect points

- [`get_NASIS_table_name_by_purpose()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_name_by_purpose.md)
  now includes `"pointplantcoverdetails"`, `"plantprodquadratdetails"`
  in the `"vegetation"` table purpose group

## soilDB 2.7.8 (2023-08-29)

CRAN release: 2023-08-29

- Added new SoilWeb “Web Coverage Service” vignette
  (<https://ncss-tech.github.io/soilDB/articles/wcs-ssurgo.html>)
  related to creating thematic maps using grids of mapunit keys from
  SoilWeb with aggregated tabular data from Soil Data Access.

- `get_SDA_*()` functions now all return `mukey` values in first column

- [`mukey.wcs()`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md)
  gains a 30m gridded version of Hawaii and Puerto Rico SSURGO data via
  `db="HI_SSURGO"` and `db="PR_SSURGO"`, respectively.

- `get_vegplot_data_from_NASIS_db()`: now uses LEFT join to plant table
  so that empty records can be diagnosed; thanks to
  [@natearoe](https://github.com/natearoe) for suggestion

- Added new NASIS query functions for vegetation transect point and
  quadrat details:
  [`get_vegplot_prodquadrats_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
  and
  [`get_vegplot_transpoints_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)

### Bug Fixes

- `get_SDA_property(method="Dominant Component (Numeric)")` now includes
  minors by default, fixing issues with STATSGO mapunits that have no
  major components flagged.
  ([@dylanbeaudette](https://github.com/dylanbeaudette))

- [`get_component_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_component_data_from_NASIS_db.md):
  removed duplicated `hydricrating` column, also now includes climate
  columns with standard names. This is a continuation of the idea that
  custom aliases for standard NASIS columns are deprecated
  (<https://github.com/ncss-tech/soilDB/issues/242>)

- `.pickBestEcosite()` now considers `record_when_last_updated` in lieu
  of correlation date; this stabilizes sort order from SQLite snapshot
  v.s. NASIS local database sources
  (<https://github.com/ncss-tech/soilDB/issues/295>)

- [`get_component_from_GDB()`](http://ncss-tech.github.io/soilDB/reference/fetchGDB.md):
  fixes for batching over component geomorphic description and parent
  material group

- [`get_mapunit_from_SDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md):
  handle possibility of mapunit-level SDA query failure

## soilDB 2.7.7 (2023-03-10)

CRAN release: 2023-03-10

### Enhancements

- `fetchNASIS(from="pedons")` and
  [`get_site_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md)
  now return `siteobsiid` column
  ([@natearoe](https://github.com/natearoe))

- [`mukey.wcs()`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md)
  gains experimental gridded STATSGO layer (300m)

- Add
  [`get_NASIS_table_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_metadata.md)
  for returning information about columns in NASIS tables.

- `simplifyFragment/ArtifactData()`: downgrade warnings

- [`get_cosoilmoist_from_SDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md):
  `mukey` and `cokey` now included when `duplicates=TRUE`; thanks to
  [@andypaolucci](https://github.com/andypaolucci) for catching this

- [`fetchSoilGrids()`](http://ncss-tech.github.io/soilDB/reference/fetchSoilGrids.md)

  - Add `SpatVector` coercion for input locations (already supported
    sf/sp classes)

  - Add handling for messages from API about erroneous input

- Add `fetchNASIS("pedons")` columns vignette

- Add
  [`get_NASIS_table_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_metadata.md)
  and optional argument `include_description` for
  [`get_NASIS_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_metadata.md)
  and
  [`get_NASIS_column_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_metadata.md)

- [`get_SDA_cosurfmorph()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_cosurfmorph.md) +
  [`get_SDA_pmgroupname()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_pmgroupname.md):
  support for including or excluding misc. areas via
  `miscellaneous_areas` argument

- [`get_site_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md):

  - Omit “multiple horizontal datums” warning

  - Remove calculated X/Y long/lat + add proper NASIS alias for long/lat

  - Add `siteothvegclass` id/name - add tables to default lookup/site
    sets

### Bug fixes

- `dbQueryNASIS(..., close=TRUE)` now calls
  [`dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html) (ensuring
  connections get closed on error)

- [`fetchSCAN()`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md):

  - 2x requests when `timeseries` argument not specified

  - Account for no results (e.g. year = 1800)

  - Bug fix in vectorization when daily + hourly data requested

- [`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md):

  - Handle
    [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
    with invalid syntax in `WHERE` clause

  - Fix for `include_template=FALSE`

- [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)

  - Attribute tables are now added to `gpkg_contents` when output
    `filename` is a GeoPackage

- Fix for `.get_comonth_from_SDA()`

- Fix selected set (`SS` argument) for
  [`get_ecosite_history_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_ecosite_history_from_NASIS_db.md)
  in
  [`get_site_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_site_data_from_NASIS_db.md)

- `get_SDA_pmgroupname`: return `NA` (not “NULL”) for empty/missing
  `pmgroupname` when `simplify=TRUE`

- [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md):

  - `geomIntersection=TRUE` with `db="SAPOLYGON"` now correctly labels
    the `areasymbol` column (previously was `mukey`) to match
    `geomIntersection=FALSE` and SDA schema

  - Bounding box extent polygons are now only calculated for
    `SpatRaster` (not `SpatVector`) input

## soilDB 2.7.6 (2022-11-28)

CRAN release: 2022-12-02

- All references to `soilDB.env` have been replaced with a function that
  returns that environment object
  ([`get_soilDB_env()`](http://ncss-tech.github.io/soilDB/reference/get_soilDB_env.md));
  thanks to [@MollicMeyer](https://github.com/MollicMeyer) for
  identifying this as a problem in
  [\#277](https://github.com/ncss-tech/soilDB/issues/277). This object
  used to be exported but was unintentionally omitted from NAMESPACE;
  this has been fixed.

- `fetchNASIS(lab=TRUE)` bug fix related to many:1 relationships between
  lab samples and morphologic horizons with all-missing columns having
  *logical* datatype
  ([\#277](https://github.com/ncss-tech/soilDB/issues/277))

- Added
  [`get_SRI()`](http://ncss-tech.github.io/soilDB/reference/get_SRI.md)
  and
  [`fetchSRI()`](http://ncss-tech.github.io/soilDB/reference/fetchSRI.md)
  functions for accessing USFS Region 6 Soil Resource Inventory
  information from <https://ecoshare.info/>; thanks to
  [@joshualerickson](https://github.com/joshualerickson) for the
  contribution (<https://github.com/ncss-tech/soilDB/pull/274>)

- [`fetchLDM()`](http://ncss-tech.github.io/soilDB/reference/fetchLDM.md)
  now sets the horizon designation metadata like other fetch\* functions
  that return SoilProfileCollection objects
  ([@dylanbeaudette](https://github.com/dylanbeaudette))

- [`mukey.wcs()`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md)
  fix spurious warnings occasionally given due to minor differences
  (rounding) of grid dimensions

- {curl} has been moved from Suggests to Imports

- `simplfyFragmentData()` alias for
  [`simplifyFragmentData()`](http://ncss-tech.github.io/soilDB/reference/simplifyFragmentData.md)
  removed ([@infotroph](https://github.com/infotroph))

## soilDB 2.7.5 (2022-10-17)

CRAN release: 2022-10-18

- Updates to SSURGO File Geodatabase functions
  ([`fetchGDB()`](http://ncss-tech.github.io/soilDB/reference/fetchGDB.md)
  and related) by [@smroecker](https://github.com/smroecker)

- Added
  [`soilColor.wcs()`](http://ncss-tech.github.io/soilDB/reference/soilColor.wcs.md)
  to access a web coverage service for soil color at various depths by
  [@dylanbeaudette](https://github.com/dylanbeaudette)

- [`waterDayYear()`](http://ncss-tech.github.io/soilDB/reference/waterDayYear.md):
  fix for CRAN and different timezones; now defaults to `tz="UTC"`
  [\#268](https://github.com/ncss-tech/soilDB/issues/268)

  - [`summarizeSoilTemperature()`](http://ncss-tech.github.io/soilDB/reference/fetchHenry.md):
    set default timezone to `tz="UTC"`

- Fix for
  [`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md) on
  pre-decoded values when ChoiceName and ChoiceLabel overlap
  [\#273](https://github.com/ncss-tech/soilDB/issues/273)

  - Fix for
    [`NASISChoiceList()`](http://ncss-tech.github.io/soilDB/reference/NASISChoiceList.md)
    related to [\#273](https://github.com/ncss-tech/soilDB/issues/273)

- [`get_soilseries_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_soilseries_from_NASIS.md):
  Remove `areaacres` and `obterm` for
  [\#272](https://github.com/ncss-tech/soilDB/issues/272) by
  [@smroecker](https://github.com/smroecker)

- Fix for `get_OSD(..., fix_ocr_errors = TRUE)` with empty typical
  profile for [\#271](https://github.com/ncss-tech/soilDB/issues/271) by
  [@dylanbeaudette](https://github.com/dylanbeaudette)

- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  drops non-representative and additional when
  `dropNotRepresentative`/`dropAdditional` are set
  ([@natearoe](https://github.com/natearoe))

- Fix for [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) in R
  4.3+; for [\#265](https://github.com/ncss-tech/soilDB/issues/265)

- Fix for “status was ’SSL connect error” for
  [`fetchKSSL()`](http://ncss-tech.github.io/soilDB/reference/fetchKSSL.md)
  and other functions that download JSON
  ([@kramdog](https://github.com/kramdog))

  - Now uses standard soilDB {curl} handle, which includes a longer
    timeout and `ssl_verifyhost=0`

- Fixes for compatibility with {jsonlite} \>1.8.1 that now uses {base}
  rather than {curl}

- Update row count expectations for end of FY22 SSURGO refresh

## soilDB 2.7.4 (2022-09-30)

CRAN release: 2022-09-30

- [`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
  replaced `\(x)` syntax inadvertently included in a recent update to
  fix R \< 4.1 compatibility; thanks to
  [@cbrueffer](https://github.com/cbrueffer) for catching this
  (<https://github.com/ncss-tech/soilDB/issues/262>)!

- [`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)
  fixes for weighted average method
  (<https://github.com/ncss-tech/soilDB/issues/229>,
  <https://github.com/ncss-tech/soilDB/issues/261>)

- [`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)
  now automatically encodes horizon distinctness codes to default depth
  offsets, stored in hz-level attr: `hzd`

## soilDB 2.7.3 (2022-08-19)

CRAN release: 2022-08-19

- [`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)
  all methods now support `miscellaneous_areas` argument. This defaults
  to `FALSE` for the methods it was previously implemented for–so be
  aware that queries using `"Dominant Component"` or
  `"Dominant Condition"` (which previously did not respond to
  `miscellaneous_areas`) may have the number of rows in result reduced
  due to omission of miscellaneous land types. If this is unexpected or
  undesired, please use `miscellaneous_areas=TRUE`.
  (<https://github.com/ncss-tech/soilDB/issues/257>)

- Adds
  [`get_NASIS_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_metadata.md)
  and helper method
  [`get_NASIS_column_metadata()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_metadata.md)
  and other new tools for working with
  [`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md),
  factors and NASIS metadata cached in the package.

- Bug fix for
  [`get_cosoilmoist_from_SDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md);
  thanks to Monika Shea [@monikashea](https://github.com/monikashea) for
  reporting the problem
  (<https://github.com/ncss-tech/soilDB/issues/253>)

- `fetchNASIS_report()` now works with the output from
  `"get_site_from_NASIS"` report (useful for site records without
  associated pedons)

- [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
  gains arguments `quiet` and `include_spatial` to toggle messages and
  inclusion of spatial data in created SQLite database, respectively

- [`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)
  now handles queries against `sacatalog` that return no results more
  gracefully

- [`get_OSD()`](http://ncss-tech.github.io/soilDB/reference/get_OSD.md):
  Add `fix_ocr_errors` argument for `result='json'` to fix common
  optical character recognition errors associated with horizon
  designations and colors (NOTE: does NOT fix depths)

- [`fetchSCAN()`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md)
  gains `timeseries` argument to support batch downloads of hourly data.
  Additional arguments (`...`) are passed as a raw request, allowing
  other parameters sent in request to be modified if needed, and
  bypassing batch functionality. This is similar to the `req` argument
  syntax used in earlier versions of this function, which had been
  deprecated for some time now. Thanks to Matthew Morriss for raising
  the discussion item [@morrismc99](https://github.com/morrismc99)
  (<https://github.com/ncss-tech/soilDB/discussions/260>).

- {curl} moved from Suggests to Imports;
  [`curl::curl_download()`](https://jeroen.r-universe.dev/curl/reference/curl_download.html)
  is now used instead of
  [`utils::download.file()`](https://rdrr.io/r/utils/download.file.html)
  because it seems to handle SSL certificates better on some networks.

## soilDB 2.7.2 (2022-06-24)

CRAN release: 2022-06-27

- `fetchNASIS("components')`: Fix and refactor ( **breaking change**
  from 2.7.1, which introduced a bug/inconsistency) of recent change;
  `duplicates` argument is now *required* to merge in data from
  mapunit/legend tables (where many:1 relationships between
  legend/mapunit and datamapunit are possible). In 2.7.1 possibly
  incomplete mapunit/legend tables could be joined to
  SoilProfileCollection result (if and only if the tables were populated
  in selected set/local DB). Does not change historic (\<=2.7.0) default
  behavior. Thanks to
  [@dylanbeaudette](https://github.com/dylanbeaudette) for suggesting
  use of
  [`get_component_correlation_data_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_component_data_from_NASIS_db.md)
  here.

- Several fixes for Roxygen documentation (notably for
  [`ROSETTA()`](http://ncss-tech.github.io/soilDB/reference/ROSETTA.md)
  and various NASIS web report related functions) that were missing
  `@export` tags. Several previously-exported functions missed being
  explicitly exported in the new Roxygen-generate NAMESPACE. These
  unintentional omissions from 2.7.1 NAMESPACE have been resolved.

- Fixes an old bug in
  [`fetchSDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)/[`get_chorizon_from_SDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
  related to LEFT OUTER versus RIGHT JOIN to `chtexture` table

## soilDB 2.7.1 (2022-06-10)

CRAN release: 2022-06-10

- [`get_SDA_coecoclass()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_coecoclass.md)
  better handling of `NULL` `ecoclassref`; support for filtering on
  `ecoclasstypename`; `not_assigned_value` now applies to
  `ecoclassname`, `ecoclasstypename` and `ecoclassref` columns in
  addition to `ecoclassid`; Thanks to Andy Paolucci and Jason Nemecek.
  Also, added additional columns from legend/mapunit tables
  (`areasymbol`, `lkey`, `muname`).
- `fetchNASIS(from="components")` now returns mapunit and legend
  information (if loaded in local NASIS database); results now contain
  `mustatus` and `repdmu` which can be used to remove components from
  additional mapunits and non-representative data mapunits; thanks to
  Nathan Roe
- Convert UTF-8 strings in `us_ss_timeline` result to ASCII
- [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md):
  Fixed POST request that could produce unhandled errors on network
  failure (now returns `try-error` like other error-causing code)
- [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md):
  In the event of a syntax error, error contents are now returned as a
  message, not a warning.
- [`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
  and [`code()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
  now work with `data.table` and `tibble` objects.
- Various fixes to tests and error handling code to reduce likelihood of
  thrown errors from network problems or missing suggested packages.

## soilDB 2.7.0 (2022-05-18)

CRAN release: 2022-05-18

- Spatial
  - Dropped imports from `sp` and `raster` which means `soilDB` no
    longer requires the soon-to-retire `rgdal` package.
  - All spatial data processing has been moved to `sf` and `terra`,
    which have been added to Suggests.
    - (*breaking change*) Classes from these packages (e.g. `sf`,
      `SpatRaster`) will be returned by default rather than
      `Spatial*DataFrame` or `RasterLayer` from `sp` and `raster`.
    - (*breaking change*) Spatial functions that take spatial input will
      return the same class type as the input unless otherwise
      specified.
      - [`fetchSDA_spatial()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA_spatial.md)
        and
        [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
        gain `as_Spatial` argument; when `TRUE` equivalent (backward
        compatible) `sp` and `raster` data types are returned.
      - Alternately, you may set `options(soilDB.return_Spatial=TRUE)`
  - [`mukey.wcs()`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md)
    and
    [`ISSR800.wcs()`](http://ncss-tech.github.io/soilDB/reference/ISSR800.wcs.md)
    bug fix for some instances where the target extent was
    miscalculated, resulting in slight differences from requested
    resolution (`res` argument) of result.
  - [`mukey.wcs()`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md)
    gains a new possible data source `db="RSS"` which accesses a Web
    Coverage Service containing grids from Raster Soil Surveys in the
    United States.
  - See: <https://github.com/ncss-tech/soilDB/pull/229> for more details
    on what has changed.
- NASIS
  - (*breaking change*) `rmHzErrors` argument now defaults to `FALSE`.
    Please let us know if allowing horizon errors results in unexpected
    behavior so we can repair various interfaces that rely on no errors
    being present. See
    [`aqp::checkHzDepthLogic()`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.html)
    for more information on the types of things that used to result in
    profiles being removed from results and for guidelines on how to
    fix.
  - Added several new columns from the `plotplantinventory` table to
    [`get_vegplot_species_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
    result; thanks to Zachary Van Abbema for suggestion and feedback
  - Local NASIS metadata used for
    [`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
    are now cached within an R session which results in faster query
    processing times for users with a local NASIS database connection
    set up.
    - The `db` argument to
      [`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
      has been deprecated. Now,
      [`code()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
      and
      [`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
      can use `ChoiceName` and/or `ChoiceLabel` in coding and decoding
      `ChoiceValue` for all data sources.
  - Rock fragment and artifact sieving now uses \<76mm as the upper
    boundary for gravel fraction, and a related QC message has been
    removed
- SSURGO / Soil Data Access
  - Added
    [`get_SDA_cosurfmorph()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_cosurfmorph.md):
    a new function in “SSURGO On Demand” style. Users can customize the
    WHERE clause, target tables and the grouping variable used to
    calculate proportions (default `by="compname"`)
    - “cosurfmorphgc” summarizes “geomposmntn”, “geomposhill”,
      “geomposflats”, “geompostrce”
    - “cosurfmorphhpp” summarizes “hillslopeprof”
    - “cosurfmorphss” summarizes “shapeacross”, “shapedown”, and
      concatenated “surfaceshape”
  - Several fixes for logic of
    [`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)
    with `method="weighted average"` and
    `include_minors=TRUE`/`miscellaneous_areas=TRUE`, thanks to Andy
    Paolucci and Dylan Beaudette for testing and providing feedback on
    the queries.
  - All `get_SDA_*()` methods (except
    [`get_SDA_metrics()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_metrics.md))
    now support input of custom `WHERE` clause in lieu of
    `mukeys`/`areasymbols` arguments and gain a `dsn` argument for
    specifying a local SQLite database or DBIConnection.
  - Added
    [`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md)
    for downloading/extraction of the SSURGO data by survey area from
    Web Soil Survey.
  - Added
    [`createSSURGO()`](http://ncss-tech.github.io/soilDB/reference/createSSURGO.md)
    for building of local databases as SQLite/Geopackage from one or
    more SSURGO exports.
    - Exports can be obtained via
      [`downloadSSURGO()`](http://ncss-tech.github.io/soilDB/reference/downloadSSURGO.md),
      from NASIS or downloaded from other sources such as
      <https://datagateway.nrcs.usda.gov/GDGHome_DirectDownLoad.aspx>.

## soilDB 2.6.15 (2022-04-13)

CRAN release: 2022-04-14

- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  - New `mixColors` argument used to toggle color mixing for pedon
    horizons with multiple colors per moisture state.
    - Default `TRUE` mixes colors based on reported color percentage via
      [`estimateColorMixture()`](http://ncss-tech.github.io/soilDB/reference/estimateColorMixture.md).
      `FALSE` returns the dominant color in each moisture state, or
      first record for cases without `colorpct` populated.
  - Added `taxreaction` and `taxfamhahatmatcl` to extended taxonomic
    information
  - Added new option (`soilDB.NASIS.skip_uncode`) to bypass
    [`uncode()`](http://ncss-tech.github.io/soilDB/reference/uncode.md)
    in NASIS functions (mostly used for debugging or special database
    instances that come pre-decoded)
  - An instance in `fetchNASIS(from="components")` where local database
    connection was left open was fixed
- Added
  [`fetchLDM()`](http://ncss-tech.github.io/soilDB/reference/fetchLDM.md)
  function to access Kellogg Soil Survey Lab Data Mart via Soil Data
  Access or local SQLite snapshot
  (<https://github.com/ncss-tech/soilDB/pull/243>)
- Added
  [`get_SDA_metrics()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_metrics.md)
  for accessing usage metrics for Soil Data Access, Web Soil Survey and
  the Lab Data Mart. Thanks to Jason Nemecek for submitting the base SQL
  query
- `get_SDA_interpretation(method = "dominant condition")` now includes
  `total_comppct_*` columns reflecting the summed component percentage
  of the dominant condition
- Updated metadata for SCAN sites that were missing `pedlabsampnum`
  (KSSL Pedon Sample ID)

## soilDB 2.6.14 (2022-03-15)

CRAN release: 2022-03-15

- `waterYearDay()` bug fix for more specific (sub-daily) date-time
  formats
- [`fetchSCAN()`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md)
  better handling of empty results in multi-site queries
- Updates to queries, metadata and sample datasets for NASIS 7.4.1
  Database Model
  - `fetchNASIS("pedons")` no longer includes `pmweathering`
  - `fetchNASIS("components")` no longer includes `ecositeorigin`
  - New Component Ecological Site attributes added
- `stringsAsFactors` argument to soilDB functions has been deprecated in
  favor of a package option
  - Default behavior for NASIS domains is to return character labels
    rather than coded factors
  - Use `NASISDomainsAsFactor(TRUE)` to toggle package option
    `soilDB.NASIS.DomainsAsFactor`

## soilDB 2.6.13 (2022-01-29)

CRAN release: 2022-01-31

- `fetchSDA_spatial` now supports `by.col` `"areaname"`, `"mlraoffice"`,
  and `"mouagencyresp"`; thanks to suggestion by Jay Skovlin
- `fetchNASIS` fix for multiple site observation records with surface
  fragments; thanks to bug report from Brianna Wegner
- `waterYearDay()` use format and timezone for start date conversion
- `fetchNASIS` fix for `from="components", SS=FALSE` and queries where
  no NASIS local database is present (e.g. from SQLite source)
- Update to `NASIS_table_column_keys` dataset of NASIS primary and
  foreign keys by table name to include many more tables
- [`fetchVegdata()`](http://ncss-tech.github.io/soilDB/reference/fetchVegdata.md)
  no longer errors for child table queries unless site/site
  observation/vegetation plot records are missing
- `fetchOSD(extended=TRUE)` now returns an element `"NCCPI"` containing
  the National Commodity Crop Production Index summaries. The values
  returned are quantiles at the 1, 5, 25, 50, 75, 95, and 99% levels
  over all SSURGO components with `compname` matching `series` for
  irrigated and non-irrigated condition.

## soilDB 2.6.12 (2022-01-07)

CRAN release: 2022-01-08

- [`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md)
  now works with mixed component and horizon-level properties; thanks to
  Matthieu Stigler for the bug report
- Added
  [`get_SDV_legend_elements()`](http://ncss-tech.github.io/soilDB/reference/get_SDV_legend_elements.md)
  for fetching and parsing XML for Soil Data Viewer / Web Soil
  Survey-style symbol themes for soil interpretations from Soil Data
  Access
- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  pedon and component geomorphic summaries now include columns for
  landscape, microfeature, microrelief, 2D/3D morphometry, and slope
  shape
- `fetchNASIS('pedons')` now uses
  [`simplifyFragmentData()`](http://ncss-tech.github.io/soilDB/reference/simplifyFragmentData.md)
  for surface fragments

## soilDB 2.6.11 (2021-12-21)

- [`fetchSDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
  handle NULL component-level results with an informative error
- [`fetchSDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
  now (again) returns mapunit/legend-level information via
  [`get_mapunit_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_mapunit_from_NASIS.md)
- [`createStaticNASIS()`](http://ncss-tech.github.io/soilDB/reference/createStaticNASIS.md)
  and queries to SQLite NASIS snapshots now preserve date/time using
  RSQLite 2.2.4+ via `extended_types=TRUE` argument to
  [`dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
- [`get_soilseries_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_soilseries_from_NASIS.md)
  now returns `soiltaxclasslastupdated` as a date/time and stores the
  year in calculated column `soiltaxclassyearlastupdated`
- Added several tables to thematic groups in
  [`get_NASIS_table_name_by_purpose()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_name_by_purpose.md)
- [`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)
  gains surface shape proportions in extended `shape_across` and
  `shape_down` tables
- [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
  `what='mukey'` now works with `db='STATSGO'`
- Added metadata to `loafercreek`, `gopheridge` and `mineralKing`
- `waterYearDay()` pass through `format` argument for POSIX time
  conversion

## soilDB 2.6.10 (2021-12-14)

CRAN release: 2021-12-15

- [`waterDayYear()`](http://ncss-tech.github.io/soilDB/reference/waterDayYear.md)
  and `.formatDates()` allow optional `format` and `tz` argument; used
  for consistent POSIX time conversion in tests where date/time has
  granularity finer than one day
- [`fetchSDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md)
  extensions for better handling of components with no horizon data
- [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
  and
  [`processSDA_WKT()`](http://ncss-tech.github.io/soilDB/reference/processSDA_WKT.md)
  fully use {sf}, replacing {sp} in these contexts
- [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
  gains argument `byFeature` to use multiple single-feature queries and
  combine the results with a unique feature ID specified by `idcol`
  argument. This allows for specific feature intersection results
  without secondary spatial overlay of the polygons
  (<https://github.com/ncss-tech/soilDB/issues/222>)
- [`dbConnectNASIS()`](http://ncss-tech.github.io/soilDB/reference/dbConnectNASIS.md)
  no longer requires that the NASIS credentials option be set if the
  `dsn` argument is specified.
- Rebuilt `loafercreek`, `gopheridge` and `mineralKing` from latest
  [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  results.

## soilDB 2.6.9 (2021-12-02)

CRAN release: 2021-12-03

- Replaced functionality using {plyr}/{reshape2} with
  {base}/{data.table}
- [`processSDA_WKT()`](http://ncss-tech.github.io/soilDB/reference/processSDA_WKT.md):
  replaced {rgeos} with {wk} and {sf}
- [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md):
  added `query_string` argument
- [`get_SDA_property()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_property.md):
  fixes for MIN/MAX aggregation
  (<https://github.com/ncss-tech/soilDB/issues/219>)
- [`get_component_from_SDA()`](http://ncss-tech.github.io/soilDB/reference/fetchSDA.md):
  fragment size thresholds now conform with new (clarified) NSSH
  definitions

## soilDB 2.6.8 (2021-11-05)

- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  upgrades: use data.table and base internally for data aggregation
- `fetchNASIS("components")` now “sieves” rock fragments and human
  artifacts using same routines as for pedons; respects
  `nullFragsAreZero` argument
- `fetchKSSL(..., returnGeochemicalData = TRUE)` safely returns 0-length
  `data.frame` when no data available

## soilDB 2.6.7 (2021-10-27)

- Removed several packages from Suggests: {gridExtra}, {ggplot2},
  {viridisLite}, {mapview}, {rasterVis}
- [`get_OSD()`](http://ncss-tech.github.io/soilDB/reference/get_OSD.md):
  Handle “NA” and type conversion for JSON results an convert spaces to
  underscores for file names as needed
- [`fetchSCAN()`](http://ncss-tech.github.io/soilDB/reference/fetchSCAN.md):
  graceful handling of timeout and converted from {plyr} to
  {data.table}; (<https://github.com/ncss-tech/soilDB/issues/161>,
  <https://github.com/ncss-tech/soilDB/issues/184>)
- [`get_EDIT_ecoclass_by_geoUnit()`](http://ncss-tech.github.io/soilDB/reference/get_EDIT_ecoclass_by_geoUnit.md):
  graceful handling of timeout
- [`get_SDA_muaggatt()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_muaggatt.md):
  add `query_string` argument for parity with other “SSURGO on demand” /
  `get_SDA_*()` functions

## soilDB 2.6.6 (2021-09-24)

- [`get_SDA_pmgroupname()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_pmgroupname.md)
  and
  [`get_SDA_hydric()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_hydric.md)
  now support alternate aggregation methods.
  - Default for
    [`get_SDA_pmgroupname()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_pmgroupname.md)
    is `"dominant component"`, now also supports `"dominant condition"`
    and `"none"`.
  - Default for
    [`get_SDA_hydric()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_hydric.md)
    is a map unit level aggregation of components and assigns new
    classes ; now supports `"dominant component"`,
    `"dominant condition"` and `"none"`

## soilDB 2.6.5 (2021-08-19)

CRAN release: 2021-08-21

- API calls that return geometry in projected coordinates (AEA/NAD83)
  now use ESPG:5070 instead of EPSG:6350

## soilDB 2.6.4 (2021-08-06)

- `fetchNASIS(from="pedons")` now supports `fill=TRUE` argument just
  like `from="components"` to include pedons that have no horizon
  records
- [`createStaticNASIS()`](http://ncss-tech.github.io/soilDB/reference/createStaticNASIS.md):
  column order should match NASIS, even if data types require reorder
  for ODBC driver
- [`fetchSoilGrids()`](http://ncss-tech.github.io/soilDB/reference/fetchSoilGrids.md)
  bug fixes, updates to metadata and references in documentation
  (<https://github.com/ncss-tech/soilDB/issues/201>)
  - Corrected the conversion factor used for predicted `nitrogen` values
    (conversion from cg/kg to g/kg)
  - Corrected the conversion factor used for uncertainty in `bdod` and
    `nitrogen` (SoilGrids uncertainty layer always uses factor of 10 to
    create integers)
  - Improved error handling
  - Added `progress` and `verbose` arguments for text progress bar and
    additional message output
  - Added support for {sf} and {sp} POINT geometry inputs
- Add
  [`get_SDA_coecoclass()`](http://ncss-tech.github.io/soilDB/reference/get_SDA_coecoclass.md)
  SOD-style method for mapunit/component level summaries of ecological
  site and other vegetation class information

## soilDB 2.6.3 (2021-07-22)

CRAN release: 2021-07-23

- [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
  and all functions that call
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
  get proper column class handling (related to
  [\#190](https://github.com/ncss-tech/soilDB/issues/190)), however:
  - be careful with the use of CAST(): unknown datatypes may not be
    correctly interpreted
  - previous column classes that were incorrectly guessed by
    [`type.convert()`](https://rdrr.io/r/utils/type.convert.html) may
    have changed (e.g. `component.wei`)
- [`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
  can now be used to return soil survey area symbols or geometry using
  `what="areasymbol"` or `what="sapolygon"`, respectively
- Added new columns to soil classification (“SC”) table result of
  [`get_soilseries_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_soilseries_from_NASIS.md);
  now including taxonomic mineralogy class which may contain multiple
  parts for series with strongly contrasting control sections
- Updates to `get_SDA_*()` methods
  - Extends `get_SDA_property(property = ...)` and
    `get_SDA_interpretation(rulename = ...)` vectorization over
    property/rulename to work with any aggregation method.
    - Now supports: Dominant Condition, Min, Max, Dominant Component,
      Weighted Average
  - Add `query_string` argument (default: `FALSE`). Set as `TRUE` to
    skip submitting query to SDA, instead returning a string of the
    query that would have been sent instead of *data.frame* result
  - `get_SDA_property`: better handling of NULL, miscellaneous areas,
    and property-specific weighting
    - Remove `ISNULL(x, 0)` logic that affected weighted averages in
      presence of missing data
    - Conditional calculation of horizon weights considering NULL values
      for requested properties (unique weights for each property)
    - New default argument `include_minors=FALSE` includes only
      components where `majcompflag = 'Yes'` in result
    - New default argument `miscellaneous_areas=FALSE` removes
      miscellaneous land types `compkind` values from result
    - Organic and bedrock layers are no longer removed from “Weighted
      Average”, “MIN” or “MAX” aggregations
  - `get_SDA_interpretation`: added argument not_rated_value with
    default value of `NA` to set “not rated”” values across
    methods/queries. For backwards compatibility with original SQL use
    `not_rated_value = 99.0`
  - Standardizing MUKEY column name (and other keys) as lowercase in
    results
  - More informative error messages for bad input / arguments
    inconsistent with specified method
- Thanks to [@hammerly](https://github.com/hammerly) who pointed out
  weighted averaging of NASIS `phlabresults` wasn’t working as expected
  and for highlighting some more improvements
  (<https://github.com/ncss-tech/soilDB/issues/192>)
- [`get_OSD()`](http://ncss-tech.github.io/soilDB/reference/get_OSD.md)
  TXT and HTML formats now supported (in addition to JSON) through a
  common function interface
- Added
  [`get_NASIS_table_key_by_name()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_key_by_name.md)
  [`get_NASIS_fkey_by_name()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_key_by_name.md),
  [`get_NASIS_pkeyref_by_name()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_key_by_name.md),
  [`get_NASIS_pkey_by_name()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_key_by_name.md),
  [`get_NASIS_table_name_by_purpose()`](http://ncss-tech.github.io/soilDB/reference/get_NASIS_table_name_by_purpose.md)
  methods for helping get information on primary/foreign keys and
  thematic groups of NASIS tables (useful for creating SQLite/external
  snapshots of NASIS tables)
- [`get_mapunit_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_mapunit_from_NASIS.md),
  [`get_legend_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_mapunit_from_NASIS.md)
  and
  [`get_lmuaoverlap_from_NASIS()`](http://ncss-tech.github.io/soilDB/reference/get_mapunit_from_NASIS.md)
  now works for “MLRA Survey Area” `areatypename` and no longer is
  limited by constraints on `legendsuituse` or `mustatus`

## soilDB 2.6.2 (2021-05-14)

CRAN release: 2021-05-16

- Added `formativeElement` argument to
  [`taxaExtent()`](http://ncss-tech.github.io/soilDB/reference/taxaExtent.md)
  (SoilWeb taxon extent function)
  - “Formative elements” are derived from the dictionary provided by the
    {SoilTaxonomy} package
    (<https://cran.r-project.org/package=SoilTaxonomy>)
  - For example:
    `taxaExtent("abruptic", level = 'subgroup', formativeElement = TRUE)`
    will get an 800m grid (for SSURGO data in CONUS) showing extent of
    taxa that have “abruptic” in subgroup-level taxon name
- `fetchNASIS(from="pedons")` result now contains the `"taxclname"`
  (full family-level taxon name) field from the NASIS `pedon` table;
  this value is calculated based on contents of `petaxhistory` child
  table
- `get_SDA_interpretation` and `get_SDA_property` now support
  aggregation `method="NONE"` allowing for returning properties, classes
  and ratings for individual components or horizons
  (<https://github.com/ncss-tech/soilDB/pull/181>)
- `ISSR800.wcs` and `mukey.wcs` now return a result that inherits from
  `try-error` (and a message) if the Web Coverage Service query fails

## soilDB 2.6.1 (2021-04-07)

CRAN release: 2021-04-18

- Connections to local NASIS and various MS Access databases now use
  `DBI` and `odbc`, replacing `RODBC`
- New methods `dbConnectNASIS` and `dbQueryNASIS` for NASIS access with
  read-only credentials, fetching query results, and closing the
  *DBIConnection* upon completion
- NASIS methods use `dsn` argument to specify a local “static” SQLite
  file containing NASIS tables, or custom *DBIConnection* to a database
  with NASIS schema
  - Default `dsn = NULL` uses `"nasis_local"` [ODBC
    connection](http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.md)
    to local NASIS SQL Server
- Horizon depth logic checking is now done with
  [`aqp::checkHzDepthLogic()`](https://ncss-tech.github.io/aqp/reference/checkHzDepthLogic.html),
  powered by {data.table}
- Added several new SDA query methods based on
  <https://github.com/ncss-tech/ssurgoOnDemand> by
  [@jneme910](https://github.com/jneme910) and
  [@cferguso](https://github.com/cferguso):
  - `get_SDA_property`, `get_SDA_interpretation`, `get_SDA_muaggatt`,
    `get_SDA_hydric`, `get_SDA_pmgroupname`

## soilDB 2.6.0 (2021-02-18)

- `OSDquery` gets a new argument (`everything`) for searching the entire
  document
- `fetchNASIS(..., rmHzErrors=TRUE)` – spurious removals of data due to
  missing “extended” records. `fetchNASIS` now uses `aqp::horizons<-`
  after building a minimal `SoilProfileCollection` from NASIS site and
  horizon tables. This allows `aqp` integrity methods to trigger where
  needed–preventing unintentional re-ordering or removals of “valid”
  horizon data.

## soilDB 2.5.9 (2021-01-26)

CRAN release: 2021-01-26

- `HenryTimeLine` moved to {sharpshootR} package
- new functions
  [`mukey.wcs()`](http://ncss-tech.github.io/soilDB/reference/mukey.wcs.md)
  and
  [`ISSR800.wcs()`](http://ncss-tech.github.io/soilDB/reference/ISSR800.wcs.md)
  for hitting web coverage service (WCS) for gSSURGO, gNATSGO, and
  ISSR-800 grids
- new function
  [`ROSETTA()`](http://ncss-tech.github.io/soilDB/reference/ROSETTA.md)
  for accessing the new ROSETTA model API (c/o Todd Skaggs, USDA-ARS)
- `fetchOSD(..., extended=TRUE)` gains geographically associated soils,
  parsed from OSD (thanks AGB)
- `fetchSDA_spatial` now can return soil survey area polygons using
  `geom.src = "sapolygon"` with `x` as a vector of area symbols
  (`areasymbol`) or legend keys (`lkey`). For `sapolygon` results, the
  `method` and `add.field` arguments work the same as for `mupolygon`,
  only now both geometries can be returned with fields from the `legend`
  table.
- `fetchSDA_spatial` now can return STATSGO `gsmmupolygon` geometry with
  `db = "STATSGO"`; these data are linked to `mapunit` and (national)
  `legend` just like `mupolygon`.

## soilDB 2.5.8 (2020-10-20)

CRAN release: 2020-10-21

- `fetchNASIS` / `soilDB:::.rockFragmentSieve` now uses fragment RV,
  `soilDB:::.sieve` uses `<` operator
  (<https://github.com/ncss-tech/soilDB/issues/1>)
- `fetchKSSL(..., returnMorphologicData=TRUE)` now returns redoximorphic
  features by horizon
- new function `taxaExtent` returns 800m gridded taxonomic grids for
  CONUS
- `seriesExtent` can now return 800m gridded series extents for CONUS
- `SDA_spatialQuery` can now retrieve SSURGO and STATSGO geometry c/o
  [dschlaep](https://github.com/dschlaep)
  (<https://github.com/ncss-tech/soilDB/issues/141>)
- new import: `data.table`; beginning to implement *data.table*-aware
  methods in *aqp* and *soilDB*

## soilDB 2.5.7 (2020-09-03)

- add `fetchGDB` for querying tabular data from SSURGO/gNATSGO/STATSGO
  File Geodatabases
- add
  [`get_NOAA_GHCND()`](http://ncss-tech.github.io/soilDB/reference/get_NOAA_GHCND.md)
  and
  [`get_NOAA_stations_nearXY()`](http://ncss-tech.github.io/soilDB/reference/get_NOAA_stations_nearXY.md)
  for batch queries of NOAA Daily Climate Data (requires free API token)
- bug fix for `fetchSDA_spatial` with `chunk.size` \> 1 resulting in
  duplicate data in result
- major improvements to `fetchSDA_spatial` to handle queries that exceed
  JSON Serialization Limit
- add `fetchSoilGrids` for point data queries to SoilGrids v2 API with
  SoilProfileCollection output
- `fetchKSSL(..., returnGeochemicalData=TRUE)` now returns geochemical,
  optical and XRD/thermal data

## soilDB 2.5.6 (2020-06-16)

- bug fixes in `fetchKSSL` related to vectorization, all arguments
  vectorized except for `bbox`
- `KSSL_VG_model` output cleaned-up, now returns phi -\> theta function

## soilDB 2.5.3 (2020-03-22)

- `fetchKSSL` is now fully vectorized and builds on new SoilWeb JSON API

## soilDB 2.5.2 (2020-02-05)

- add
  [`get_concentrations_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  and
  [`get_phfmp_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  for fetching Pedon Horizon Concentrations and Field Measured
  Properties from NASIS local database

## soilDB 2.5.1 (2020-01-29)

- bug fix for
  `fetchNASIS(from='components', fill=TRUE, rmHzErrors=TRUE)` in context
  of new `::hzDepthTests()` and non-unique `chiid` due to `NA` values
  introduced by `fill`

## soilDB 2.5 (2020-01-23)

CRAN release: 2020-01-28

- CRAN release
- `simplifyColorData` and `mix_and_clean_colors` always use CIELAB
  colorspace for weighted averages, and best-available metric for
  transformation to Munsell notation
- `fetchSDA_spatial` - new fetch function that simplifies getting
  spatial data associated with a vector of `mukey` or `nationalmusym`.
  The function has options for customizing result attribute table and is
  designed to automatically use
  [`makeChunks()`](http://ncss-tech.github.io/soilDB/reference/makeChunks.md)
  to prevent timeout on large queries.
- `aqp::test_hz_logic` is now deprecated – refactored affected fetch
  functions

## soilDB 2.4.3 (2020-01-07)

- surface water depth added to `fetchNASIS_pedons()`
- [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  has [@restrictions](https://github.com/restrictions) set automatically
  if data are populated
- new function for accessing pedon RMF in local NASIS DB:
  [`get_RMF_from_NASIS_db()`](http://ncss-tech.github.io/soilDB/reference/get_RMF_from_NASIS_db.md)

## soilDB 2.4 (2019-11-05)

- CRAN release (<https://github.com/ncss-tech/soilDB/releases/tag/2.4>)
- documentation updates

## soilDB 2.3.9 (2019-07-25)

- SDA_query() no longer writes temporary files, c/o suggestion from Kyle
  Bocinsky ([\#106](https://github.com/ncss-tech/soilDB/issues/106) /
  [\#108](https://github.com/ncss-tech/soilDB/issues/108))
- fetchOSD() gets a sanity check to protect against going over GET
  request limits
- makeChunks() added to util functions, useful for splitting data before
  sending API calls

## soilDB 2.3.8 (2019-03-22)

- loafercreek, gopheridge, and mineralking sample data have been updated
  with valid place-holder in [@sp](https://github.com/sp)
- bug fix for
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)
  related to multi-line records
  (<https://github.com/ncss-tech/soilDB/issues/28>)

## soilDB 2.3.7 (2019-03-12)

- `sharpshootR` added to SUGGESTS
- [`fetchHenry()`](http://ncss-tech.github.io/soilDB/reference/fetchHenry.md)
  and fetchSCAN() now include water year/day (Oct 1 – Sep 30)
- `HenryTimeLine()` convenience function added
- bug fix in `fetchOSD(..., extended=TRUE)` when no climate data
  available
- bug fix in
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md)

## soilDB 2.3.6 (2019-02-12)

- [`simplifyFragmentData()`](http://ncss-tech.github.io/soilDB/reference/simplifyFragmentData.md)
  and related functions now 4-5x faster
- [`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)
  now returns metadata when extended=TRUE

## soilDB 2.3.5 (2019-01-14)

CRAN release: 2019-01-14

- NAMESPACE and R CMD check fixes
- documentation updates
- soilDB now suggests `stringr`
- new tests
- release to CRAN

## soilDB 2.3.3 (2018-12-18)

- bug fix in
  [`fetchNASIS()`](http://ncss-tech.github.io/soilDB/reference/fetchNASIS.md)
  related to conversion of NULL fragment volume to 0
- [`fetchKSSL()`](http://ncss-tech.github.io/soilDB/reference/fetchKSSL.md)
  can now automatically simplify colors with `simplifyColors = TRUE`

## soilDB 2.3 (2018-11-17)

CRAN release: 2018-11-19

- new function for exploring soil series co-occurrence data:
  [`siblings()`](http://ncss-tech.github.io/soilDB/reference/siblings.md)
- `fetchOSD(..., extended=TRUE)` gets competing soil series
- new tests
- removed old sample gSSURGO chunk and related documentation
- release to CRAN

## soilDB 2.2-6 (2018-10-11)

- new function for spatial queries: SDA_spatialQuery(), still needs
  testing and documentation

## soilDB 2.2-6 (2018-10-11)

- [`fetchOSD()`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)
  gets an overhaul, new API and features

## soilDB 2.2-5 (2018-10-05)

- experimental interface to SoillWeb OSD fulltext search:
  [`OSDquery()`](http://ncss-tech.github.io/soilDB/reference/OSDquery.md)

## soilDB 2.2-4 (2018-09-04)

- numerous bug-fixes in simplifyFragmentData() see
  (<https://github.com/ncss-tech/soilDB/issues/70>)

## soilDB 2.2-1 (2018-05-21)

- get_mutext_from_NASIS_db() added for extraction of map unit text notes

## soilDB 2.2 (2018-05-08)

- CRAN release
- better bug-fix for fetchSCAN and missing data
  (<https://github.com/ncss-tech/soilDB/issues/26>)
- exposing some of the internal functionality used by fetchHenry

## soilDB 2.1-1 (2018-03-29)

- added more SCAN/SNOTEL metadata
  (<https://github.com/ncss-tech/soilDB/issues/61>)

## soilDB 2.1 (2018-03-12)

- generalized local NASIS ODBC authentication, should work on windows 7,
  8, 10 <https://github.com/ncss-tech/soilDB/issues/54>

## soilDB 2.0-4 (2018-03-06)

- bug fix for subtle change in how SCAN data are returned from
  webservice

## soilDB 2.0-3 (2018-02-13)

- bug fix for simplifyFragmentData(, nullFragsAreZero=FALSE), still more
  work to do

## soilDB 2.0-2 (2018-01-29)

- bug fix for simplifyFragmentData() when fragment volume \> 100%

## soilDB 2.0-1 (2018-01-23)

CRAN release: 2018-01-23

- updated `loafercreek` and `gopheridge` sample datasets and manual page
  to reflect latest fetchNASIS

## soilDB 2.0 (2017-12-23)

CRAN release: 2018-01-11

- bug fixes, manual page updates, and stabilization of NASIS interaction
- released to CRAN

## soilDB 1.8.12 (2017-09-19)

- bug fix in fetchHenry(), it is now possible to query only sensor
  metadata

## soilDB 1.8.11 (2017-08-17)

- temporary bug-fix related to SCAN data
  (<https://github.com/ncss-tech/soilDB/issues/26>)

## soilDB 1.8.10 (2017-08-10)

- re-write of mix_and_clean_colors() so that color mixing happens in CIE
  LAB space

## soilDB 1.8.5 (2017-05-24)

- re-write of SDA_query(), better error handling, support for multi-part
  result sets

## soilDB 1.8.2 (2017-03-24)

- fetchHenry() can now access water level data, see manual page

## soilDB 1.8.1 (2017-01-23)

- new function: get_comonth_from_NASIS_db() see manual page for details

## soilDB 1.8-10 (2016-12-27)

- converting all URLs to ‘HTTPS’, addressing bugs with fetchSCAN() and
  associated functions

## soilDB 1.8-7 (2016-11-16)

CRAN release: 2016-11-10

- sanity checks for fetchHenry()
- SCAN_sensor_metadata() vectorized and documented
- unique row names in SPDF returned by seriesExtent()

## soilDB 1.8-6 (2016-11-04)

- converted all links to SoilWeb servers to HTTPS
- converted all NRCS web-service links to HTTPS

## soilDB 1.8-4 (2016-10-07)

- fetchOSD() now returns dry colors and the source “narrative” for each
  horizon; function gains an argument
- fetchOSD() now returns texture class, coarse fragment modifier, pH,
  and pH class
- fetchSCAN() re-written, see manual pages and
  [http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.html](http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.md)

## soilDB 1.8 (2016-04-29)

CRAN release: 2016-04-29

- fetchKSSL() can now query basic morphologic data with the argument
  “returnMorphologicData”
- two new functions for simplifying NASIS color and fragment data:
  - simplifyFragmentData()
  - simplifyColorData()
- ^^^ these new functions are now used by fetchNASIS() to summarize
  color and fragment data (vs. SQL)
- NOTE: soilDB now uses reshape2::dcast() instead of reshape::cast()

## soilDB 1.7 (2016-04-18)

CRAN release: 2016-02-16

- bug fix in colors returned by fetchNASIS(), reported by Andy Paolucci
- bug fix in SDA_query(), due to changes in httr::content. Thanks Kyle
  Bocinsky for the fix!
- SDA_query() now returns SQL errors generated by SDA
- new columns from NASIS site table returned by fetchNASIS()
- NASIS pedon ecosite data now returned by fetchNASIS() (c/o J. Skovlin)

## soilDB 1.6.9 (2015-12-28)

- clean-up in local NASIS queries, removed extra parentheses
- NASIS component query function overhaul: previous code may be broken,
  details pending

## soilDB 1.6.8 (2015-12-22)

- added temperature class (temp_class) field from the pedon taxonomic
  history table (NASIS) \[addition suggested by J. Baker\]

## soilDB 1.6.7 (2015-12-16)

- experimental function for processing SDA queries that return geometry:
  processSDA_WKT()

## soilDB 1.6.5 (2015-12-03)

- bug fix for poorly specified geomorphic descriptions, caused
  fetchNASIS() to barf
- fetchKSSL() gains new query filters, see man page

## soilDB 1.6.4 (2015-11-27)

- package re-org, getting ready for soilDB 2.0:
  - dropping use of `RCurl` functions in favor of `httr` alternatives
    (done)
  - switch from `reshape` to `reshape2` (pending)
  - possibly move some packages from SUGGESTS to IMPORTS

## soilDB 1.6.3 (2015-11-20)

- new spatial query helper functions for SDA
- re-named SSURGO_spatial_query() to SoilWeb_spatial_query(), this
  function may be phased-out due to spatial support now available in SDA
- removed MUKEYS_by_ll_bbox() function, no longer needed and web-service
  may be disabled at any time
- mapunit_geom_by_ll_bbox() will probably be removed in the near future
  with an SDA-based alternative
- KSSL data updated (server-side) to June 2015 snapshot, “site_id”
  column removed from fetchKSSL() results

## soilDB 1.6 (2015-08-19)

- soilDB now imports from the `reshape` package, will transition to
  `reshape2` with soilDB 2.0
- fetchHenry() officially added, complete with documentation
- SDA_query() undergoing some upgrades, no longer requires SSOAP /
  XMLSchema packages
- SSOAP / XMLSchema packages no longer in ‘suggests’ list

## soilDB 1.5-8 (2015-08-13)

- fetchKSSL() gets an argument for downloading data by MLRA

## soilDB 1.5-7 (2015-03-10)

- updated the `loafercreek` and `gopheridge` sample datasets

## soilDB 1.5-3 (2015-03-10)

- nullFragsAreZero argument to fetchNASIS() reset to TRUE, new
  documentation pending
- consolidate of messages printed when running fetchNASIS()
- introduction of new option “soilDB.verbose” (default is FALSE) for
  increasing the level of QC information printed
- new tutorial on fetchNASIS()

## soilDB 1.5-2 (2015-02-24)

CRAN release: 2015-02-25

- RODBC temporarily moved from “suggested” package to “imported”
  package… not a good idea, reverted to “suggested”

## soilDB 1.4-5 (2015-01-20)

- mix_and_clean_colors() now returns back-transformed, mixed, munsell
  colors
- added psctopdepth and pscbotdepth back to extended NASIS query
  (particle size control section depths)

## soilDB 1.4-3 (2015-01-12)

- get_hz_data_from_NASIS_db() no longer assumes fragment volume of NULL
  = 0
- get_extended_data_from_NASIS_db() no longer assumes fragment volume of
  NULL = 0
- fetchNASIS() has a new argument, nullFragsAreZero=FALSE; set to TRUE
  for past NULL -\> 0 conversion

## soilDB 1.4-2 (2014-12-23)

- new internally-used function .formatLandformString() for flattening
  NASIS/PedonPC “landform” records
- new internally-used function .formatParentMaterialString() for
  flattening NASIS/PedonPC “parent material” records
- fetchNASIS() incorporates the results of these two new functions
- new function SSURGO_spatial_query() queries SSURGO data from SoilWeb
  (see manual page for details)

## soilDB 1.4 (2014-11-13)

- moving most of the hard-to-install packages into `suggests`, and
  checking for package availability at function runtime. this will make
  soilDB more portable, as not every user will want or need all
  functionality.

## soilDB 1.3-6 (2014-10-27)

- fetchNASIS() gains argument rmHzErrors to optionally retain pedons
  with horizonation errors. use with caution.

## soilDB 1.3-5 (2014-09-25)

- loafercreek sample dataset re-created from new data
- seriesExtent() now utilizes pre-cached GeoJSON files from
  \[<http://casoilresource.lawr.ucdavis.edu/see/>\]

## soilDB 1.3 (2014-02-26)

- Changes made to fetch functions to accommodate changes implemented in
  the NASIS 6.3 data structure
- get_site_data_from_NASIS_db(): in cases where multiple records in
  site-bedrock exist, retain only the most shallow
- fetchRaCA() has a new argument for querying data by rcasiteid

## soilDB 1.2-9 (2014-01-06)

- SOC concentration and stock values are temporarily disabled in
  fetchRaCA()
  - waiting for new estimates from the Soil Survey Center…

## soilDB 1.2-6 (2013-12-05)

CRAN release: 2013-12-17

- added new function contributed by J.M. Skovlin:
  get_veg_from_NPS_PLOTS_db()
- bug fixes in get_extended_data_from_NASIS_db(): results from geomorph
  tables are now returned

## soilDB 1.2-3 (2013-10-25)

- added new function fetchSCAN() for downloading soil/climate data from
  USDA-NRCS SCAN stations (still experimental!)

## soilDB 1.2-1 (2013-09-12)

- bug fixing in fetchRaCA():
  - “#” characters no longer cause errors
  - duplicate IDs in source data have been fixed

## soilDB 1.2 (2013-08-13)

- fetchRaCA() function now stable, ready for general usage
- fixed bug in fetchRaCA() function where the presence of single-quote
  character in horizon designation would throw and error

## soilDB 1.1-3 (2013-07-30)

- new function get_copedon_from_NASIS_db(): returns basic information
  associated with pedons linked to component data in NASIS
- new function fetchRaCA(): gets data from the Rapid Carbon Assessment
  project \[work in progress\]

## soilDB 1.1 (2013-06-27)

- get_site_data_from_NASIS_db() no longer has problems when more than 1
  site bedrock kind is defined; only the shallowest row is returned

## soilDB 1.0 (2013-04-04)

CRAN release: 2013-04-18

- fetchNASIS() is now much more efficient: faster / less memory used
- new function! fetchKSSL(): experimental interface to (most of) the
  KSSL data
  - queries can be performed by series name or GCS bounding box
  - data are delivered via CA Soil Resource Lab server

## soilDB 0.9-8 (2013-04-03)

- fetchOSD() now immune to queries for non-soils
- improved efficiency in fetch\* functions
- NASIS-DMU fetching functions now return more information, still not as
  complete as pedon-queries

## soilDB 0.9-7 (2013-03-01)

- NASIS query functions now return only records from the selected set,
  please test older code!

## soilDB 0.9-6 (2013-02-26)

CRAN release: 2013-02-28

- new function get_text_notes_from_NASIS_db() – still needs proper
  documentation
- bug fixes to PedonPC and NASIS queries, new data elements returned by
  extended query functions
- SoilWeb queries via URL are now URL-encoded to allow for spaces in
  soil series names

## soilDB 0.9-4 (2013-01-31)

- new function: fetchOSD() returns basic OSD data as a
  SoilProfileCollection

## soilDB 0.9-3 (2013-01-08)

CRAN release: 2013-01-10

- speed-bump in color-fetching functions from NASIS/pedonPC
- fetchNASIS() now attempts to correct for \>1 pedon/site by appending
  peiid to pedon_id

## soilDB 0.9-2 (2012-12-18)

- new functions for getting / plotting the geographic extent of a soil
  series, using the SoilWeb query service: +seriesExtent() : fetches
  extent as a SpatialPolygonDataFrame +seriesExtentAsGmap() : fetches
  extent and plots on Google Maps (requires dismo package)

## soilDB 0.9-1 (2012-11-27)

- updated functionality in fetchNASIS_component_data() pulls
  mapunit-level data as well as component-level data
- Munsell ‘value’ is now returned from PedonPC/NASIS fetching functions

## soilDB 0.9 (2012-10-24)

- added a surface fragment summary “surf_frag_summary” to the results of
  get_extended_data_from_NASIS()
  - note: this has not been ported to the related PedonPC queries
- integrated surf_frag_summary data into data returned from
  fetchNASIS(), stored in [@site](https://github.com/site)
  - note: this has not been ported to the related PedonPC queries
- minor bug-fix in horizon-level rock fragment summary SQL which
  previously mis-classified BD as ST in some, rare cases
  - note: this has not been ported to the related PedonPC queries

## soilDB 0.8-3 (2012-09-28)

CRAN release: 2012-10-10

- site-level data: {elev, slope, aspect} are now named {elev_field,
  slope_field, aspect_field}
  - this affects the data returned by fetchNASIS() and fetchPedonPC()

## soilDB 0.8-2 (2012-08-23)

- compatible with NASIS 6.2
- bug fixes
- taxhistory selection method is preserved in the results
- ‘taxonname’ is used instead of ‘sampled_as’ / ’correlated_as, **seems
  to work, but needs further testing**

## soilDB 0.8 (2012-08-20)

- updating for PedonPC 5.0 and NASIS 6.2
  - site data contains the best-guessed corresponding row from the
    taxhistory table, based on:
    1.  most recent record, or 2) record with the least amount of
        missing data
  - taxhistory data are now included in the output from
    get_extended_data_from_pedon_db()
- package is now partially compatible with NASIS 6.2

## soilDB 0.6 (2012-06-19)

- tidying up documentation, package dependencies, and NAMESPACE

## soilDB 0.5-6 (2012-04-16)

- adding preliminary functions for querying component data from local
  NASIS
- fixed minor bug in SDA_query() and added some links to related
  documentation

## soilDB 0.5-1 (2012-02-22)

- fetchNASIS() and fetchPedonPC() now integrate \`extended’ data
- warning and error messages cleaned-up
- multiple textures no longer cause duplicate HZ rows (NASIS only)
- extended queries now split frags/para-frags
- silt fraction is estimated from 100-(sand+clay) when possible
- added local NASIS ODBC connection vignette (thanks JMS)
- removed dsn argument from NASIS functions as it should always be
  ‘nasis_local’
- new wide-formatted, boolean representation of diagnostic horizon data
- queries standardized between NASIS/PedonPC
- new fetchNASIS() function for 1-line access to local NASIS data
- new sample data set ‘loafercreek’
- basic vignette added, switching to knitr-style vignettes

## soilDB 0.3-3 (2012-01-10)

CRAN release: 2012-01-16

- SDA_query() now functional on MacOS/UNIX-like OSes with (SSOAP 0.8-1,
  XMLSchema 0.6-0, and XML 3.7-1), thanks to D.T. Lang for the updates!
- moving hard to find packages to ‘suggested’ status: SSOAP, RCurl, XML,
  rgdal
- new wrapper function fetchPedonPC() for typical site/pedon/hz queries
- new function getHzErrorsPedonPC() for ID-ing pedons with problem
  horizonation
- NOTE: hz data queries will return 2 rows/hz (error) when multiple
  texture classes are assigned to a single horizon- this is a bug, not a
  feature!

## soilDB 0.3-2 (2011-12-30)

CRAN release: 2012-01-03

- NASIS/pedonPC queries synced
- minor bug fixes and documentation updates
- updated query structure, switching over to native NASIS/PedonPC IDs
- extended summaries added: NASIS only

## soilDB 0.2 (2011-12-27)

- moved functions out of aqp package
- basic query functionality from local NASIS DB (Jay)

## soilDB 0.1 (2011-12-23)

- initial version on r-forge
- functions still exist in aqp package… need to move over completely

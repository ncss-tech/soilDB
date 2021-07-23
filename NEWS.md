# soilDB 2.6.3 (2021-07-22)
 * `SDA_query()` and all functions that call `SDA_query()` get proper column class handling (related to #190), however:
   - be careful with the use of CAST(): unknown datatypes may not be correctly interpreted
   - previous column classes that were incorrectly guessed by `type.convert()` may have changed (e.g. `component.wei`)
 * `SDA_spatialQuery()` can now be used to return soil survey area symbols or geometry using `what="areasymbol"` or `what="sapolygon"`, respectively
 * Added new columns to soil classification ("SC") table result of `get_soilseries_from_NASIS()`; now including taxonomic mineralogy class which may contain multiple parts for series with strongly contrasting control sections
 * Updates to `get_SDA_*()` methods 
   - Extends `get_SDA_property(property = ...)` and `get_SDA_interpretation(rulename = ...)` vectorization over property/rulename to work with any aggregation method. 
     - Now supports: Dominant Condition, Min, Max, Dominant Component, Weighted Average
   - Add `query_string` argument (default: `FALSE`). Set as `TRUE` to skip submitting query to SDA, instead returning a string of the query that would have been sent instead of _data.frame_ result
   - `get_SDA_property`: better handling of NULL, miscellaneous areas, and property-specific weighting
     - Remove `ISNULL(x, 0)` logic that affected weighted averages in presence of missing data
     - Conditional calculation of horizon weights considering NULL values for requested properties (unique weights for each property)
     - New default argument `include_minors=FALSE` includes only components where `majcompflag = 'Yes'` in result
     - New default argument `miscellaneous_areas=FALSE` removes miscellaneous land types `compkind` values from result
     - Organic and bedrock layers are no longer removed from "Weighted Average", "MIN" or "MAX" aggregations
   - `get_SDA_interpretation`: added argument not_rated_value with default value of `NA` to set "not rated"" values across methods/queries. For backwards compatibility with original SQL use `not_rated_value = 99.0`
   - Standardizing MUKEY column name (and other keys) as lowercase in results 
   - More informative error messages for bad input / arguments inconsistent with specified method
 * Thanks to @hammerly who pointed out weighted averaging of NASIS `phlabresults` wasn't working as expected and for highlighting some more improvements (https://github.com/ncss-tech/soilDB/issues/192)
 * `get_OSD()` TXT and HTML formats now supported (in addition to JSON) through a common function interface
 * Added `get_NASIS_table_key_by_name()` `get_NASIS_fkey_by_name()`, `get_NASIS_pkeyref_by_name()`, `get_NASIS_pkey_by_name()`, `get_NASIS_table_name_by_purpose()` methods for helping get information on primary/foreign keys and thematic groups of NASIS tables (useful for creating SQLite/external snapshots of NASIS tables)
 * `get_mapunit_from_NASIS()`, `get_legend_from_NASIS()` and `get_lmuaoverlap_from_NASIS()` now works for "MLRA Survey Area" `areatypename` and no longer is limited by constraints on `legendsuituse` or `mustatus`
 
# soilDB 2.6.2 (2021-05-14)
 *  Added `formativeElement` argument to `taxaExtent()` (SoilWeb taxon extent function)
    - "Formative elements" are derived from the dictionary provided by the {SoilTaxonomy} package (https://cran.r-project.org/package=SoilTaxonomy)
    - For example: `taxaExtent("abruptic", level = 'subgroup', formativeElement = TRUE)` will get an 800m grid (for SSURGO data in CONUS) showing extent of taxa that have "abruptic" in subgroup-level taxon name
 * `fetchNASIS(from="pedons")` result now contains the `"taxclname"` (full family-level taxon name) field from the NASIS `pedon` table; this value is calculated based on contents of `petaxhistory` child table
 * `get_SDA_interpretation` and `get_SDA_property` now support aggregation `method="NONE"` allowing for returning properties, classes and ratings for individual components or horizons (https://github.com/ncss-tech/soilDB/pull/181)
 * `ISSR800.wcs` and `mukey.wcs` now return a result that inherits from `try-error` (and a message) if the Web Coverage Service query fails

# soilDB 2.6.1 (2021-04-07)
 * Connections to local NASIS and various MS Access databases now use `DBI` and `odbc`, replacing `RODBC`
 * New methods `dbConnectNASIS` and `dbQueryNASIS` for NASIS access with read-only credentials, fetching query results, and closing the _DBIConnection_ upon completion
 * NASIS methods use `dsn` argument to specify a local "static" SQLite file containing NASIS tables, or custom _DBIConnection_ to a database with NASIS schema
   * Default `dsn = NULL` uses `"nasis_local"` [ODBC connection](http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html) to local NASIS SQL Server 
 * Horizon depth logic checking is now done with `aqp::checkHzDepthLogic()`, powered by {data.table}
 * Added several new SDA query methods based on https://github.com/ncss-tech/ssurgoOnDemand by @jneme910 and @cferguso:
   * `get_SDA_property`, `get_SDA_interpretation`, `get_SDA_muaggatt`, `get_SDA_hydric`, `get_SDA_pmgroupname`
   
# soilDB 2.6.0 (2021-02-18)
 * `OSDquery` gets a new argument (`everything`) for searching the entire document
 * `fetchNASIS(..., rmHzErrors=TRUE)` -- spurious removals of data due to missing "extended" records. `fetchNASIS` now uses `aqp::horizons<-` after building a minimal `SoilProfileCollection` from NASIS site and horizon tables. This allows `aqp` integrity methods to trigger where needed--preventing unintentional re-ordering or removals of "valid" horizon data.

# soilDB 2.5.9 (2021-01-26)
 * `HenryTimeLine` moved to {sharpshootR} package
 * new functions `mukey.wcs()` and `ISSR800.wcs()` for hitting web coverage service (WCS) for gSSURGO, gNATSGO, and ISSR-800 grids
 * new function `ROSETTA()` for accessing the new ROSETTA model API (c/o Todd Skaggs, USDA-ARS)
 * `fetchOSD(..., extended=TRUE)` gains geographically associated soils, parsed from OSD (thanks AGB)
 * `fetchSDA_spatial` now can return soil survey area polygons using `geom.src = "sapolygon"` with `x` as a vector of area symbols (`areasymbol`) or legend keys (`lkey`). For `sapolygon` results, the `method` and `add.field` arguments work the same as for `mupolygon`, only now both geometries can be returned with fields from the `legend` table.
 * `fetchSDA_spatial` now can return STATSGO `gsmmupolygon` geometry with `db = "STATSGO"`; these data are linked to `mapunit` and (national) `legend` just like `mupolygon`.
 
# soilDB 2.5.8 (2020-10-20)
 * `fetchNASIS` / `soilDB:::.rockFragmentSieve` now uses fragment RV, `soilDB:::.sieve` uses `<` operator (https://github.com/ncss-tech/soilDB/issues/1)
 * `fetchKSSL(..., returnMorphologicData=TRUE)` now returns redoximorphic features by horizon
 * new function `taxaExtent` returns 800m gridded taxonomic grids for CONUS
 * `seriesExtent` can now return 800m gridded series extents for CONUS
 * `SDA_spatialQuery` can now retrieve SSURGO and STATSGO geometry c/o [dschlaep](https://github.com/dschlaep) (https://github.com/ncss-tech/soilDB/issues/141)
 * new import: `data.table`; beginning to implement _data.table_-aware methods in _aqp_ and _soilDB_

# soilDB 2.5.7 (2020-09-03) 
 * add `fetchGDB` for querying tabular data from SSURGO/gNATSGO/STATSGO File Geodatabases
 * add `get_NOAA_GHCND()` and `get_NOAA_stations_nearXY()` for batch queries of NOAA Daily Climate Data (requires free API token) 
 * bug fix for `fetchSDA_spatial` with `chunk.size` > 1 resulting in duplicate data in result
 * major improvements to `fetchSDA_spatial` to handle queries that exceed JSON Serialization Limit
 * add `fetchSoilGrids` for point data queries to SoilGrids v2 API with SoilProfileCollection output
 * `fetchKSSL(..., returnGeochemicalData=TRUE)` now returns geochemical, optical and XRD/thermal data
 
# soilDB 2.5.6 (2020-06-16)
 * bug fixes in `fetchKSSL` related to vectorization, all arguments vectorized except for `bbox`
 * `KSSL_VG_model` output cleaned-up, now returns phi -> theta function

# soilDB 2.5.3 (2020-03-22)
 * `fetchKSSL` is now fully vectorized and builds on new SoilWeb JSON API

# soilDB 2.5.2 (2020-02-05)
  * add `get_concentrations_from_NASIS_db()` and `get_phfmp_from_NASIS_db()` for fetching Pedon Horizon Concentrations and Field Measured Properties from NASIS local database
  
# soilDB 2.5.1 (2020-01-29)
  * bug fix for `fetchNASIS(from='components', fill=TRUE, rmHzErrors=TRUE)` in context of new `::hzDepthTests()` and non-unique `chiid` due to `NA` values introduced by `fill`
  
# soilDB 2.5 (2020-01-23)
  * CRAN release
  * `simplifyColorData` and `mix_and_clean_colors` always use CIELAB colorspace for weighted averages, and best-available metric for transformation to Munsell notation
  * `fetchSDA_spatial` - new fetch function that simplifies getting spatial data associated with a vector of `mukey` or `nationalmusym`. The function has options for customizing result attribute table and is designed to automatically use `makeChunks()` to prevent timeout on large queries.
  * `aqp::test_hz_logic` is now deprecated -- refactored affected fetch functions

# soilDB 2.4.3 (2020-01-07)
  * surface water depth added to `fetchNASIS_pedons()`
  * `fetchNASIS()` has @restrictions set automatically if data are populated
  * new function for accessing pedon RMF in local NASIS DB: `get_RMF_from_NASIS_db()`
  
# soilDB 2.4 (2019-11-05)
   * CRAN release (https://github.com/ncss-tech/soilDB/releases/tag/2.4)
   * documentation updates

# soilDB 2.3.9 (2019-07-25)
   * SDA_query() no longer writes temporary files, c/o suggestion from Kyle Bocinsky (#106 / #108)
   * fetchOSD() gets a sanity check to protect against going over GET request limits
   * makeChunks() added to util functions, useful for splitting data before sending API calls

# soilDB 2.3.8 (2019-03-22)
   * loafercreek, gopheridge, and mineralking sample data have been updated with valid place-holder in @sp
   * bug fix for `SDA_query()` related to multi-line records (https://github.com/ncss-tech/soilDB/issues/28)

# soilDB 2.3.7 (2019-03-12)
   * `sharpshootR` added to SUGGESTS
   * `fetchHenry()` and fetchSCAN() now include water year/day (Oct 1 -- Sep 30)
   * `HenryTimeLine()` convenience function added
   * bug fix in `fetchOSD(..., extended=TRUE)` when no climate data available
   * bug fix in `SDA_query()`

# soilDB 2.3.6 (2019-02-12)
   * `simplifyFragmentData()` and related functions now 4-5x faster
   * `fetchOSD()` now returns metadata when extended=TRUE

# soilDB 2.3.5 (2019-01-14)
   * NAMESPACE and R CMD check fixes
   * documentation updates
   * soilDB now suggests `stringr`
   * new tests
   * release to CRAN

# soilDB 2.3.3 (2018-12-18)
   * bug fix in `fetchNASIS()` related to conversion of NULL fragment volume to 0
   * `fetchKSSL()` can now automatically simplify colors with `simplifyColors = TRUE`

# soilDB 2.3 (2018-11-17)
   * new function for exploring soil series co-occurrence data: `siblings()`
   * `fetchOSD(..., extended=TRUE)` gets competing soil series
   * new tests
   * removed old sample gSSURGO chunk and related documentation
   * release to CRAN

# soilDB 2.2-6 (2018-10-11)
   * new function for spatial queries: SDA_spatialQuery(), still needs testing and documentation

# soilDB 2.2-6 (2018-10-11)
   * `fetchOSD()` gets an overhaul, new API and features

# soilDB 2.2-5 (2018-10-05)
   * experimental interface to SoillWeb OSD fulltext search: `OSDquery()`

# soilDB 2.2-4 (2018-09-04)
   * numerous bug-fixes in simplifyFragmentData() see (https://github.com/ncss-tech/soilDB/issues/70)

# soilDB 2.2-1 (2018-05-21)
   * get_mutext_from_NASIS_db() added for extraction of map unit text notes

# soilDB 2.2 (2018-05-08)
   * CRAN release
   * better bug-fix for fetchSCAN and missing data (https://github.com/ncss-tech/soilDB/issues/26)
   * exposing some of the internal functionality used by fetchHenry

# soilDB 2.1-1 (2018-03-29)
   * added more SCAN/SNOTEL metadata (https://github.com/ncss-tech/soilDB/issues/61)

# soilDB 2.1 (2018-03-12)
   * generalized local NASIS ODBC authentication, should work on windows 7, 8, 10 https://github.com/ncss-tech/soilDB/issues/54

# soilDB 2.0-4 (2018-03-06)
   * bug fix for subtle change in how SCAN data are returned from webservice

# soilDB 2.0-3 (2018-02-13)
   * bug fix for simplifyFragmentData(, nullFragsAreZero=FALSE), still more work to do

# soilDB 2.0-2 (2018-01-29)
   * bug fix for simplifyFragmentData() when fragment volume > 100%

# soilDB 2.0-1 (2018-01-23)
   * updated `loafercreek` and `gopheridge` sample datasets and manual page to reflect latest fetchNASIS

# soilDB 2.0 (2017-12-23)
   * bug fixes, manual page updates, and stabilization of NASIS interaction
   * released to CRAN

# soilDB 1.8.12 (2017-09-19)
   * bug fix in fetchHenry(), it is now possible to query only sensor metadata

# soilDB 1.8.11 (2017-08-17)
   * temporary bug-fix related to SCAN data (https://github.com/ncss-tech/soilDB/issues/26)

# soilDB 1.8.10 (2017-08-10)
   * re-write of mix_and_clean_colors() so that color mixing happens in CIE LAB space

# soilDB 1.8.5 (2017-05-24)
   * re-write of SDA_query(), better error handling, support for multi-part result sets

# soilDB 1.8.2 (2017-03-24)
   * fetchHenry() can now access water level data, see manual page

# soilDB 1.8.1 (2017-01-23)
   * new function: get_comonth_from_NASIS_db() see manual page for details

# soilDB 1.8-10 (2016-12-27)
   * converting all URLs to 'HTTPS', addressing bugs with fetchSCAN() and associated functions

# soilDB 1.8-7 (2016-11-16)
   * sanity checks for fetchHenry()
   * SCAN_sensor_metadata() vectorized and documented
   * unique row names in SPDF returned by seriesExtent()

# soilDB 1.8-6 (2016-11-04)
   * converted all links to SoilWeb servers to HTTPS
   * converted all NRCS web-service links to HTTPS

# soilDB 1.8-4 (2016-10-07)
   * fetchOSD() now returns dry colors and the source "narrative" for each horizon; function gains an argument
   * fetchOSD() now returns texture class, coarse fragment modifier, pH, and pH class
   * fetchSCAN() re-written, see manual pages and http://ncss-tech.github.io/AQP/soilDB/fetchSCAN-demo.html 

# soilDB 1.8 (2016-04-29)
   * fetchKSSL() can now query basic morphologic data with the argument "returnMorphologicData"
   * two new functions for simplifying NASIS color and fragment data:
      + simplifyFragmentData()
      + simplifyColorData()
   * ^^^ these new functions are now used by fetchNASIS() to summarize color and fragment data (vs. SQL)
   * NOTE: soilDB now uses reshape2::dcast() instead of reshape::cast()

# soilDB 1.7 (2016-04-18)
   * bug fix in colors returned by fetchNASIS(), reported by Andy Paolucci
   * bug fix in SDA_query(), due to changes in httr::content. Thanks Kyle Bocinsky for the fix!
   * SDA_query() now returns SQL errors generated by SDA
   * new columns from NASIS site table returned by fetchNASIS()
   * NASIS pedon ecosite data now returned by fetchNASIS() (c/o J. Skovlin)

# soilDB 1.6.9 (2015-12-28)
   * clean-up in local NASIS queries, removed extra parentheses
   * NASIS component query function overhaul: previous code may be broken, details pending

# soilDB 1.6.8 (2015-12-22)
   * added temperature class (temp_class) field from the pedon taxonomic history table (NASIS) [addition suggested by J. Baker]

# soilDB 1.6.7 (2015-12-16)
   * experimental function for processing SDA queries that return geometry: processSDA_WKT()

# soilDB 1.6.5 (2015-12-03)
   * bug fix for poorly specified geomorphic descriptions, caused fetchNASIS() to barf
   * fetchKSSL() gains new query filters, see man page

# soilDB 1.6.4 (2015-11-27)
   * package re-org, getting ready for soilDB 2.0:
      + dropping use of `RCurl` functions in favor of `httr` alternatives (done)
      + switch from `reshape` to `reshape2` (pending)
      + possibly move some packages from SUGGESTS to IMPORTS

# soilDB 1.6.3 (2015-11-20)
   * new spatial query helper functions for SDA
   * re-named SSURGO_spatial_query() to SoilWeb_spatial_query(), this function may be phased-out due to spatial support now available in SDA
   * removed MUKEYS_by_ll_bbox() function, no longer needed and web-service may be disabled at any time
   * mapunit_geom_by_ll_bbox() will probably be removed in the near future with an SDA-based alternative
   * KSSL data updated (server-side) to June 2015 snapshot, "site_id" column removed from fetchKSSL() results

# soilDB 1.6 (2015-08-19)
   * soilDB now imports from the `reshape` package, will transition to `reshape2` with soilDB 2.0
   * fetchHenry() officially added, complete with documentation
   * SDA_query() undergoing some upgrades, no longer requires SSOAP / XMLSchema packages
   * SSOAP / XMLSchema packages no longer in 'suggests' list

# soilDB 1.5-8 (2015-08-13)
   * fetchKSSL() gets an argument for downloading data by MLRA

# soilDB 1.5-7 (2015-03-10)
   * updated the `loafercreek` and `gopheridge` sample datasets

# soilDB 1.5-3 (2015-03-10)
   * nullFragsAreZero argument to fetchNASIS() reset to TRUE, new documentation pending
   * consolidate of messages printed when running fetchNASIS()
   * introduction of new option "soilDB.verbose" (default is FALSE) for increasing the level of QC information printed
   * new tutorial on fetchNASIS()

# soilDB 1.5-2 (2015-02-24)
   * RODBC temporarily moved from "suggested" package to "imported" package... not a good idea, reverted to "suggested"

# soilDB 1.4-5 (2015-01-20)
   * mix_and_clean_colors() now returns back-transformed, mixed, munsell colors
   * added psctopdepth and pscbotdepth back to extended NASIS query (particle size control section depths)

# soilDB 1.4-3 (2015-01-12)
   * get_hz_data_from_NASIS_db() no longer assumes fragment volume of NULL = 0
   * get_extended_data_from_NASIS_db() no longer assumes fragment volume of NULL = 0
   * fetchNASIS() has a new argument, nullFragsAreZero=FALSE; set to TRUE for past NULL -> 0 conversion

# soilDB 1.4-2 (2014-12-23)
   * new internally-used function .formatLandformString() for flattening NASIS/PedonPC "landform" records
   * new internally-used function .formatParentMaterialString() for flattening NASIS/PedonPC "parent material" records
   * fetchNASIS() incorporates the results of these two new functions
   * new function SSURGO_spatial_query() queries SSURGO data from SoilWeb (see manual page for details)

# soilDB 1.4 (2014-11-13)
   * moving most of the hard-to-install packages into `suggests`, and checking for package availability at function runtime. this will make soilDB more portable, as not every user will want or need all functionality.

# soilDB 1.3-6 (2014-10-27)
   * fetchNASIS() gains argument rmHzErrors to optionally retain pedons with horizonation errors. use with caution.

# soilDB 1.3-5 (2014-09-25)
   * loafercreek sample dataset re-created from new data
   * seriesExtent() now utilizes pre-cached GeoJSON files from [http://casoilresource.lawr.ucdavis.edu/see/]

# soilDB 1.3 (2014-02-26)
   * Changes made to fetch functions to accommodate changes implemented in the NASIS 6.3 data structure
   * get_site_data_from_NASIS_db(): in cases where multiple records in site-bedrock exist, retain only the most shallow
   * fetchRaCA() has a new argument for querying data by rcasiteid

# soilDB 1.2-9 (2014-01-06)
   * SOC concentration and stock values are temporarily disabled in fetchRaCA() 
      + waiting for new estimates from the Soil Survey Center...

# soilDB 1.2-6 (2013-12-05)
   * added new function contributed by J.M. Skovlin: get_veg_from_NPS_PLOTS_db()
   * bug fixes in get_extended_data_from_NASIS_db(): results from geomorph tables are now returned

# soilDB 1.2-3 (2013-10-25)
   * added new function fetchSCAN() for downloading soil/climate data from USDA-NRCS SCAN stations (still experimental!)

# soilDB 1.2-1 (2013-09-12)
   * bug fixing in fetchRaCA(): 
      + "#" characters no longer cause errors
      + duplicate IDs in source data have been fixed

# soilDB 1.2 (2013-08-13)
   * fetchRaCA() function now stable, ready for general usage
   * fixed bug in fetchRaCA() function where the presence of single-quote character in horizon designation would throw and error

# soilDB 1.1-3 (2013-07-30)
   * new function get_copedon_from_NASIS_db(): returns basic information associated with pedons linked to component data in NASIS
   * new function fetchRaCA(): gets data from the Rapid Carbon Assessment project [work in progress]

# soilDB 1.1 (2013-06-27)
   * get_site_data_from_NASIS_db() no longer has problems when more than 1 site bedrock kind is defined; only the shallowest row is returned

# soilDB 1.0 (2013-04-04)
   * fetchNASIS() is now much more efficient: faster / less memory used
   * new function! fetchKSSL(): experimental interface to (most of) the KSSL data
      + queries can be performed by series name or GCS bounding box
      + data are delivered via CA Soil Resource Lab server

# soilDB 0.9-8 (2013-04-03)
   * fetchOSD() now immune to queries for non-soils
   * improved efficiency in fetch* functions
   * NASIS-DMU fetching functions now return more information, still not as complete as pedon-queries

# soilDB 0.9-7 (2013-03-01)
   * NASIS query functions now return only records from the selected set, please test older code!

# soilDB 0.9-6 (2013-02-26)
   * new function get_text_notes_from_NASIS_db() -- still needs proper documentation
   * bug fixes to PedonPC and NASIS queries, new data elements returned by extended query functions
   * SoilWeb queries via URL are now URL-encoded to allow for spaces in soil series names

# soilDB 0.9-4 (2013-01-31)
   * new function: fetchOSD() returns basic OSD data as a SoilProfileCollection

# soilDB 0.9-3 (2013-01-08)
   * speed-bump in color-fetching functions from NASIS/pedonPC
   * fetchNASIS() now attempts to correct for >1 pedon/site by appending peiid to pedon_id

# soilDB 0.9-2 (2012-12-18)
   * new functions for getting / plotting the geographic extent of a soil series, using the SoilWeb query service:
      +seriesExtent() : fetches extent as a SpatialPolygonDataFrame
      +seriesExtentAsGmap() : fetches extent and plots on Google Maps (requires dismo package)

# soilDB 0.9-1 (2012-11-27)
   * updated functionality in fetchNASIS_component_data() pulls mapunit-level data as well as component-level data
   * Munsell 'value' is now returned from PedonPC/NASIS fetching functions

# soilDB 0.9 (2012-10-24)
   * added a surface fragment summary "surf_frag_summary" to the results of get_extended_data_from_NASIS()
      + note: this has not been ported to the related PedonPC queries
   * integrated surf_frag_summary data into data returned from fetchNASIS(), stored in @site
      + note: this has not been ported to the related PedonPC queries
   * minor bug-fix in horizon-level rock fragment summary SQL which previously mis-classified BD as ST in some, rare cases
      + note: this has not been ported to the related PedonPC queries

# soilDB 0.8-3 (2012-09-28)
   * site-level data: {elev, slope, aspect} are now named {elev_field, slope_field, aspect_field}
      + this affects the data returned by fetchNASIS() and fetchPedonPC()

# soilDB 0.8-2 (2012-08-23)
   * compatible with NASIS 6.2
   * bug fixes
   * taxhistory selection method is preserved in the results
   * 'taxonname' is used instead of 'sampled_as' / 'correlated_as, **seems to work, but needs further testing**

# soilDB 0.8 (2012-08-20)
   * updating for PedonPC 5.0 and NASIS 6.2
      + site data contains the best-guessed corresponding row from the taxhistory table, based on:
		1) most recent record, or 2) record with the least amount of missing data
      + taxhistory data are now included in the output from get_extended_data_from_pedon_db()
   * package is now partially compatible with NASIS 6.2

# soilDB 0.6 (2012-06-19)
   * tidying up documentation, package dependencies, and NAMESPACE

# soilDB 0.5-6 (2012-04-16)
   * adding preliminary functions for querying component data from local NASIS
   * fixed minor bug in SDA_query() and added some links to related documentation

# soilDB 0.5-1 (2012-02-22)
   * fetchNASIS() and fetchPedonPC() now integrate `extended' data
   * warning and error messages cleaned-up
   * multiple textures no longer cause duplicate HZ rows (NASIS only)
   * extended queries now split frags/para-frags
   * silt fraction is estimated from 100-(sand+clay) when possible
   * added local NASIS ODBC connection vignette (thanks JMS)
   * removed dsn argument from NASIS functions as it should always be 'nasis_local'
   * new wide-formatted, boolean representation of diagnostic horizon data
   * queries standardized between NASIS/PedonPC
   * new fetchNASIS() function for 1-line access to local NASIS data
   * new sample data set 'loafercreek'
   * basic vignette added, switching to knitr-style vignettes

# soilDB 0.3-3 (2012-01-10)
   * SDA_query() now functional on MacOS/UNIX-like OSes with (SSOAP 0.8-1, XMLSchema 0.6-0, and XML 3.7-1), thanks to D.T. Lang for the updates!
   * moving hard to find packages to 'suggested' status: SSOAP, RCurl, XML, rgdal
   * new wrapper function fetchPedonPC() for typical site/pedon/hz queries
   * new function getHzErrorsPedonPC() for ID-ing pedons with problem horizonation
   * NOTE: hz data queries will return 2 rows/hz (error) when multiple texture 
  classes are assigned to a single horizon- this is a bug, not a feature!

# soilDB 0.3-2 (2011-12-30)
   * NASIS/pedonPC queries synced
   * minor bug fixes and documentation updates
   * updated query structure, switching over to native NASIS/PedonPC IDs
   * extended summaries added: NASIS only

# soilDB 0.2 (2011-12-27)
   * moved functions out of aqp package
   * basic query functionality from local NASIS DB (Jay)

# soilDB 0.1 (2011-12-23)
   * initial version on r-forge
   * functions still exist in aqp package... need to move over completely

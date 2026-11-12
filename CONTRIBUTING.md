# Contributing to soilDB

Internal architecture, existing patterns, and guidance for adding features or fixing bugs in `soilDB`.

---

## 1. Quick File Map

Find code fast by functional area:

| Task / Subsystem | Primary Files | What Lives Here |
| :--- | :--- | :--- |
| **Package setup & handles** | [`R/AAAA.R`](R/AAAA.R) | Package environment (`soilDB.env`), `.onLoad` options, curl handles. |
| **Local NASIS connections** | [`R/openNASISchannel.R`](R/openNASISchannel.R), [`R/dbQueryNASIS.R`](R/dbQueryNASIS.R) | ODBC / SQLite switching, connection lifecycle, auto-disconnect. |
| **NASIS Queries & Area Joins** | [`R/get_site_data_from_NASIS_db.R`](R/get_site_data_from_NASIS_db.R), [`R/get_soilseries_from_NASIS.R`](R/get_soilseries_from_NASIS.R), [`R/get_mapunit_from_NASIS.R`](R/get_mapunit_from_NASIS.R) | SQL table queries, Selected Set views, Area relationship joins. |
| **Soil Data Access (SDA)** | [`R/SDA_query.R`](R/SDA_query.R), [`R/get_component_from_SDA.R`](R/get_component_from_SDA.R), [`R/SDA-spatial.R`](R/SDA-spatial.R), [`R/fetchSDA_spatial.R`](R/fetchSDA_spatial.R) | SDA REST endpoint, chunking, SQL parsing, spatial queries. |
| **Metadata & domain decoders** | [`R/uncode.R`](R/uncode.R), [`data/metadata.rda`](data/metadata.rda) | NASIS and SSURGO code/uncode mappings, domain caching. |
| **Data simplifiers** | [`R/simplifyFragmentData.R`](R/simplifyFragmentData.R), [`R/simplifyColorData.R`](R/simplifyColorData.R), [`R/simplifyArtifactData.R`](R/simplifyArtifactData.R) | Sieves, Munsell CIELAB color mixing, artifact classification. |
| **SoilProfileCollection building** | [`R/fetchNASIS_pedons.R`](R/fetchNASIS_pedons.R), [`R/fetchKSSL.R`](R/fetchKSSL.R), [`R/fetchOSD.R`](R/fetchOSD.R) | `depths<-`, `site<-`, horizon depth error cleaning. |
| **Shared utilities & string formats** | [`R/utils.R`](R/utils.R) | `.gluelite()`, `.SDA_comment_header()`, landform formatters. |
| **Web Coverage Services (WCS)** | [`R/mukey-WCS.R`](R/mukey-WCS.R), [`R/WCS-utils.R`](R/WCS-utils.R) | Raster grid queries (gSSURGO, gNATSGO, ISSR-800). |
| **File caches** | [`R/soilDB_user_dir.R`](R/soilDB_user_dir.R) | Standard user data and cache folder management. |
| **Test suite** | [`tests/testthat/`](tests/testthat/) | Defensive test rules (`skip_if_offline`, `skip_on_cran`). |

---

## 2. Package Setup, Options, Environment Variables, and Network Handles

### Package Environment (`soilDB.env`)

[`R/AAAA.R`](R/AAAA.R) creates an internal hash environment:

```r
soilDB.env <- new.env(hash = TRUE)
```

Use this environment to cache session data (such as live NASIS domain tables via `soilDB.env$NASIS.metadata`) and track warning states (such as `soilDB.env$soilDB.warn.aliases_fnlist`).

### Package Options

Defaults are set in `.onLoad()` ([`R/AAAA.R`](R/AAAA.R)):

- `soilDB.verbose`: Print debug and progress logs. Default: `FALSE`.
- `soilDB.timeout`: Timeout in seconds for curl handles. Default: `300`.
- `soilDB.ssl_verifyhost`: Host verification option initialized by `.onLoad()`. Default: `0`. Note that `.soilDB_curl_handle()` currently reads `soilDB.verify_host` instead.
- `soilDB.warn.aliases`: Show column alias deprecation messages. Default: `TRUE`.
- `soilDB.NASIS.credentials`: Default ODBC connection string for local SQL Server Express.
- `soilDB.NASIS.skip_uncode`: Skip [`uncode()`](R/uncode.R) calls when source tables are already decoded. Default: `FALSE`.
- `soilDB.NASIS.DomainsAsFactor`: Return decoded columns as R factors instead of character strings. Default: `FALSE`.
- `soilDB.return_Spatial`: Return legacy `sp::Spatial*` objects instead of `sf`. Default: `FALSE`.

### Environment Variables

`soilDB` inspects specific environment variables via `Sys.getenv()`:

- `R_SOILDB_SSURGO_TABLE_WRITE_BATCH_SIZE`: Number of tables written per SQLite transaction in [`R/createSSURGO.R`](R/createSSURGO.R). Default: `"25"`.
- `R_SOILDB_SSURGO_ROW_CHUNK_SIZE`: Number of tabular rows streamed per read/write chunk in [`R/createSSURGO.R`](R/createSSURGO.R). Default: `"100000"`.
- `R_SOILDB_SKIP_LONG_EXAMPLES`: Controls execution of long example blocks during checks ([`R/fetchKSSL.R`](R/fetchKSSL.R), [`R/get_SDA_coecoclass.R`](R/get_SDA_coecoclass.R)).
- `soilDB_user_dir()` in [`R/soilDB_user_dir.R`](R/soilDB_user_dir.R) wraps `tools::R_user_dir("soilDB", ...)` to manage package data, config, and cache directories.

### HTTP Requests and Downloads

Use the shared handle helper in [`R/AAAA.R`](R/AAAA.R) for curl requests:

- `.soilDB_curl_handle(...)`: Returns a handle with the package timeout and host-verification settings.
- `.soilDB_curl_get_JSON(x, gzip = FALSE)`: Downloads a JSON endpoint to a temporary file and parses it with `jsonlite::fromJSON`; when `gzip = TRUE`, it reads the downloaded file as gzip-compressed data.

Other downloads use `curl::curl_download()` with `.soilDB_curl_handle()` so they share the package's request settings.

---

## 3. Database Connections (NASIS, SQLite, DBI)

### How Connections Work

`soilDB` connects to local MS SQL Server instances (via `odbc`), SQLite files (via `RSQLite`), or user-supplied `DBIConnection` objects.

[`R/dbQueryNASIS.R`](R/dbQueryNASIS.R) tracks connection ownership:

1. When `dbConnectNASIS(dsn = NULL)` opens an ODBC channel, it sets `attr(conn, 'isUserDefined') <- FALSE`.
2. When a user passes an existing connection object, it sets `attr(conn, 'isUserDefined') <- TRUE`.
3. `dbQueryNASIS(conn, q, close = TRUE)` runs queries. On exit, it checks `attr(conn, 'isUserDefined')`. It closes managed connections and leaves user-supplied connections open.

### NASIS Selected Set vs. Local Database

In local NASIS, selected set records live in views named `*_View_1` (e.g. `pedon_View_1`). When `SS = FALSE`, queries must strip the suffix to query the full database. All functions targeting the NASIS database should, in general, have the ability to toggle selected set behavior (though default argument value may vary as needed).

```r
# R/get_site_data_from_NASIS_db.R
if (isFALSE(SS)) {
  q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
}
```

### Area Table Relationships and `areasymbol` Naming

Per #272, when joining NASIS `area` table records via foreign keys, alias derived columns to match the **NASIS relationship name** (`<table_or_alias>_<relationship>`). Avoid bare `areasymbol` or custom table prefixes:

- **Site Area Overlaps** ([`R/get_site_data_from_NASIS_db.R`](R/get_site_data_from_NASIS_db.R)):
  - `area.areasymbol AS site_state` (joined on `site_View_1.stateareaiidref`)
  - `area.areasymbol AS site_county` (joined on `site_View_1.countyareaiidref`)
  - `area.areasymbol AS site_mlra` (joined on `site_View_1.mlraareaiidref`)
- **Soil Series Type Locations** ([`R/get_soilseries_from_NASIS.R`](R/get_soilseries_from_NASIS.R)):
  - `areasymbol AS soilseries_typelocst` (joined on `soilseries_View_1.typelocstareaiidref`)
- **Mapunit Dominant MLRA** ([`R/get_mapunit_from_NASIS.R`](R/get_mapunit_from_NASIS.R)):
  - `area.areasymbol AS lmapunit_mlra` (joined on `lmapunit_View_1.mlraareaiidref`)
- **Designated Exception**: `legend.areasymbol` remains `areasymbol` to preserve backward compatibility.

### Adding or Updating NASIS Queries

1. Place SQL queries in `R/get_*_from_NASIS_db.R` or `R/get_*_from_NASIS.R`.
2. Open channels with `dbConnectNASIS(dsn = dsn)` ([`R/dbQueryNASIS.R`](R/dbQueryNASIS.R)).
3. Run queries using `dbQueryNASIS(conn, q)` ([`R/dbQueryNASIS.R`](R/dbQueryNASIS.R)).
4. Wrap results with `uncode(df, dsn = dsn)` ([`R/uncode.R`](R/uncode.R)) to convert database integers to human-readable strings.

---

## 4. Soil Data Access (SDA)

### Running SDA Queries

SDA accepts SQL queries sent via HTTP POST to `https://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest`.

[`R/SDA_query.R`](R/SDA_query.R) handles transport:

- Automatically inserts telemetry headers using `.SDA_comment_header(function_name)` from [`R/utils.R`](R/utils.R).
- Checks that query strings do not exceed SDA's 2.5 MB payload limit.
- Parses the 2-row metadata JSON response and casts columns to their native R types (`.post_process_SDA_result_set()`).

### Query Templating Helpers

Use internal string helpers in [`R/utils.R`](R/utils.R) and [`R/SDA_query.R`](R/SDA_query.R) when constructing queries:

- **`.gluelite(x, env = parent.frame())`** ([`R/utils.R`](R/utils.R)): Replaces `{variable}` tokens using base R without external dependencies.
  ```r
  # R/get_SDA_NASIS_keys.R
  q <- .gluelite(
    "SELECT component.mukey, component.cokey, nasiscoiid {INCLUDE_CHIID}
     FROM component {INCLUDE_CHORIZON}
     WHERE {WHERE}"
  )
  ```
- **`.SDA_comment_header(function_name)`** ([`R/utils.R`](R/utils.R)): Generates telemetry comment blocks (`/** SDA Query application='soilDB' rule='...' version='...' **/`) prepended to SDA queries.
- **`format_SQL_in_statement(x)`** ([`R/SDA_query.R`](R/SDA_query.R)): Deduplicates and wraps vectors into SQL `IN ('val1', 'val2')` clauses.

### Chunking Large Queries

When querying by ID lists (such as `mukey`, `cokey`, or `areasymbol`), batch queries using `makeChunks()` from [`R/SDA_query.R`](R/SDA_query.R):

```r
chunks <- makeChunks(ids, size = 100)
for (chunk in unique(chunks)) {
  id_subset <- ids[chunks == chunk]
  in_clause <- format_SQL_in_statement(id_subset)
  # run query with in_clause...
}
```

---

## 5. Metadata Decoding and Simplifiers

### `uncode()` and Domain Caching

NASIS stores lookup values as integers or codes. `uncode()` in [`R/uncode.R`](R/uncode.R) maps these values to labels:

1. If connected to a local NASIS database, it queries `MetadataDomainDetail` and caches the result in `soilDB.env$NASIS.metadata`.
2. If offline or no database exists, it falls back to the embedded snapshot in [`data/metadata.rda`](data/metadata.rda).
3. Set `options(soilDB.NASIS.DomainsAsFactor = TRUE)` to get factors, or `FALSE` for character strings.

### Reusable Data Simplifiers

Do not write custom rock fragment, artifact, or color aggregation logic. Use the existing modules:

- `simplifyFragmentData()` ([`R/simplifyFragmentData.R`](R/simplifyFragmentData.R)): Sieves rock fragments into standard size classes (gravel, cobbles, stones, boulders).
- `simplifyArtifactData()` ([`R/simplifyArtifactData.R`](R/simplifyArtifactData.R)): Classifies human artifacts into size and shape classes.
- `simplifyColorData()` ([`R/simplifyColorData.R`](R/simplifyColorData.R)): Converts Munsell colors to CIELAB, separates dry and moist states, and mixes multi-color horizons by percent area.

---

## 6. Building SoilProfileCollection Objects

Functions returning profile data ([`fetchNASIS`](R/fetchNASIS.R), [`fetchSDA`](R/get_component_from_SDA.R), [`fetchKSSL`](R/fetchKSSL.R), [`fetchOSD`](R/fetchOSD.R)) assemble `aqp::SoilProfileCollection` objects in four steps:

### 1. Depth Cleaning

Fix missing or zero-thickness depths before promotion:

```r
# Missing bottom depth: add 1 cm
missing_b <- which(!is.na(hz$hzdept) & is.na(hz$hzdepb))
hz$hzdepb[missing_b] <- hz$hzdept[missing_b] + 1

# Top equals bottom: add 1 cm
same_d <- which(hz$hzdept == hz$hzdepb)
hz$hzdepb[same_d] <- hz$hzdepb[same_d] + 1
```

### 2. Promote to SoilProfileCollection

Set unique IDs as characters and promote with formula syntax:

```r
hz$peiid <- as.character(hz$peiid)
hz$phiid <- as.character(hz$phiid)
aqp::depths(hz) <- peiid ~ hzdept + hzdepb
```

### 3. Join Site and Diagnostic Tables

Join tables using `aqp` replacement methods:

```r
# Site attributes
site$upedonid <- NULL # Remove duplicate merge keys
aqp::site(hz) <- site

# Diagnostic features and restrictions
aqp::diagnostic_hz(hz) <- diagnostic_data
aqp::restrictions(hz) <- restriction_data
```

### 4. Register Coordinates

Register spatial coordinates when present:

```r
aqp::initSpatial(hz) <- ~ longitude_std_decimal_degrees + latitude_std_decimal_degrees
```

---

## 7. Writing Tests

All test files live in [`tests/testthat/`](tests/testthat/). Every test must run cleanly during `R CMD check` without internet access or local NASIS databases.

### Rules for New Tests

1. **Web service tests**: Add `skip_if_offline()` and `skip_on_cran()` at the top of the test file ([`tests/testthat/test-SDA_query.R`](tests/testthat/test-SDA_query.R)).
2. **Network error protection**: Wrap network calls in `try()` and skip if the endpoint failed:
   ```r
   res <- try(fetchOSD("sierra"), silent = TRUE)
   skip_if(inherits(res, "try-error") || is.null(res))
   expect_s4_class(res, "SoilProfileCollection")
   ```
3. **Local NASIS tests**: Guard with `if (!local_NASIS_defined(dsn = dsn)) skip("NASIS not available")` ([`tests/testthat/test-fetchNASIS.R`](tests/testthat/test-fetchNASIS.R)).
4. **Temporary file cleanup**: Delete any created test files inside an `on.exit(unlink(tf))` block.
5. **Pure functions**: Write fast unit tests for decoders, sieves, and string formatters using small in-memory data frames ([`tests/testthat/test-uncode.R`](tests/testthat/test-uncode.R), [`tests/testthat/test-simplifyFragmentData.R`](tests/testthat/test-simplifyFragmentData.R)).

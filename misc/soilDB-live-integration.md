# Live soilDB integration suite

Run from the repository root with:

```powershell
$env:SOILDB_RUN_LIVE_INTEGRATION = "1"
Rscript misc\soilDB-live-integration.R
```

The runner requires internet access and the `soilDB`, `DBI`, `RSQLite`, `sf`,
`terra`, and `duckdb` packages. It downloads live SSURGO data, creates
temporary SQLite, GeoPackage, and DuckDB outputs, exercises the intentional
`legacy_gssurgo`, `target_aligned`, and `nlcd` alignment modes, and removes all
temporary files when it exits.

Raster output defaults to a 10 m grid. Standard 30 m, 300 m, and 900 m grids
are selected by appending `_30m`, `_300m`, or `_900m` to an alignment mode, for
example `grid_alignment = "legacy_gssurgo_30m"`. An arbitrary `terra::SpatRaster`
can be supplied through `template`; when supplied, it takes precedence over
the named standard grid. Cached SSA rasters include the selected resolution in
their filenames.

Failures are fatal. The output names the scenario that failed and includes
the underlying error message.

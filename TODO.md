# TODO

## Goal
Make the `createSSURGO()` MUKEY raster workflow release-ready by supporting documented standard and custom grids, preserving intentional alignment behavior, removing correctness issues, and validating the result.

## Tasks

### 1. Define and document the raster grid contract
- [x] Inspect the current raster arguments and helpers and map each alignment mode to its supported resolution/template behavior.
- [x] Add documented named support for 10 m, 30 m, 300 m, and 900 m grids while preserving arbitrary user-supplied templates and the 10 m default.
- [x] Ensure generated raster filenames include the selected grid resolution.
- [x] Document the intentional `legacy_gssurgo`, `target_aligned`, and `nlcd` alignment policies in `man/createSSURGO.Rd` and `misc/soilDB-live-integration.md`.
- [x] Add a `NEWS.md` entry describing MUKEY raster output, grid choices, custom templates, and alignment modes.

### 2. Correct rasterization behavior
- [ ] Make raster generation work when `include_spatial` excludes `mupolygon`, without requiring vector database output beyond the actual rasterization prerequisites.
- [ ] Ensure custom-template and named-grid settings are preserved through per-SSA rasterization, mosaicking, cropping, and final output.
- [ ] Verify and fix final mosaic extent/alignment so it matches the intended snapped vector/template extent for every alignment mode.
- [ ] Preserve `NAflag = 0` and document that MUKEY zero is reserved as NoData.

### 3. Consolidate implementation without changing supported hooks
- [ ] Extract shared grid normalization/origin and COG-writing configuration where duplication is real.
- [ ] Preserve `LAPPLY.FUN`, `LAPPLY.FUN.ARGS`, and `.RASTERIZE.FUN` because they are established parallel-worker contracts.
- [ ] Replace opaque positional schema handling only where a named, behavior-preserving improvement is possible.

### 4. Strengthen focused tests
- [ ] Add tests for all named grid resolutions, default behavior, resolution-bearing filenames, custom templates, and each intentional alignment mode.
- [ ] Add coverage for rasterization with vector database output disabled.
- [ ] Add assertions for final extent/alignment, datatype, NoData, compression, tiling, and COG-related metadata.
- [ ] Consolidate only genuinely shared raster test fixtures and retain guards for optional spatial/raster packages.

### 5. Validate release readiness
- [ ] Run the focused `createSSURGO` test file and relevant package checks.
- [ ] Run the live integration runner when required dependencies and service access are available.
- [ ] Review the final diff for dead code, duplicated logic, tautological checks, stale documentation, and unrelated changes.

## Notes
- Governing feature request: ncss-tech/soilDB#486.
- Default grid remains 10 m.
- Standard grids are exposed through named modes rather than a new numeric public argument.
- `NAflag = 0` remains intentional because MUKEY zero is not a valid MUKEY.
- Scope includes `R/createSSURGO.R`, generated/reference documentation, focused tests, `misc/soilDB-live-integration.md`, `NEWS.md`, and directly dependent metadata only.

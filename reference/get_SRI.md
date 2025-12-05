# Get Soil Inventory Resource (SRI) for USFS Region 6

This function calls ECOSHARE (zip files) to get Soil Inventory Resource
(SRI) data for USFS Region 6. These datasets contain both spatial and
non-spatial data in the form of a File Geodatabase (GDB).

## Usage

``` r
get_SRI(gdb, layers = "MapUnits", quiet = FALSE, simplify = TRUE)
```

## Arguments

- gdb:

  A `character` of the GDB, e.g. `'Deschutes'`.

- layers:

  A `character` of the layer(s) within the GDB, e.g. `'MapUnits'`
  (default).

- quiet:

  A `logical`; suppress info on name, driver, size and spatial
  reference, or signaling no or multiple layers.

- simplify:

  A `logical`; whether to return a simplified list (`data.frame` or
  `sf`) if length(layers) == 1.

## Value

An `sf` or `data.frame` object.

## Details

Due to the fact that many Region 6 Forests do not have NRCS SSURGO
surveys (at a scale of 1:24,000, these are the highest-resolution soils
data generally available), Region 6 initiated a project in 2012 to bring
these legacy SRI soils data into digital databases to facilitate their
use in regional planning activities. The datasets available on this page
are the results of that effort.

The SRI were originally compiled in 20 volumes, with the original year
of publication ranging from 1969 to 1979. The Gifford-Pinchot SRI was
redone following the eruption of Mt Saint Helens, and that version was
published in 1992. The Olympic NF also produced two versions, the
original version being published in 1969, with an update in 1982. The
Colville National Forest was the only Region 6 forest that did not
compile a SRI.

The data are organized into one single regional GDB, together with
twenty individual forest-level GDBs. The regional database contains
polygons from all twenty SRIs together with a common set of attributes
for the two or three soil layers delineated in the individual mapping
unit descriptions, such as texture, depth, color, rock content, etc. In
general, the regional database contains physical soil attributes that
could be compiled more or less completely and consistently across all
forests. The individual forest-level databases contain the polygons for
each individual SRI, together with various tables of management
interpretations and laboratory data, together with a variety of
miscellaneous tables. The information contained in these forest-level
databases varies widely from forest to forest, which is why they were
not merged into a regional view. Full metadata are included with each
database, and scans of the original SRI volumes are provided for
reference as well. A Forest Service General Technical Report that fully
describes the available data is currently in preparation.

The GDB's currently available:

- **Region6**

- **Deschutes**

- **Fremont**

- **GiffordPinchot**

- **Malheur**

- **MtBaker**

- **MtHood**

- **Ochoco**

- **Okanogan**

- **Olympic**

- **RogueRiver**

- **Siskiyou**

- **Siuslaw**

- **Umatilla**

- **Umpqua**

- **WallowaWhitman**

- **Wenatchee**

- **Willamette**

- **Winema**

## Note

Please use
[`get_SRI_layers`](http://ncss-tech.github.io/soilDB/reference/get_SRI_layers.md)
to get the layer id information needed for the layer argument. This will
help with joining `sf` and `data.frame` objects.

## See also

[`get_SRI_layers()`](http://ncss-tech.github.io/soilDB/reference/get_SRI_layers.md)

## Author

Josh Erickson

## Examples

``` r
if (FALSE) { # \dontrun{

# get Deschutes SRI
sri_deschutes <- get_SRI('Deschutes')

# get multiple layers in a list

sri_deschutes_multiple <- get_SRI(gdb = 'Deschutes',
layers = c('MapUnits', 'ErosionAndHydro', 'SampleSites_MaterialsTesting'))

} # }

```

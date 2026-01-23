# Get Rapid Carbon Assessment (RaCA) data

**NOTICE:** The SoilWeb snapshot of the RaCA data has been deprecated.
The latest version of the data, including values measured by the Kellogg
Soil Survey Laboratory, and supporting documentation, are available
here:
<https://www.nrcs.usda.gov/resources/data-and-reports/rapid-carbon-assessment-raca>.

Please use current RaCA distribution if you need *lab measured* SOC
rather than SOC estimated by VNIR.

Download link on NRCS Soils Box site:
https://nrcs.app.box.com/s/upx5xhlwis7saunfiysclfrhl5vxxudn

Gets Rapid Carbon Assessment (RaCA) data by state, geographic
bounding-box, RaCA site ID, or soil series query from the SoilWeb API.
This interface to the data was an experimental delivery service that
does not include the latest soil organic carbon (SOC) measurements.

## Usage

``` r
fetchRaCA(
  series = NULL,
  bbox = NULL,
  state = NULL,
  rcasiteid = NULL,
  get.vnir = FALSE
)
```

## Arguments

- series:

  a soil series name; case-insensitive

- bbox:

  a bounding box in WGS84 geographic coordinates e.g.
  `c(-120, 37, -122, 38)`, constrained to a 5-degree block

- state:

  a two-letter US state abbreviation; case-insensitive

- rcasiteid:

  a RaCA site id (e.g. 'C1609C01')

- get.vnir:

  logical, should associated VNIR spectra be downloaded? (see details)

## Value

- `pedons`::

  a `SoilProfileCollection` object containing site/pedon/horizon data

- `trees`::

  a `data.frame` object containing tree DBH and height

- `veg`::

  a `data.frame` object containing plant species

- `stock`::

  a `data.frame` object containing carbon quantities (stocks) at
  standardized depths

- `sample`::

  a `data.frame` object containing sample-level bulk density and soil
  organic carbon values

- `spectra`::

  a numeric `matrix` containing VNIR reflectance spectra from 350–2500
  nm

## Details

The VNIR spectra associated with RaCA data are quite large (each
gzip-compressed VNIR spectra record is about 6.6kb), so requests for
these data are disabled by default. Note that VNIR spectra can only be
queried by soil series or geographic BBOX.

## References

USDA Natural Resources Conservation Service (2018). Rapid Carbon
Assessment (RaCA). United States Department of Agriculture. Dataset.
https://hdl.handle.net/10113/AA21139

## See also

[`fetchOSD`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)

## Author

D.E. Beaudette, USDA-NRCS staff

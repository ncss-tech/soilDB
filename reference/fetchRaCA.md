# Get Rapid Carbon Assessment (RaCA) data

**NOTICE:** The SoilWeb snapshot of the RaCA data has been deprecated.
The latest version of the data, including values measured by the Kellogg
Soil Survey Laboratory, and supporting documentation, are available
here:
<https://www.nrcs.usda.gov/resources/data-and-reports/rapid-carbon-assessment-raca>.
Download link on National Agricultural Library Ag Data Commons:
<https://data.nal.usda.gov/dataset/rapid-carbon-assessment-raca>

Get Rapid Carbon Assessment (RaCA) data by state, geographic
bounding-box, RaCA site ID, or soil series query from the SoilWeb API.
This interface to the data was an experimental delivery service that
does not include the latest soil organic carbon (SOC) measurements.

Please use [current RaCA
distribution](https://data.nal.usda.gov/dataset/rapid-carbon-assessment-raca)
if you need lab *measured* SOC rather than SOC estimated by VNIR.

This interface will be updated sometime calendar year 2022 to include
the latest soil morphology, taxonomic classification, and measured SOC
values. More detailed coordinates for sample sites should also be
available.

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

<https://data.nal.usda.gov/dataset/rapid-carbon-assessment-raca>

## See also

[`fetchOSD`](http://ncss-tech.github.io/soilDB/reference/fetchOSD.md)

## Author

D.E. Beaudette, USDA-NRCS staff

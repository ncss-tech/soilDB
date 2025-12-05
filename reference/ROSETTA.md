# Query USDA-ARS ROSETTA Model API

A simple interface to the [ROSETTA
model](https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-model/)
for predicting hydraulic parameters from soil properties. The ROSETTA
API was developed by Dr. Todd Skaggs (USDA-ARS) and links to the work of
Zhang and Schaap, (2017). See the [related
tutorial](http://ncss-tech.github.io/AQP/soilDB/ROSETTA-API.md) for
additional examples.

## Usage

``` r
ROSETTA(
  x,
  vars,
  v = c("1", "2", "3"),
  include.sd = FALSE,
  chunkSize = 10000,
  conf = NULL
)
```

## Arguments

- x:

  a `data.frame` of required soil properties, may contain other columns,
  see details

- vars:

  character vector of column names in `x` containing relevant soil
  property values, see details

- v:

  ROSETTA model version number: '1', '2', or '3', see details and
  references.

- include.sd:

  logical, include bootstrap standard deviation for estimated parameters

- chunkSize:

  number of records per API call

- conf:

  configuration passed to
  [`httr::POST()`](https://httr.r-lib.org/reference/POST.html) such as
  `verbose()`.

## Details

Soil properties supplied in `x` must be described, in order, via `vars`
argument. The API does not use the names but column ordering must
follow: sand, silt, clay, bulk density, volumetric water content at
33kPa (1/3 bar), and volumetric water content at 1500kPa (15 bar).

The ROSETTA model relies on a minimum of 3 soil properties, with
increasing (expected) accuracy as additional properties are included:

- required, sand, silt, clay: USDA soil texture separates (percentages)
  that sum to 100 percent

- optional, bulk density (any moisture basis): mass per volume after
  accounting for \>2mm fragments, units of gm/cm3

- optional, volumetric water content at 33 kPa: roughly "field capacity"
  for most soils, units of cm^3/cm^3

- optional, volumetric water content at 1500 kPa: roughly "permanent
  wilting point" for most plants, units of cm^3/cm^3

The Rosetta pedotransfer function predicts five parameters for the van
Genuchten model of unsaturated soil hydraulic properties

- theta_r : residual volumetric water content

- theta_s : saturated volumetric water content

- log10(alpha) : retention shape parameter `[log10(1/cm)]`

- log10(npar) : retention shape parameter

- log10(ksat) : saturated hydraulic conductivity `[log10(cm/d)]`

Column names not specified in `vars` are retained in the output.

Three versions of the ROSETTA model are available, selected using "v =
1", "v = 2", or "v = 3".

- version 1 - Schaap, M.G., F.J. Leij, and M.Th. van Genuchten. 2001.
  ROSETTA: a computer program for estimating soil hydraulic parameters
  with hierarchical pedotransfer functions. Journal of Hydrology
  251(3-4): 163-176. doi:
  [doi:10.1016/S0022-1694(01)00466-8](https://doi.org/10.1016/S0022-1694%2801%2900466-8)
  .

- version 2 - Schaap, M.G., A. Nemes, and M.T. van Genuchten. 2004.
  Comparison of Models for Indirect Estimation of Water Retention and
  Available Water in Surface Soils. Vadose Zone Journal 3(4): 1455-1463.
  doi: [doi:10.2136/vzj2004.1455](https://doi.org/10.2136/vzj2004.1455)
  .

- version 3 - Zhang, Y., and M.G. Schaap. 2017. Weighted recalibration
  of the Rosetta pedotransfer model with improved estimates of hydraulic
  parameter distributions and summary statistics (Rosetta3). Journal of
  Hydrology 547: 39-53. doi:
  [doi:10.1016/j.jhydrol.2017.01.004](https://doi.org/10.1016/j.jhydrol.2017.01.004)
  .

## References

Consider using the interactive version, with copy/paste functionality
at: <https://www.handbook60.org/rosetta>.

Rosetta Model Home Page:
<https://www.ars.usda.gov/pacific-west-area/riverside-ca/agricultural-water-efficiency-and-salinity-research-unit/docs/model/rosetta-model/>.

Python ROSETTA model: <https://pypi.org/project/rosetta-soil/>.

Yonggen Zhang, Marcel G. Schaap. 2017. Weighted recalibration of the
Rosetta pedotransfer model with improved estimates of hydraulic
parameter distributions and summary statistics (Rosetta3). Journal of
Hydrology. 547: 39-53.
[doi:10.1016/j.jhydrol.2017.01.004](https://doi.org/10.1016/j.jhydrol.2017.01.004)
.

Kosugi, K. 1999. General model for unsaturated hydraulic conductivity
for soils with lognormal pore-size distribution. Soil Sci. Soc. Am. J.
63:270-277.

Mualem, Y. 1976. A new model predicting the hydraulic conductivity of
unsaturated porous media. Water Resour. Res. 12:513-522.

Schaap, M.G. and W. Bouten. 1996. Modeling water retention curves of
sandy soils using neural networks. Water Resour. Res. 32:3033-3040.

Schaap, M.G., Leij F.J. and van Genuchten M.Th. 1998. Neural network
analysis for hierarchical prediction of soil water retention and
saturated hydraulic conductivity. Soil Sci. Soc. Am. J. 62:847-855.

Schaap, M.G., and F.J. Leij, 1998. Database Related Accuracy and
Uncertainty of Pedotransfer Functions, Soil Science 163:765-779.

Schaap, M.G., F.J. Leij and M. Th. van Genuchten. 1999. A
bootstrap-neural network approach to predict soil hydraulic parameters.
In: van Genuchten, M.Th., F.J. Leij, and L. Wu (eds), Proc. Int.
Workshop, Characterization and Measurements of the Hydraulic Properties
of Unsaturated Porous Media, pp 1237-1250, University of California,
Riverside, CA.

Schaap, M.G., F.J. Leij, 1999, Improved prediction of unsaturated
hydraulic conductivity with the Mualem-van Genuchten, Submitted to Soil
Sci. Soc. Am. J.

van Genuchten, M.Th. 1980. A closed-form equation for predicting the
hydraulic conductivity of unsaturated soils. Soil Sci. Am. J.
44:892-898.

Schaap, M.G., F.J. Leij, and M.Th. van Genuchten. 2001. ROSETTA: a
computer program for estimating soil hydraulic parameters with
hierarchical pedotransfer functions. Journal of Hydrology 251(3-4):
163-176. doi:
[doi:10.1016/S0022-1694(01)00466-8](https://doi.org/10.1016/S0022-1694%2801%2900466-8)
.

Schaap, M.G., A. Nemes, and M.T. van Genuchten. 2004. Comparison of
Models for Indirect Estimation of Water Retention and Available Water in
Surface Soils. Vadose Zone Journal 3(4): 1455-1463. doi:
[doi:10.2136/vzj2004.1455](https://doi.org/10.2136/vzj2004.1455) .

Zhang, Y., and M.G. Schaap. 2017. Weighted recalibration of the Rosetta
pedotransfer model with improved estimates of hydraulic parameter
distributions and summary statistics (Rosetta3). Journal of Hydrology
547: 39-53. doi:
[doi:10.1016/j.jhydrol.2017.01.004](https://doi.org/10.1016/j.jhydrol.2017.01.004)
.

## Author

D.E. Beaudette, Todd Skaggs (ARS), Richard Reid

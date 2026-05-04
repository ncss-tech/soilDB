# Web Coverage Services Details

List variables or databases provided by soilDB web coverage service
(WCS) abstraction. These lists will be expanded in future versions.

## Usage

``` r
WCS_details(wcs = c("mukey", "ISSR800", "soilColor"))
```

## Arguments

- wcs:

  a WCS label ('mukey', 'ISSR800', or 'soilColor')

## Value

a `data.frame`

## Examples

``` r

WCS_details(wcs = 'ISSR800')
#>                 var       crs
#> 4     caco3_kg_sq_m EPSG:5070
#> 8         cec_025cm EPSG:5070
#> 9         cec_050cm EPSG:5070
#> 7          cec_05cm EPSG:5070
#> 18       clay_025cm EPSG:5070
#> 17        clay_05cm EPSG:5070
#> 19      clay_2550cm EPSG:5070
#> 20      clay_3060cm EPSG:5070
#> 32   drainage_class EPSG:5070
#> 6          ec_025cm EPSG:5070
#> 5           ec_05cm EPSG:5070
#> 38       greatgroup EPSG:5070
#> 31           hydgrp EPSG:5070
#> 42    lcc_irrigated EPSG:5070
#> 43 lcc_nonirrigated EPSG:5070
#> 46     n_components EPSG:5070
#> 45       n_polygons EPSG:5070
#> 3        om_kg_sq_m EPSG:5070
#> 10             paws EPSG:5070
#> 12       paws_025cm EPSG:5070
#> 11       paws_050cm EPSG:5070
#> 14         ph_025cm EPSG:5070
#> 13          ph_05cm EPSG:5070
#> 15        ph_2550cm EPSG:5070
#> 16        ph_3060cm EPSG:5070
#> 22       sand_025cm EPSG:5070
#> 21        sand_05cm EPSG:5070
#> 23      sand_2550cm EPSG:5070
#> 24      sand_3060cm EPSG:5070
#> 29              sar EPSG:5070
#> 30      series_name EPSG:5070
#> 26       silt_025cm EPSG:5070
#> 25        silt_05cm EPSG:5070
#> 27      silt_2550cm EPSG:5070
#> 28      silt_3060cm EPSG:5070
#> 36        soilorder EPSG:5070
#> 2        ssurgo_pct EPSG:5070
#> 1       statsgo_pct EPSG:5070
#> 35              str EPSG:5070
#> 37         suborder EPSG:5070
#> 44      survey_type EPSG:5070
#> 40    texture_025cm EPSG:5070
#> 39     texture_05cm EPSG:5070
#> 41   texture_2550cm EPSG:5070
#> 33              weg EPSG:5070
#> 34              wei EPSG:5070
#>                                               description
#> 4                                    Total CaCO3 (kg/m^2)
#> 8                   CEC at pH 7 0-25cm depth (cmol[+]/kg)
#> 9                   CEC at pH 7 0-50cm depth (cmol[+]/kg)
#> 7                    CEC at pH 7 0-5cm depth (cmol[+]/kg)
#> 18                              clay percent 0-25cm depth
#> 17                               clay percent 0-5cm depth
#> 19                             clay percent 25-50cm depth
#> 20                             clay percent 30-60cm depth
#> 32                                    Soil Drainage Class
#> 6                                  EC 0-25cm depth (dS/m)
#> 5                                   EC 0-5cm depth (dS/m)
#> 38                              Soil Taxonomy: Greatgroup
#> 31                                  Hydrologic Soil Group
#> 42                       Land Capability Class, irrigated
#> 43                   Land Capability Class, non-irrigated
#> 46                     Number of components per grid cell
#> 45                       Number of polygons per grid cell
#> 3                      Total Soil Organic Matter (kg/m^2)
#> 10         total plant available water storage (cm water)
#> 12  plant available water storage 0-25cm depth (cm water)
#> 11  plant available water storage 0-50cm depth (cm water)
#> 14                                pH 1:1 H2O 0-25cm depth
#> 13                                 pH 1:1 H2O 0-5cm depth
#> 15                               pH 1:1 H2O 25-50cm depth
#> 16                               pH 1:1 H2O 30-60cm depth
#> 22                              sand percent 0-25cm depth
#> 21                               sand percent 0-5cm depth
#> 23                             sand percent 25-50cm depth
#> 24                             sand percent 30-60cm depth
#> 29                                    SAR, entire profile
#> 30                                       Soil Series Name
#> 26                              silt percent 0-25cm depth
#> 25                               silt percent 0-5cm depth
#> 27                             silt percent 25-50cm depth
#> 28                             silt percent 30-60cm depth
#> 36                              Soil Taxonomy: Soil Order
#> 2   SSURGO data available, fraction of 800x800m grid cell
#> 1  STATSGO data available, fraction of 800x800m grid cell
#> 35                                Soil Temperature Regime
#> 37                                Soil Taxonomy: Suborder
#> 44                                Soil survey data source
#> 40                             Soil Texture Class, 0-25cm
#> 39                              Soil Texture Class, 0-5cm
#> 41                            Soil Texture Class, 25-50cm
#> 33                                 Wind Erodibility Group
#> 34                                 Wind Erodibility Index
```

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
#>                 var                                            description
#> 4     caco3_kg_sq_m                                   Total CaCO3 (kg/m^2)
#> 8         cec_025cm                  CEC at pH 7 0-25cm depth (cmol[+]/kg)
#> 9         cec_050cm                  CEC at pH 7 0-50cm depth (cmol[+]/kg)
#> 7          cec_05cm                   CEC at pH 7 0-5cm depth (cmol[+]/kg)
#> 18       clay_025cm                              clay percent 0-25cm depth
#> 17        clay_05cm                               clay percent 0-5cm depth
#> 19      clay_2550cm                             clay percent 25-50cm depth
#> 20      clay_3060cm                             clay percent 30-60cm depth
#> 32   drainage_class                                    Soil Drainage Class
#> 6          ec_025cm                                 EC 0-25cm depth (dS/m)
#> 5           ec_05cm                                  EC 0-5cm depth (dS/m)
#> 38       greatgroup                              Soil Taxonomy: Greatgroup
#> 31           hydgrp                                  Hydrologic Soil Group
#> 42    lcc_irrigated                       Land Capability Class, irrigated
#> 43 lcc_nonirrigated                   Land Capability Class, non-irrigated
#> 3        om_kg_sq_m                     Total Soil Organic Matter (kg/m^2)
#> 10             paws         total plant available water storage (cm water)
#> 12       paws_025cm  plant available water storage 0-25cm depth (cm water)
#> 11       paws_050cm  plant available water storage 0-50cm depth (cm water)
#> 14         ph_025cm                                pH 1:1 H2O 0-25cm depth
#> 13          ph_05cm                                 pH 1:1 H2O 0-5cm depth
#> 15        ph_2550cm                               pH 1:1 H2O 25-50cm depth
#> 16        ph_3060cm                               pH 1:1 H2O 30-60cm depth
#> 22       sand_025cm                              sand percent 0-25cm depth
#> 21        sand_05cm                               sand percent 0-5cm depth
#> 23      sand_2550cm                             sand percent 25-50cm depth
#> 24      sand_3060cm                             sand percent 30-60cm depth
#> 29              sar                                    SAR, entire profile
#> 30      series_name                                       Soil Series Name
#> 26       silt_025cm                              silt percent 0-25cm depth
#> 25        silt_05cm                               silt percent 0-5cm depth
#> 27      silt_2550cm                             silt percent 25-50cm depth
#> 28      silt_3060cm                             silt percent 30-60cm depth
#> 36        soilorder                              Soil Taxonomy: Soil Order
#> 2        ssurgo_pct  SSURGO data available, fraction of 800x800m grid cell
#> 1       statsgo_pct STATSGO data available, fraction of 800x800m grid cell
#> 35              str                                Soil Temperature Regime
#> 37         suborder                                Soil Taxonomy: Suborder
#> 40    texture_025cm                             Soil Texture Class, 0-25cm
#> 39     texture_05cm                              Soil Texture Class, 0-5cm
#> 41   texture_2550cm                            Soil Texture Class, 25-50cm
#> 33              weg                                 Wind Erodibility Group
#> 34              wei                                 Wind Erodibility Index
```

# Get map unit properties from Soil Data Access

Get map unit properties from Soil Data Access

## Usage

``` r
get_SDA_property(
  property,
  method = c("Dominant Component (Category)", "Weighted Average", "Min/Max",
    "Dominant Component (Numeric)", "Dominant Condition", "None"),
  areasymbols = NULL,
  mukeys = NULL,
  WHERE = NULL,
  top_depth = 0,
  bottom_depth = 200,
  FUN = NULL,
  include_minors = FALSE,
  miscellaneous_areas = FALSE,
  query_string = FALSE,
  dsn = NULL
)
```

## Arguments

- property:

  character vector of labels from property dictionary tables (see
  details) OR physical column names from `component` or `chorizon`
  table.

- method:

  one of: "Dominant Component (Category)", "Dominant Component
  (Numeric)", "Weighted Average", "MIN", "MAX", "Dominant Condition", or
  "None". If "None" is selected, the number of rows returned will depend
  on whether a component or horizon level property was selected,
  otherwise the result will be 1:1 with the number of map units.

- areasymbols:

  vector of soil survey area symbols

- mukeys:

  vector of map unit keys

- WHERE:

  character containing SQL WHERE clause specified in terms of fields in
  `legend` or `mapunit` tables, used in lieu of `mukeys` or
  `areasymbols`. With aggregation method `"NONE"` the WHERE clause may
  additionally contain logic for columns from the `component` and
  `chorizon` table.

- top_depth:

  Default: `0` (centimeters); a numeric value for upper boundary (top
  depth) used only for method="Weighted Average", "Dominant Component
  (Numeric)", and "MIN/MAX"

- bottom_depth:

  Default: `200` (centimeters); a numeric value for lower boundary
  (bottom depth) used only for method="Weighted Average", "Dominant
  Component (Numeric)", and "MIN/MAX"

- FUN:

  Optional: character representing SQL aggregation function either "MIN"
  or "MAX" used only for method="min/max"; this argument is calculated
  internally if you specify `method="MIN"` or `method="MAX"`

- include_minors:

  Include minor components in "Weighted Average" or "MIN/MAX" results?
  Default: `TRUE`

- miscellaneous_areas:

  Include miscellaneous areas (non-soil components) in results? Default:
  `FALSE`. Now works with all `method` types)

- query_string:

  Default: `FALSE`; if `TRUE` return a character string containing query
  that would be sent to SDA via `SDA_query`

- dsn:

  Path to local SQLite database or a DBIConnection object. If `NULL`
  (default) use Soil Data Access API via
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md).

## Value

a data.frame with result

## Details

The `property` argument refers to one of the property names or columns
specified in the tables below. Note that `property` can be specified as
either a character vector of labeled properties, such as
`"Bulk Density 0.33 bar H2O - Rep Value"`, OR physical column names such
as `"dbthirdbar_r"`. To get "low" and "high" values for a particular
property, replace the `_r` with `_l` or `_h` in the physical column
name; for example
`property = c("dbthirdbar_l","dbthirdbar_r","dbthirdbar_h")`. You can
view exhaustive lists of component and component horizon level
properties in the Soil Data Access ["Tables and Columns
Report"](https://sdmdataaccess.sc.egov.usda.gov/documents/TablesAndColumnsReport.pdf).

### Selected Component-level Properties

|                                     |               |
|-------------------------------------|---------------|
| **Property (Component)**            | **Column**    |
| Range Production - Favorable Year   | rsprod_h      |
| Range Production - Normal Year      | rsprod_r      |
| Range Production - Unfavorable Year | rsprod_l      |
| Corrosion of Steel                  | corsteel      |
| Corrosion of Concrete               | corcon        |
| Drainage Class                      | drainagecl    |
| Hydrologic Group                    | hydgrp        |
| Taxonomic Class Name                | taxclname     |
| Taxonomic Order                     | taxorder      |
| Taxonomic Suborder                  | taxsuborder   |
| Taxonomic Temperature Regime        | taxtempregime |
| Wind Erodibility Group              | weg           |
| Wind Erodibility Index              | wei           |
| t Factor                            | tfact         |

### Selected Horizon-level Properties

|                                                  |                    |
|--------------------------------------------------|--------------------|
| **Property (Horizon)**                           | **Column**         |
| 0.1 bar H2O - Rep Value                          | wtenthbar_r        |
| 0.33 bar H2O - Rep Value                         | wthirdbar_r        |
| 15 bar H2O - Rep Value                           | wfifteenbar_r      |
| Available Water Capacity - Rep Value             | awc_r              |
| Bray 1 Phosphate - Rep Value                     | pbray1_r           |
| Bulk Density 0.1 bar H2O - Rep Value             | dbtenthbar_r       |
| Bulk Density 0.33 bar H2O - Rep Value            | dbthirdbar_r       |
| Bulk Density 15 bar H2O - Rep Value              | dbfifteenbar_r     |
| Bulk Density oven dry - Rep Value                | dbovendry_r        |
| CaCO3 Clay - Rep Value                           | claysizedcarb_r    |
| Calcium Carbonate - Rep Value                    | caco3_r            |
| Cation Exchange Capacity - Rep Value             | cec7_r             |
| Coarse Sand - Rep Value                          | sandco_r           |
| Coarse Silt - Rep Value                          | siltco_r           |
| Effective Cation Exchange Capacity - Rep Value   | ecec_r             |
| Electrial Conductivity 1:5 by volume - Rep Value | ec15_r             |
| Electrical Conductivity - Rep Value              | ec_r               |
| Exchangeable Sodium Percentage - Rep Value       | esp_r              |
| Extract Aluminum - Rep Value                     | extral_r           |
| Extractable Acidity - Rep Value                  | extracid_r         |
| Fine Sand - Rep Value                            | sandfine_r         |
| Fine Silt - Rep Value                            | siltfine_r         |
| Free Iron - Rep Value                            | freeiron_r         |
| Gypsum - Rep Value                               | gypsum_r           |
| Kf                                               | kffact             |
| Ki                                               | kifact             |
| Kr                                               | krfact             |
| Kw                                               | kwfact             |
| LEP - Rep Value                                  | lep_r              |
| Liquid Limit - Rep Value                         | ll_r               |
| Medium Sand - Rep Value                          | sandmed_r          |
| Organic Matter - Rep Value                       | om_r               |
| Oxalate Aluminum - Rep Value                     | aloxalate_r        |
| Oxalate Iron - Rep Value                         | feoxalate_r        |
| Oxalate Phosphate - Rep Value                    | poxalate_r         |
| Plasticity Index - Rep Value                     | pi_r               |
| Rock Fragments 3 - 10 inches - Rep Value         | frag3to10_r        |
| Rock Fragments \> 10 inches - Rep Value          | fraggt10_r         |
| Rubbed Fiber % - Rep Value                       | fiberrubbedpct_r   |
| Satiated H2O - Rep Value                         | wsatiated_r        |
| Saturated Hydraulic Conductivity - Rep Value     | ksat_r             |
| Sodium Adsorption Ratio - Rep Value              | sar_r              |
| Sum of Bases - Rep Value                         | sumbases_r         |
| Total Clay - Rep Value                           | claytotal_r        |
| Total Phosphate - Rep Value                      | ptotal_r           |
| Total Sand - Rep Value                           | sandtotal_r        |
| Total Silt - Rep Value                           | silttotal_r        |
| Unrubbed Fiber % - Rep Value                     | fiberunrubbedpct_r |
| Very Coarse Sand - Rep Value                     | sandvc_r           |
| Very Fine Sand - Rep Value                       | sandvf_r           |
| Water Soluble Phosphate - Rep Value              | ph2osoluble_r      |
| no. 10 sieve - Rep Value                         | sieveno10_r        |
| no. 200 sieve - Rep Value                        | sieveno200_r       |
| no. 4 sieve - Rep Value                          | sieveno4_r         |
| no. 40 sieve - Rep Value                         | sieveno40_r        |
| pH .01M CaCl2 - Rep Value                        | ph01mcacl2_r       |
| pH 1:1 water - Rep Value                         | ph1to1h2o_r        |
| pH Oxidized - Rep Value                          | phoxidized_r       |

## Author

Jason Nemecek, Chad Ferguson, Andrew Brown

## Examples

``` r
# \donttest{

 # get 1/3 bar bulk density [0,25] centimeter depth weighted average from dominant component
 get_SDA_property(property = c("dbthirdbar_l","dbthirdbar_r","dbthirdbar_h"),
                  method = "Dominant Component (Numeric)",
                  areasymbols = "CA630",
                  top_depth = 0,
                  bottom_depth = 25)
#> single result set, returning a data.frame
#>       mukey areasymbol musym
#> 1   1865918      CA630  3046
#> 2   1865926      CA630  7088
#> 3   1865927      CA630  7155
#> 4   1865928      CA630  7156
#> 5   1865929      CA630  8033
#> 6   1865930      CA630  8034
#> 7   1865931      CA630  8036
#> 8   1900697      CA630  6038
#> 9   1906347      CA630  7159
#> 10  1913590      CA630  6029
#> 11  1913591      CA630  6034
#> 12  1913592      CA630  6037
#> 13  1913600      CA630  7207
#> 14  1913601      CA630  8173
#> 15  1913602      CA630  8160
#> 16  1913605      CA630  8177
#> 17  1913606      CA630  8178
#> 18  1913607      CA630  3020
#> 19  2220266      CA630  3038
#> 20  2220269      CA630  4048
#> 21  2220270      CA630  4046
#> 22  2220271      CA630  6043
#> 23  2220273      CA630  6041
#> 24  2220301      CA630  8161
#> 25  2374651      CA630  4136
#> 26  2383083      CA630  8171
#> 27  2383084      CA630  8172
#> 28  2399766      CA630  6070
#> 29  2399769      CA630  6074
#> 30  2399770      CA630  6075
#> 31  2399771      CA630  6076
#> 32  2399780      CA630  8110
#> 33  2399783      CA630  8115
#> 34  2403696      CA630  5100
#> 35  2403709      CA630  1012
#> 36  2403710      CA630  8314
#> 37  2403711      CA630  8312
#> 38  2403719      CA630  9013
#> 39  2403720      CA630  9012
#> 40  2403721      CA630  9011
#> 41  2403722      CA630  9010
#> 42  2403747      CA630  6202
#> 43  2424959      CA630  8317
#> 44  2424960      CA630  8318
#> 45  2424961      CA630  8319
#> 46  2424962      CA630  8175
#> 47  2424963      CA630  8176
#> 48  2424975      CA630  9014
#> 49  2425107      CA630  3021
#> 50  2426355      CA630  8026
#> 51  2426483      CA630  7096
#> 52  2436790      CA630  8307
#> 53  2436792      CA630  8302
#> 54  2440240      CA630  7086
#> 55  2440242      CA630  7087
#> 56  2441253      CA630  6071
#> 57  2441798      CA630  7085
#> 58  2450478      CA630  5101
#> 59  2450843      CA630  7089
#> 60  2450844      CA630  7083
#> 61  2450845      CA630  3058
#> 62  2452459      CA630  8162
#> 63  2455490      CA630  8190
#> 64  2455492      CA630  8286
#> 65  2455494      CA630  8289
#> 66  2455495      CA630  8287
#> 67  2462630      CA630     W
#> 68  2480973      CA630  3033
#> 69  2482710      CA630  6039
#> 70  2483494      CA630  8194
#> 71  2600456      CA630  7210
#> 72  2600457      CA630  7211
#> 73  2600458      CA630  7212
#> 74  2600460      CA630  4040
#> 75  2600465      CA630  5051
#> 76  2600467      CA630  5053
#> 77  2600469      CA630  5057
#> 78  2600480      CA630  9015
#> 79  2600481      CA630  9016
#> 80  2600527      CA630  4200
#> 81  2600528      CA630  4201
#> 82  2600529      CA630  5013
#> 83  2600534      CA630  7165
#> 84  2600537      CA630  9017
#> 85  2600538      CA630  4202
#> 86  2766830      CA630  7166
#> 87  2766836      CA630  5201
#> 88  2766837      CA630  5202
#> 89  2766838      CA630  5012
#> 90  2766850      CA630  5015
#> 91  2924701      CA630  6078
#> 92  2924738      CA630  6072
#> 93  2924739      CA630  6079
#> 94  2924751      CA630  7074
#> 95  2924752      CA630  7076
#> 96  2924753      CA630  7078
#> 97  2924754      CA630  7079
#> 98  2924831      CA630  6036
#> 99  2924832      CA630  6033
#> 100 2924833      CA630  1090
#> 101 2924834      CA630  1091
#> 102 2924835      CA630  9018
#> 103 2924879      CA630   206
#> 104 2924880      CA630   207
#> 105 2924881      CA630   208
#> 106 2924882      CA630   209
#> 107 2924883      CA630   212
#> 108 2924884      CA630   401
#> 109 2924885      CA630   451
#> 110 2924886      CA630   475
#> 111 2924887      CA630  5016
#> 112 2924890      CA630   851
#> 113 2924907      CA630   128
#> 114 2924908      CA630   301
#> 115 2924909      CA630  8120
#> 116 2924912      CA630   DAM
#> 117 2924913      CA630   220
#> 118 2924914      CA630  1013
#> 119 2924955      CA630  8111
#> 120 3225132      CA630  7065
#> 121 3225133      CA630  7066
#> 122 3225134      CA630  7091
#> 123 3225135      CA630  7092
#> 124 3356286      CA630  6054
#> 125 3356287      CA630  6055
#> 126 3356288      CA630  6205
#> 127 3356289      CA630  6206
#> 128 3356290      CA630  6207
#> 129 3431597      CA630  5105
#>                                                                                                                                                 muname
#> 1                                                                                          Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes
#> 2                                                                                              Loafercreek-Gopheridge complex, 30 to 60 percent slopes
#> 3                                                                                                   Crimeahouse-Sixbit complex, 3 to 15 percent slopes
#> 4                                                                                                  Crimeahouse-Sixbit complex, 15 to 30 percent slopes
#> 5                                                                                         Copperopolis-Whiterock complex, 2 to 8 percent slopes, rocky
#> 6                                                                                        Copperopolis-Whiterock complex, 3 to 15 percent slopes, rocky
#> 7                                                                                       Copperopolis-Whiterock complex, 15 to 30 percent slopes, rocky
#> 8                                                                                  Musick-Wukusick complex, 30 to 60 percent slopes, low precipitation
#> 9                                                                                                  Crimeahouse-Sixbit complex, 30 to 70 percent slopes
#> 10                                                                                    Holland-Chawanakee-Rock outcrop complex, 45 to 90 percent slopes
#> 11                                                                                                     Musick-Wukusick complex, 3 to 15 percent slopes
#> 12                                                                                                    Musick-Wukusick complex, 15 to 30 percent slopes
#> 13                                                                                      Millvilla-Copperopolis-Hetchy complex, 30 to 60 percent slopes
#> 14                                                                                       Nedsgulch-Wallyhill-Arpatutu complex, 30 to 60 percent slopes
#> 15                                                                                                     Nedsgulch-Sites complex, 3 to 15 percent slopes
#> 16                                                                              Moccasinhill-Copperopolis-Sanguinetti complex, 30 to 60 percent slopes
#> 17                                                                                          Moccasinhill-Copperopolis complex, 60 to 90 percent slopes
#> 18                                                                                          Iron Mountain-Rock outcrop complex, 3 to 15 percent slopes
#> 19                                                                              Devilsnose-Lilygap complex, 30 to 60 percent slopes, low precipitation
#> 20                                                                                                 Devilsnose-Lilygap complex, 30 to 60 percent slopes
#> 21                                                                                                   Redapple-Lilygap complex, 15 to 30 percent slopes
#> 22                                                                                                    Mantree ashy sandy loam, 15 to 30 percent slopes
#> 23                                                                                                     Mantree ashy sandy loam, 3 to 15 percent slopes
#> 24                                                                                                    Nedsgulch-Sites complex, 15 to 30 percent slopes
#> 25                                                    Ultic Haploxeralfs, shallow-Ultic Haploxeralfs, moderately deep complex, 10 to 35 percent slopes
#> 26                                                                                                 Nedsgulch-Wallyhill complex, 3 to 15 percent slopes
#> 27                                                                                       Nedsgulch-Wallyhill-Arpatutu complex, 15 to 30 percent slopes
#> 28                                                                                   Sierra-Verjeles-Aquic Haploxeralfs complex, 0 to 8 percent slopes
#> 29                                                                                                        Sierra-Orose complex, 8 to 30 percent slopes
#> 30                                                                                                      Sierra-Flanly complex, 30 to 60 percent slopes
#> 31                                                                                      Auberry-Hurleton-Rock outcrop complex, 20 to 60 percent slopes
#> 32                                                                                        Cumulic Humixerepts-Riverwash complex, 0 to 8 percent slopes
#> 33                                                                                           Sanguinetti-Copperopolis complex, 30 to 60 percent slopes
#> 34                                                     Mokelumne-Buenavista-Aquultic Haploxeralfs, occasionally ponded complex, 1 to 12 percent slopes
#> 35                                                                                                                                          Mined land
#> 36                                                                        Rock outcrop-Tiger Creek-Vertic Haploxerepts complex, 1 to 45 percent slopes
#> 37                                                                                              Aquariusmine-Millvilla complex, 3 to 30 percent slopes
#> 38                                                                                                Urban land-Millvilla complex, 1 to 25 percent slopes
#> 39                                                                                             Urban land-Copperopolis complex, 0 to 15 percent slopes
#> 40                                                                                            Urban land-Sierra-Flanly complex, 3 to 25 percent slopes
#> 41                                                                                                                                          Urban land
#> 42                                                                  Musick-Ultic Haploxeralfs, moderately well drained, complex, 1 to 8 percent slopes
#> 43                                                                                                 Beybek-Rock outcrop complex, 3 to 30 percent slopes
#> 44                                                                                   Aquariusmine-Hetchy-Rock outcrop complex, 30 to 60 percent slopes
#> 45                                                                Mollic Haploxeralfs-Pachic Argixerolls-Rock Outcrop complex, 50 to 90 percent slopes
#> 46                                                                                                 Copperopolis-Hetchy complex, 8 to 30 percent slopes
#> 47                                                                            Sanguinetti-Moccasinhill-Deerflat complex, 60 to 90 percent slopes, cool
#> 48                                                                                             Urban land-Musick-Hotaw complex, 3 to 30 percent slopes
#> 49                                                                                 Iron Mountain-Crozier-Rock outcrop complex, 15 to 60 percent slopes
#> 50                                                                                          Moccasinhill-Copperopolis complex, 30 to 60 percent slopes
#> 51                                                                                              Gopheridge-Jasperpeak complex, 50 to 90 percent slopes
#> 52                                                                                              Tiger Creek-Nedsgulch complex, 15 to 50 percent slopes
#> 53                                                                                               Tiger Creek-Nedsgulch complex, 3 to 15 percent slopes
#> 54                                                                                             Loafercreek-Gopheridge complex, 15 to 30 percent slopes
#> 55                                                                                       Loafercreek-Gopheridge complex, cool, 15 to 30 percent slopes
#> 56                                                                                                       Sierra-Flanly complex, 3 to 15 percent slopes
#> 57                                                                                                 Bonanza-Loafercreek complex, 3 to 15 percent slopes
#> 58                                                                                                  Hornitos-Red Bluff complex, 2 to 25 percent slopes
#> 59                                                                                  Gardellones-Gopheridge-Motherlode complex, 30 to 60 percent slopes
#> 60                                     Aquic Haploxeralfs, rarely flooded and occasionally ponded-Loafercreek-Dunstone complex, 1 to 12 percent slopes
#> 61                                                                                              Shawsflat-Angelscreek complex, 25 to 60 percent slopes
#> 62                                                                                                 Nedsgulch-Arpatutu complex, 30 to 60 percent slopes
#> 63                                                                                                Lickinfork-Arpatutu complex, 40 to 90 percent slopes
#> 64                                                                                                    Jocal gravelly silt loam, 8 to 30 percent slopes
#> 65                                                                                            Fiddletown-Rock outcrop complex, 40 to 90 percent slopes
#> 66                                                                                                   Jocal-Fiddletown complex, 30 to 60 percent slopes
#> 67                                                                                                                                               Water
#> 68                                                                                Redapple-Lilygap complex, 15 to 30 percent slopes, low precipitation
#> 69                                                                                           Holland-Wukusick-Mantree complex, 30 to 60 percent slopes
#> 70                                                                                         Wallyhill, deep-Lickinfork complex, 40 to 90 percent slopes
#> 71                                                                                                  Deerflat-Millvilla complex, 3 to 15 percent slopes
#> 72                                                                                                Millvilla-Luckymine complex, 15 to 30 percent slopes
#> 73                                                                                               Wardsferry-Millvilla complex, 30 to 60 percent slopes
#> 74                                                                                   Iron Mountain-Redapple-Devilsnose complex, 3 to 15 percent slopes
#> 75                                                                                           Fuches-Lithic Xerorthents complex, 3 to 15 percent slopes
#> 76                                                                                          Fuches-Lithic Xerorthents complex, 15 to 50 percent slopes
#> 77                                                                                                                  Supan loam, 5 to 30 percent slopes
#> 78                                                                                     Urban land-Loafercreek-Dunstone complex, 3 to 15 percent slopes
#> 79                                                                                     Urban land-Nedsgulch-Wallyhill complex, 15 to 30 percent slopes
#> 80                                                                                                    Inks-Angelscreek complex, 3 to 15 percent slopes
#> 81                                                                                                  Angelscreek-Pentz complex, 15 to 30 percent slopes
#> 82                                                                                                 Miltonhills-Amador complex, 15 to 45 percent slopes
#> 83                                                                                                  Sixbit-Crimeahouse complex, 5 to 20 percent slopes
#> 84                                                                                                   Urban land-Amador complex, 2 to 15 percent slopes
#> 85                                                                                                  Angelscreek-Pentz complex, 30 to 60 percent slopes
#> 86                                                                                                Sixbit-Rock outcrop complex, 20 to 45 percent slopes
#> 87                                                                                                       Pardee-Amador complex, 1 to 15 percent slopes
#> 88                                                                                                      Pardee-Amador complex, 15 to 40 percent slopes
#> 89                                                                                                           Amador sandy loam, 2 to 15 percent slopes
#> 90                                                                                                   Ospital-Jennylind complex, 2 to 15 percent slopes
#> 91                                                                                                      Sierra-Flanly complex, 15 to 65 percent slopes
#> 92                                                                                                      Flanly-Verjeles complex, 0 to 8 percent slopes
#> 93                                                                                                                 Flanly loam, 8 to 30 percent slopes
#> 94                                                                                                 Loafercreek-Bonanza complex, 3 to 15 percent slopes
#> 95                                                                                     Bonanza-Loafercreek-Gopheridge complex, 15 to 30 percent slopes
#> 96                                                                                              Jasperpeak-Gopheridge complex, 30 to 60 percent slopes
#> 97                                                                                             Gopheridge-Loafercreek complex, 30 to 60 percent slopes
#> 98                                                                                 Musick-Wukusick complex, 15 to 30 percent slopes, low precipitation
#> 99                                                                                  Musick-Wukusick complex, 3 to 15 percent slopes, low precipitation
#> 100                                                                             Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
#> 101                                                                              Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
#> 102                                                                                  Urban land-Copperopolis-Whiterock complex, 8 to 30 percent slopes
#> 103                                                                                                           Pentz sandy loam, 2 to 15 percent slopes
#> 104                                                                                                          Pentz sandy loam, 15 to 50 percent slopes
#> 105                                                                                                     Pentz cobbly sandy loam, 2 to 8 percent slopes
#> 106                                                                                                      Pentz-Bellota complex, 2 to 15 percent slopes
#> 107                                                                                                                 Peters clay, 2 to 8 percent slopes
#> 108                                                                                                    Peters-Pentz association, 2 to 8 percent slopes
#> 109                                                                                                   Pentz-Peters association, 2 to 15 percent slopes
#> 110                                                                                                   Pentz-Peters association, 2 to 50 percent slopes
#> 111                                                                                            Jennylind-Rock outcrop complex, 10 to 45 percent slopes
#> 112                                                                                                           Mckeonhills clay, 5 to 15 percent slopes
#> 113                                                                                                        Cogna loam, 0 to 2 percent slopes, overwash
#> 114                                                                                           Archerdale-Hicksville association, 0 to 2 percent slopes
#> 115                                                                        Fluventic Haploxerepts-Oxyaquic Xerofluvents complex, 0 to 8 percent slopes
#> 116                                                                                                                                               Dams
#> 117                                                                                                  Redding gravelly loam, 0 to 8 percent slopes, dry
#> 118                                                                                  Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
#> 119 Psammentic Haploxerolls, rarely flooded-Mollic Fluvaquents, occasionally flooded-Riverwash, very frequently flooded complex, 0 to 8 percent slopes
#> 120                                                                             Bonanza-Loafercreek complex, 3 to 15 percent slopes, low precipitation
#> 121                                                                                    Bonanza-Loafercreek-Jasperpeak complex, 15 to 30 percent slopes
#> 122                                                                                    Trabuco-Jasperpeak-Rock outcrop complex, 8 to 30 percent slopes
#> 123                                                                                Gopheridge-Jasperpeak-Rock outcrop complex, 30 to 60 percent slopes
#> 124                                                                                          Shaver-Holland-Chawanakee complex, 8 to 30 percent slopes
#> 125                                                                                 Shaver-Lithic Humixerepts-Holland complex, 30 to 60 percent slopes
#> 126                                                                                                      Musick fine sandy loam, 3 to 8 percent slopes
#> 127                                                                                                       Musick-Hotaw complex, 8 to 30 percent slopes
#> 128                                                                                           Musick-Hotaw-Chawanakee complex, 30 to 60 percent slopes
#> 129                                                                                                Firebrick-Mokelumne complex, 2 to 30 percent slopes
#>     dbthirdbar_l dbthirdbar_r dbthirdbar_h
#> 1      1.3600000    1.4200000     1.490000
#> 2      1.3128000    1.3812000     1.458000
#> 3      1.2912000    1.3992000     1.514000
#> 4      1.3728000    1.4072000     1.448400
#> 5      1.3588000    1.4280000     1.500400
#> 6      1.3588000    1.4280000     1.500400
#> 7      1.3684000    1.4344000     1.500400
#> 8      1.3684000    1.4456000     1.530000
#> 9      1.3164000    1.4212000     1.526000
#> 10     1.1360000    1.2632000     1.390400
#> 11     1.0980000    1.1684000     1.244000
#> 12     1.2236000    1.3236000     1.421200
#> 13     1.3208000    1.4124000     1.504400
#> 14     1.2560000    1.3548000     1.453600
#> 15     1.0692000    1.1356000     1.212000
#> 16     1.3280000    1.3976000     1.468000
#> 17     1.3196000    1.4132000     1.506800
#> 18     1.2963640    1.4109090     1.517273
#> 19     0.3900000    0.4784000     0.660000
#> 20     0.3900000    0.4784000     0.660000
#> 21     0.5660000    0.7496000     0.792000
#> 22     0.8172001    0.8228000     1.045200
#> 23     0.8172001    0.8228000     1.045200
#> 24     1.0180000    1.0960000     1.173600
#> 25     1.4168000    1.4912000     1.572400
#> 26     1.3004000    1.3004000     1.429200
#> 27     1.3092000    1.3800000     1.458000
#> 28     1.3744000    1.4468000     1.520800
#> 29     1.3440000    1.4680000     1.594000
#> 30     1.3432000    1.4284000     1.515200
#> 31     1.3328000    1.4064000     1.483600
#> 32     1.2000000    1.3000000     1.410000
#> 33     1.2420000    1.3568000     1.463200
#> 34     1.3792000    1.4952000     1.604000
#> 35            NA           NA           NA
#> 36     1.1576000    1.2244000     1.290400
#> 37     1.3340000    1.4400000     1.546000
#> 38     1.3208000    1.4444000     1.559200
#> 39     1.3588000    1.4280000     1.500400
#> 40     1.3744000    1.4468000     1.520800
#> 41     1.4480000    1.4680000     1.484000
#> 42     1.2412000    1.3596000     1.479200
#> 43     1.1324000    1.3744000     1.478000
#> 44     1.2300000    1.3520000     1.472000
#> 45     1.3448000    1.4412000     1.537600
#> 46     1.3076000    1.3792000     1.450800
#> 47     1.1944000    1.2980000     1.402000
#> 48     1.3328000    1.4264000     1.526000
#> 49     1.2780000    1.3920000     1.500000
#> 50     1.2416000    1.3212000     1.398000
#> 51     1.3760000    1.4384000     1.501600
#> 52     1.1116000    1.2708000     1.424800
#> 53     1.2400000    1.3384000     1.436400
#> 54     1.3940000    1.4608000     1.534800
#> 55     1.1740000    1.2500000     1.328800
#> 56     1.3120000    1.4000000     1.490000
#> 57     1.3260000    1.4472000     1.567200
#> 58     1.2980000    1.3820000     1.424000
#> 59     1.2596000    1.3684000     1.483600
#> 60     1.2420000    1.3740000     1.506000
#> 61     1.1500000    1.3100000     1.470000
#> 62     1.0460000    1.1592000     1.273200
#> 63     1.2868000    1.3764000     1.456800
#> 64     1.0100000    1.0804000     1.160000
#> 65     0.8160000    0.9200001     1.020000
#> 66     0.7916000    0.8604000     1.020000
#> 67            NA           NA           NA
#> 68     0.5660000    0.7496000     0.792000
#> 69     0.7920001    0.8660001     0.942000
#> 70     0.9836000    1.0892000     1.197200
#> 71     1.3040000    1.4684000     1.548000
#> 72     1.3208000    1.4444000     1.559200
#> 73     1.2952000    1.3852000     1.480000
#> 74     1.3260000    1.4560000     1.580000
#> 75     1.3604000    1.4688000     1.567200
#> 76     1.3536000    1.4632000     1.580400
#> 77     0.9828000    1.1172000     1.252800
#> 78     1.3928000    1.4680000     1.546000
#> 79     1.3004000    1.3004000     1.429200
#> 80     1.3220000    1.4208000     1.519600
#> 81     1.1080000    1.3588000     1.501600
#> 82     1.4200000    1.4700000     1.520000
#> 83     1.3600000    1.4500000     1.540000
#> 84     1.4960000    1.5640000     1.624000
#> 85     1.1152000    1.4040000     1.495200
#> 86     1.3568000    1.4308000     1.503200
#> 87     1.3672000    1.4196000     1.472000
#> 88     1.3700000    1.4648000     1.563200
#> 89     1.4960000    1.5640000     1.624000
#> 90     1.4193330    1.4693330     1.514667
#> 91     1.3760000    1.4560000     1.538400
#> 92     1.3980000    1.5000000     1.596000
#> 93     1.3100000    1.4076000     1.512400
#> 94     1.2812000    1.3972000     1.518000
#> 95     1.3264000    1.4340000     1.538400
#> 96     1.3680000    1.4588000     1.554400
#> 97     1.3348000    1.4120000     1.486000
#> 98     1.2236000    1.3236000     1.421200
#> 99     1.0980000    1.1684000     1.244000
#> 100    1.3288000    1.4504000     1.572400
#> 101    1.4468000    1.5216000     1.596400
#> 102    1.3588000    1.4280000     1.500400
#> 103    1.3760000    1.4800000     1.582000
#> 104    1.3760000    1.4800000     1.582000
#> 105    1.4000000    1.4800000     1.566000
#> 106    1.3528000    1.4312000     1.499600
#> 107    1.1852000    1.2800000     1.380000
#> 108    1.3260000    1.3780000     1.426000
#> 109    1.3820000    1.4160000     1.456000
#> 110    1.3820000    1.4160000     1.456000
#> 111    1.4540000    1.4900000     1.526000
#> 112    1.0600000    1.1100000     1.300000
#> 113    1.4300000    1.4800000     1.540000
#> 114    1.3700000    1.3800000     1.390000
#> 115    1.3880000    1.4600000     1.538000
#> 116           NA           NA           NA
#> 117    1.2680000    1.4248000     1.542000
#> 118    1.5028000    1.5416000     1.579200
#> 119    1.4600000    1.5200000     1.580000
#> 120    1.3672000    1.4464000     1.481600
#> 121    1.3816000    1.4772000     1.524800
#> 122    1.3020000    1.3964000     1.486000
#> 123    1.3316000    1.4380000     1.498400
#> 124    1.4700000    1.4980000     1.534000
#> 125    1.4700000    1.4980000     1.534000
#> 126    1.1584000    1.2816000     1.373200
#> 127    1.3328000    1.4536000     1.517200
#> 128    1.2912000    1.4484000     1.538400
#> 129    1.5140000    1.5620000     1.600000
# }
```

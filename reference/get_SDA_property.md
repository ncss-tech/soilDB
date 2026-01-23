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
#> 1   2403709      CA630  1012
#> 2   2924914      CA630  1013
#> 3   2924833      CA630  1090
#> 4   2924834      CA630  1091
#> 5   2924907      CA630   128
#> 6   2924879      CA630   206
#> 7   2924880      CA630   207
#> 8   2924881      CA630   208
#> 9   2924882      CA630   209
#> 10  2924883      CA630   212
#> 11  2924913      CA630   220
#> 12  2924908      CA630   301
#> 13  1913607      CA630  3020
#> 14  2425107      CA630  3021
#> 15  2480973      CA630  3033
#> 16  2220266      CA630  3038
#> 17  1865918      CA630  3046
#> 18  2450845      CA630  3058
#> 19  2924884      CA630   401
#> 20  2600460      CA630  4040
#> 21  2220270      CA630  4046
#> 22  2220269      CA630  4048
#> 23  2374651      CA630  4136
#> 24  2600527      CA630  4200
#> 25  2600528      CA630  4201
#> 26  2600538      CA630  4202
#> 27  2924885      CA630   451
#> 28  2924886      CA630   475
#> 29  2766838      CA630  5012
#> 30  2600529      CA630  5013
#> 31  2766850      CA630  5015
#> 32  2924887      CA630  5016
#> 33  2600465      CA630  5051
#> 34  2600467      CA630  5053
#> 35  2600469      CA630  5057
#> 36  2403696      CA630  5100
#> 37  2450478      CA630  5101
#> 38  3431597      CA630  5105
#> 39  2766836      CA630  5201
#> 40  2766837      CA630  5202
#> 41  1913590      CA630  6029
#> 42  2924832      CA630  6033
#> 43  1913591      CA630  6034
#> 44  2924831      CA630  6036
#> 45  1913592      CA630  6037
#> 46  1900697      CA630  6038
#> 47  2482710      CA630  6039
#> 48  2220273      CA630  6041
#> 49  2220271      CA630  6043
#> 50  3356286      CA630  6054
#> 51  3356287      CA630  6055
#> 52  2399766      CA630  6070
#> 53  2441253      CA630  6071
#> 54  2924738      CA630  6072
#> 55  2399769      CA630  6074
#> 56  2399770      CA630  6075
#> 57  2399771      CA630  6076
#> 58  2924701      CA630  6078
#> 59  2924739      CA630  6079
#> 60  2403747      CA630  6202
#> 61  3356288      CA630  6205
#> 62  3356289      CA630  6206
#> 63  3356290      CA630  6207
#> 64  3225132      CA630  7065
#> 65  3225133      CA630  7066
#> 66  2924751      CA630  7074
#> 67  2924752      CA630  7076
#> 68  2924753      CA630  7078
#> 69  2924754      CA630  7079
#> 70  2450844      CA630  7083
#> 71  2441798      CA630  7085
#> 72  2440240      CA630  7086
#> 73  2440242      CA630  7087
#> 74  1865926      CA630  7088
#> 75  2450843      CA630  7089
#> 76  3225134      CA630  7091
#> 77  3225135      CA630  7092
#> 78  2426483      CA630  7096
#> 79  1865927      CA630  7155
#> 80  1865928      CA630  7156
#> 81  1906347      CA630  7159
#> 82  2600534      CA630  7165
#> 83  2766830      CA630  7166
#> 84  1913600      CA630  7207
#> 85  2600456      CA630  7210
#> 86  2600457      CA630  7211
#> 87  2600458      CA630  7212
#> 88  2426355      CA630  8026
#> 89  1865929      CA630  8033
#> 90  1865930      CA630  8034
#> 91  1865931      CA630  8036
#> 92  2399780      CA630  8110
#> 93  2924955      CA630  8111
#> 94  2399783      CA630  8115
#> 95  2924909      CA630  8120
#> 96  1913602      CA630  8160
#> 97  2220301      CA630  8161
#> 98  2452459      CA630  8162
#> 99  2383083      CA630  8171
#> 100 2383084      CA630  8172
#> 101 1913601      CA630  8173
#> 102 2424962      CA630  8175
#> 103 2424963      CA630  8176
#> 104 1913605      CA630  8177
#> 105 1913606      CA630  8178
#> 106 2455490      CA630  8190
#> 107 2483494      CA630  8194
#> 108 2455492      CA630  8286
#> 109 2455495      CA630  8287
#> 110 2455494      CA630  8289
#> 111 2436792      CA630  8302
#> 112 2436790      CA630  8307
#> 113 2403711      CA630  8312
#> 114 2403710      CA630  8314
#> 115 2424959      CA630  8317
#> 116 2424960      CA630  8318
#> 117 2424961      CA630  8319
#> 118 2924890      CA630   851
#> 119 2403722      CA630  9010
#> 120 2403721      CA630  9011
#> 121 2403720      CA630  9012
#> 122 2403719      CA630  9013
#> 123 2424975      CA630  9014
#> 124 2600480      CA630  9015
#> 125 2600481      CA630  9016
#> 126 2600537      CA630  9017
#> 127 2924835      CA630  9018
#> 128 2924912      CA630   DAM
#> 129 2462630      CA630     W
#>                                                                                                                                                 muname
#> 1                                                                                                                                           Mined land
#> 2                                                                                    Mined land-Anthraltic Xerorthents complex, 1 to 15 percent slopes
#> 3                                                                               Ultic Haploxeralfs-Mollic Haploxeralfs complex, 3 to 30 percent slopes
#> 4                                                                                Ultic Haploxeralfs-Aquic Dystroxerepts complex, 2 to 8 percent slopes
#> 5                                                                                                          Cogna loam, 0 to 2 percent slopes, overwash
#> 6                                                                                                             Pentz sandy loam, 2 to 15 percent slopes
#> 7                                                                                                            Pentz sandy loam, 15 to 50 percent slopes
#> 8                                                                                                       Pentz cobbly sandy loam, 2 to 8 percent slopes
#> 9                                                                                                        Pentz-Bellota complex, 2 to 15 percent slopes
#> 10                                                                                                                  Peters clay, 2 to 8 percent slopes
#> 11                                                                                                   Redding gravelly loam, 0 to 8 percent slopes, dry
#> 12                                                                                            Archerdale-Hicksville association, 0 to 2 percent slopes
#> 13                                                                                          Iron Mountain-Rock outcrop complex, 3 to 15 percent slopes
#> 14                                                                                 Iron Mountain-Crozier-Rock outcrop complex, 15 to 60 percent slopes
#> 15                                                                                Redapple-Lilygap complex, 15 to 30 percent slopes, low precipitation
#> 16                                                                              Devilsnose-Lilygap complex, 30 to 60 percent slopes, low precipitation
#> 17                                                                                         Goldwall-Toomes-Rock outcrop complex, 1 to 8 percent slopes
#> 18                                                                                              Shawsflat-Angelscreek complex, 25 to 60 percent slopes
#> 19                                                                                                     Peters-Pentz association, 2 to 8 percent slopes
#> 20                                                                                   Iron Mountain-Redapple-Devilsnose complex, 3 to 15 percent slopes
#> 21                                                                                                   Redapple-Lilygap complex, 15 to 30 percent slopes
#> 22                                                                                                 Devilsnose-Lilygap complex, 30 to 60 percent slopes
#> 23                                                    Ultic Haploxeralfs, shallow-Ultic Haploxeralfs, moderately deep complex, 10 to 35 percent slopes
#> 24                                                                                                    Inks-Angelscreek complex, 3 to 15 percent slopes
#> 25                                                                                                  Angelscreek-Pentz complex, 15 to 30 percent slopes
#> 26                                                                                                  Angelscreek-Pentz complex, 30 to 60 percent slopes
#> 27                                                                                                    Pentz-Peters association, 2 to 15 percent slopes
#> 28                                                                                                    Pentz-Peters association, 2 to 50 percent slopes
#> 29                                                                                                           Amador sandy loam, 2 to 15 percent slopes
#> 30                                                                                                 Miltonhills-Amador complex, 15 to 45 percent slopes
#> 31                                                                                                   Ospital-Jennylind complex, 2 to 15 percent slopes
#> 32                                                                                             Jennylind-Rock outcrop complex, 10 to 45 percent slopes
#> 33                                                                                           Fuches-Lithic Xerorthents complex, 3 to 15 percent slopes
#> 34                                                                                          Fuches-Lithic Xerorthents complex, 15 to 50 percent slopes
#> 35                                                                                                                  Supan loam, 5 to 30 percent slopes
#> 36                                                     Mokelumne-Buenavista-Aquultic Haploxeralfs, occasionally ponded complex, 1 to 12 percent slopes
#> 37                                                                                                  Hornitos-Red Bluff complex, 2 to 25 percent slopes
#> 38                                                                                                 Firebrick-Mokelumne complex, 2 to 30 percent slopes
#> 39                                                                                                       Pardee-Amador complex, 1 to 15 percent slopes
#> 40                                                                                                      Pardee-Amador complex, 15 to 40 percent slopes
#> 41                                                                                    Holland-Chawanakee-Rock outcrop complex, 45 to 90 percent slopes
#> 42                                                                                  Musick-Wukusick complex, 3 to 15 percent slopes, low precipitation
#> 43                                                                                                     Musick-Wukusick complex, 3 to 15 percent slopes
#> 44                                                                                 Musick-Wukusick complex, 15 to 30 percent slopes, low precipitation
#> 45                                                                                                    Musick-Wukusick complex, 15 to 30 percent slopes
#> 46                                                                                 Musick-Wukusick complex, 30 to 60 percent slopes, low precipitation
#> 47                                                                                           Holland-Wukusick-Mantree complex, 30 to 60 percent slopes
#> 48                                                                                                     Mantree ashy sandy loam, 3 to 15 percent slopes
#> 49                                                                                                    Mantree ashy sandy loam, 15 to 30 percent slopes
#> 50                                                                                           Shaver-Holland-Chawanakee complex, 8 to 30 percent slopes
#> 51                                                                                  Shaver-Lithic Humixerepts-Holland complex, 30 to 60 percent slopes
#> 52                                                                                   Sierra-Verjeles-Aquic Haploxeralfs complex, 0 to 8 percent slopes
#> 53                                                                                                       Sierra-Flanly complex, 3 to 15 percent slopes
#> 54                                                                                                      Flanly-Verjeles complex, 0 to 8 percent slopes
#> 55                                                                                                        Sierra-Orose complex, 8 to 30 percent slopes
#> 56                                                                                                      Sierra-Flanly complex, 30 to 60 percent slopes
#> 57                                                                                      Auberry-Hurleton-Rock outcrop complex, 20 to 60 percent slopes
#> 58                                                                                                      Sierra-Flanly complex, 15 to 65 percent slopes
#> 59                                                                                                                 Flanly loam, 8 to 30 percent slopes
#> 60                                                                  Musick-Ultic Haploxeralfs, moderately well drained, complex, 1 to 8 percent slopes
#> 61                                                                                                       Musick fine sandy loam, 3 to 8 percent slopes
#> 62                                                                                                        Musick-Hotaw complex, 8 to 30 percent slopes
#> 63                                                                                            Musick-Hotaw-Chawanakee complex, 30 to 60 percent slopes
#> 64                                                                              Bonanza-Loafercreek complex, 3 to 15 percent slopes, low precipitation
#> 65                                                                                     Bonanza-Loafercreek-Jasperpeak complex, 15 to 30 percent slopes
#> 66                                                                                                 Loafercreek-Bonanza complex, 3 to 15 percent slopes
#> 67                                                                                     Bonanza-Loafercreek-Gopheridge complex, 15 to 30 percent slopes
#> 68                                                                                              Jasperpeak-Gopheridge complex, 30 to 60 percent slopes
#> 69                                                                                             Gopheridge-Loafercreek complex, 30 to 60 percent slopes
#> 70                                     Aquic Haploxeralfs, rarely flooded and occasionally ponded-Loafercreek-Dunstone complex, 1 to 12 percent slopes
#> 71                                                                                                 Bonanza-Loafercreek complex, 3 to 15 percent slopes
#> 72                                                                                             Loafercreek-Gopheridge complex, 15 to 30 percent slopes
#> 73                                                                                       Loafercreek-Gopheridge complex, cool, 15 to 30 percent slopes
#> 74                                                                                             Loafercreek-Gopheridge complex, 30 to 60 percent slopes
#> 75                                                                                  Gardellones-Gopheridge-Motherlode complex, 30 to 60 percent slopes
#> 76                                                                                     Trabuco-Jasperpeak-Rock outcrop complex, 8 to 30 percent slopes
#> 77                                                                                 Gopheridge-Jasperpeak-Rock outcrop complex, 30 to 60 percent slopes
#> 78                                                                                              Gopheridge-Jasperpeak complex, 50 to 90 percent slopes
#> 79                                                                                                  Crimeahouse-Sixbit complex, 3 to 15 percent slopes
#> 80                                                                                                 Crimeahouse-Sixbit complex, 15 to 30 percent slopes
#> 81                                                                                                 Crimeahouse-Sixbit complex, 30 to 70 percent slopes
#> 82                                                                                                  Sixbit-Crimeahouse complex, 5 to 20 percent slopes
#> 83                                                                                                Sixbit-Rock outcrop complex, 20 to 45 percent slopes
#> 84                                                                                      Millvilla-Copperopolis-Hetchy complex, 30 to 60 percent slopes
#> 85                                                                                                  Deerflat-Millvilla complex, 3 to 15 percent slopes
#> 86                                                                                                Millvilla-Luckymine complex, 15 to 30 percent slopes
#> 87                                                                                               Wardsferry-Millvilla complex, 30 to 60 percent slopes
#> 88                                                                                          Moccasinhill-Copperopolis complex, 30 to 60 percent slopes
#> 89                                                                                        Copperopolis-Whiterock complex, 2 to 8 percent slopes, rocky
#> 90                                                                                       Copperopolis-Whiterock complex, 3 to 15 percent slopes, rocky
#> 91                                                                                      Copperopolis-Whiterock complex, 15 to 30 percent slopes, rocky
#> 92                                                                                        Cumulic Humixerepts-Riverwash complex, 0 to 8 percent slopes
#> 93  Psammentic Haploxerolls, rarely flooded-Mollic Fluvaquents, occasionally flooded-Riverwash, very frequently flooded complex, 0 to 8 percent slopes
#> 94                                                                                           Sanguinetti-Copperopolis complex, 30 to 60 percent slopes
#> 95                                                                         Fluventic Haploxerepts-Oxyaquic Xerofluvents complex, 0 to 8 percent slopes
#> 96                                                                                                     Nedsgulch-Sites complex, 3 to 15 percent slopes
#> 97                                                                                                    Nedsgulch-Sites complex, 15 to 30 percent slopes
#> 98                                                                                                 Nedsgulch-Arpatutu complex, 30 to 60 percent slopes
#> 99                                                                                                 Nedsgulch-Wallyhill complex, 3 to 15 percent slopes
#> 100                                                                                      Nedsgulch-Wallyhill-Arpatutu complex, 15 to 30 percent slopes
#> 101                                                                                      Nedsgulch-Wallyhill-Arpatutu complex, 30 to 60 percent slopes
#> 102                                                                                                Copperopolis-Hetchy complex, 8 to 30 percent slopes
#> 103                                                                           Sanguinetti-Moccasinhill-Deerflat complex, 60 to 90 percent slopes, cool
#> 104                                                                             Moccasinhill-Copperopolis-Sanguinetti complex, 30 to 60 percent slopes
#> 105                                                                                         Moccasinhill-Copperopolis complex, 60 to 90 percent slopes
#> 106                                                                                               Lickinfork-Arpatutu complex, 40 to 90 percent slopes
#> 107                                                                                        Wallyhill, deep-Lickinfork complex, 40 to 90 percent slopes
#> 108                                                                                                   Jocal gravelly silt loam, 8 to 30 percent slopes
#> 109                                                                                                  Jocal-Fiddletown complex, 30 to 60 percent slopes
#> 110                                                                                           Fiddletown-Rock outcrop complex, 40 to 90 percent slopes
#> 111                                                                                              Tiger Creek-Nedsgulch complex, 3 to 15 percent slopes
#> 112                                                                                             Tiger Creek-Nedsgulch complex, 15 to 50 percent slopes
#> 113                                                                                             Aquariusmine-Millvilla complex, 3 to 30 percent slopes
#> 114                                                                       Rock outcrop-Tiger Creek-Vertic Haploxerepts complex, 1 to 45 percent slopes
#> 115                                                                                                Beybek-Rock outcrop complex, 3 to 30 percent slopes
#> 116                                                                                  Aquariusmine-Hetchy-Rock outcrop complex, 30 to 60 percent slopes
#> 117                                                               Mollic Haploxeralfs-Pachic Argixerolls-Rock Outcrop complex, 50 to 90 percent slopes
#> 118                                                                                                           Mckeonhills clay, 5 to 15 percent slopes
#> 119                                                                                                                                         Urban land
#> 120                                                                                           Urban land-Sierra-Flanly complex, 3 to 25 percent slopes
#> 121                                                                                            Urban land-Copperopolis complex, 0 to 15 percent slopes
#> 122                                                                                               Urban land-Millvilla complex, 1 to 25 percent slopes
#> 123                                                                                            Urban land-Musick-Hotaw complex, 3 to 30 percent slopes
#> 124                                                                                    Urban land-Loafercreek-Dunstone complex, 3 to 15 percent slopes
#> 125                                                                                    Urban land-Nedsgulch-Wallyhill complex, 15 to 30 percent slopes
#> 126                                                                                                  Urban land-Amador complex, 2 to 15 percent slopes
#> 127                                                                                  Urban land-Copperopolis-Whiterock complex, 8 to 30 percent slopes
#> 128                                                                                                                                               Dams
#> 129                                                                                                                                              Water
#>     dbthirdbar_l dbthirdbar_r dbthirdbar_h
#> 1             NA           NA           NA
#> 2       1.502800    1.5416000     1.579200
#> 3       1.328800    1.4504000     1.572400
#> 4       1.446800    1.5216000     1.596400
#> 5       1.430000    1.4800000     1.540000
#> 6       1.376000    1.4800000     1.582000
#> 7       1.376000    1.4800000     1.582000
#> 8       1.400000    1.4800000     1.566000
#> 9       1.352800    1.4312000     1.499600
#> 10      1.185200    1.2800000     1.380000
#> 11      1.268000    1.4248000     1.542000
#> 12      1.370000    1.3800000     1.390000
#> 13      1.296364    1.4109091     1.517273
#> 14      1.278000    1.3919999     1.500000
#> 15      0.566000    0.7496000     0.792000
#> 16      0.390000    0.4784000     0.660000
#> 17      1.360000    1.4199999     1.490000
#> 18      1.150000    1.3100000     1.470000
#> 19      1.326000    1.3780000     1.426000
#> 20      1.326000    1.4560000     1.580000
#> 21      0.566000    0.7496000     0.792000
#> 22      0.390000    0.4784000     0.660000
#> 23      1.416800    1.4912000     1.572400
#> 24      1.322000    1.4208000     1.519600
#> 25      1.108000    1.3588000     1.501600
#> 26      1.115200    1.4040000     1.495200
#> 27      1.382000    1.4160001     1.456000
#> 28      1.382000    1.4160001     1.456000
#> 29      1.496000    1.5640000     1.624000
#> 30      1.420000    1.4700000     1.520000
#> 31      1.419333    1.4693334     1.514667
#> 32      1.454000    1.4900000     1.526000
#> 33      1.360400    1.4688000     1.567200
#> 34      1.353600    1.4632000     1.580400
#> 35      0.982800    1.1172000     1.252800
#> 36      1.379200    1.4952000     1.604000
#> 37      1.298000    1.3820000     1.424000
#> 38      1.514000    1.5620000     1.600000
#> 39      1.367200    1.4196000     1.472000
#> 40      1.370000    1.4648000     1.563200
#> 41      1.136000    1.2632000     1.390400
#> 42      1.098000    1.1684000     1.244000
#> 43      1.098000    1.1684000     1.244000
#> 44      1.223600    1.3236000     1.421200
#> 45      1.223600    1.3236000     1.421200
#> 46      1.368400    1.4456000     1.530000
#> 47      0.792000    0.8660000     0.942000
#> 48      0.817200    0.8228000     1.045200
#> 49      0.817200    0.8228000     1.045200
#> 50      1.470000    1.4980000     1.534000
#> 51      1.470000    1.4980000     1.534000
#> 52      1.374400    1.4468000     1.520800
#> 53      1.312000    1.4000000     1.490000
#> 54      1.398000    1.5000000     1.596000
#> 55      1.344000    1.4680000     1.594000
#> 56      1.343200    1.4284000     1.515200
#> 57      1.332800    1.4064000     1.483600
#> 58      1.376000    1.4560001     1.538400
#> 59      1.310000    1.4076000     1.512400
#> 60      1.241200    1.3596000     1.479200
#> 61      1.158400    1.2816000     1.373200
#> 62      1.332800    1.4536000     1.517200
#> 63      1.291200    1.4484000     1.538400
#> 64      1.367200    1.4464000     1.481600
#> 65      1.381600    1.4772000     1.524800
#> 66      1.281200    1.3972000     1.518000
#> 67      1.326400    1.4340000     1.538400
#> 68      1.368000    1.4588000     1.554400
#> 69      1.334800    1.4120000     1.486000
#> 70      1.242000    1.3740000     1.506000
#> 71      1.326000    1.4472001     1.567200
#> 72      1.394000    1.4608001     1.534800
#> 73      1.174000    1.2500001     1.328800
#> 74      1.312800    1.3812000     1.458000
#> 75      1.259600    1.3684001     1.483600
#> 76      1.302000    1.3964000     1.486000
#> 77      1.331600    1.4380001     1.498400
#> 78      1.376000    1.4384000     1.501600
#> 79      1.291200    1.3992000     1.514000
#> 80      1.372800    1.4072000     1.448400
#> 81      1.316400    1.4212000     1.526000
#> 82      1.360000    1.4500000     1.540000
#> 83      1.356800    1.4308000     1.503200
#> 84      1.320800    1.4124000     1.504400
#> 85      1.304000    1.4684000     1.548000
#> 86      1.320800    1.4444001     1.559200
#> 87      1.295200    1.3852000     1.480000
#> 88      1.241600    1.3212000     1.398000
#> 89      1.358800    1.4280000     1.500400
#> 90      1.358800    1.4280000     1.500400
#> 91      1.368400    1.4344000     1.500400
#> 92      1.200000    1.3000000     1.410000
#> 93      1.460000    1.5200000     1.580000
#> 94      1.242000    1.3568000     1.463200
#> 95      1.388000    1.4600000     1.538000
#> 96      1.069200    1.1356000     1.212000
#> 97      1.018000    1.0959999     1.173600
#> 98      1.046000    1.1592000     1.273200
#> 99      1.300400    1.3004000     1.429200
#> 100     1.309200    1.3800000     1.458000
#> 101     1.256000    1.3548000     1.453600
#> 102     1.307600    1.3792000     1.450800
#> 103     1.194400    1.2980001     1.402000
#> 104     1.328000    1.3976000     1.468000
#> 105     1.319600    1.4132000     1.506800
#> 106     1.286800    1.3763999     1.456800
#> 107     0.983600    1.0892000     1.197200
#> 108     1.010000    1.0804000     1.160000
#> 109     0.791600    0.8604000     1.020000
#> 110     0.816000    0.9200001     1.020000
#> 111     1.240000    1.3384000     1.436400
#> 112     1.111600    1.2708000     1.424800
#> 113     1.334000    1.4400000     1.546000
#> 114     1.157600    1.2244000     1.290400
#> 115     1.132400    1.3744000     1.478000
#> 116     1.230000    1.3520000     1.472000
#> 117     1.344800    1.4412000     1.537600
#> 118     1.060000    1.1100000     1.300000
#> 119     1.448000    1.4680000     1.484000
#> 120     1.374400    1.4468000     1.520800
#> 121     1.358800    1.4280000     1.500400
#> 122     1.320800    1.4444001     1.559200
#> 123     1.332800    1.4264000     1.526000
#> 124     1.392800    1.4680000     1.546000
#> 125     1.300400    1.3004000     1.429200
#> 126     1.496000    1.5640000     1.624000
#> 127     1.358800    1.4280000     1.500400
#> 128           NA           NA           NA
#> 129           NA           NA           NA
# }
```

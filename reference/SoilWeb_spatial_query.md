# Get SSURGO Data via Spatial Query

Get SSURGO Data via Spatial Query to SoilWeb

Data are currently available from SoilWeb. These data are a snapshot of
the "official" data. The snapshot date is encoded in the
"soilweb_last_update" column in the function return value. Planned
updates to this function will include a switch to determine the data
source: "official" data via USDA-NRCS servers, or a "snapshot" via
SoilWeb.

## Usage

``` r
SoilWeb_spatial_query(
  bbox = NULL,
  coords = NULL,
  what = "mapunit",
  source = "soilweb"
)
```

## Arguments

- bbox:

  a bounding box in WGS84 geographic coordinates, see examples

- coords:

  a coordinate pair in WGS84 geographic coordinates, see examples

- what:

  data to query, currently ignored

- source:

  the data source, currently ignored

## Value

The data returned from this function will depend on the query style. See
examples below.

## Note

SDA now supports spatial queries, consider using
[`SDA_spatialQuery()`](http://ncss-tech.github.io/soilDB/reference/SDA_spatialQuery.md)
instead.

## Author

D.E. Beaudette

## Examples

``` r
# \donttest{
    # query by bbox
    SoilWeb_spatial_query(bbox=c(-122.05, 37, -122, 37.05))
#>    area_ac areasymbol   mukey musym
#> 1   1123.9      ca087  455958   182
#> 2    749.0      ca087  455891   115
#> 3    586.7      ca087  455889   113
#> 4    523.9      ca087  455920   144
#> 5    459.8      ca087  455934   158
#> 6    267.7      ca087  455935   159
#> 7    231.4      ca087  455921   145
#> 8    175.0      ca087  455947   171
#> 9    165.9      ca087  455894   118
#> 10   150.3      ca087  455909   133
#> 11   149.7      ca087  455918   142
#> 12   131.9      ca087  455955   179
#> 13   117.3      ca087  455919   143
#> 14   107.1      ca087  455953   177
#> 15    84.9      ca087  455936   160
#> 16    83.8      ca087  455893   117
#> 17    83.8      ca087  455927   151
#> 18    73.9      ca087  455940   164
#> 19    73.0      ca087  455886   110
#> 20    72.3      ca087  455960   184
#> 21    66.2      ca087  455950   174
#> 22    57.3      ca087  455949   173
#> 23    54.5      ca087  455959   183
#> 24    47.2      ca087  455951   175
#> 25    43.7      ca087 2833423   130
#> 26    43.2      ca087  455892   116
#> 27    37.4      ca087 2833403   131
#> 28    33.6      ca087  455924   148
#> 29    31.2      ca087  455882   106
#> 30    28.2      ca087  455877   101
#> 31    26.6      ca087  455881   105
#> 32    25.6      ca087  455876   100
#> 33    25.3      ca087  455901   125
#> 34    22.2      ca087  455954   178
#> 35    19.8      ca087  455874   185
#> 36    19.1      ca087  455933   157
#> 37    17.2      ca087  455912   136
#> 38    16.8      ca087  455890   114
#> 39    15.4      ca087  455911   135
#> 40    14.2      ca087  455880   104
#> 41    11.7      ca087  455887   111
#> 42     7.0      ca087  455915   139
#> 43     5.9      ca087  455946   170
#> 44     5.0      ca087  455922   146
#> 45     4.7      ca087  455956   180
#> 46     3.5      ca087  455910   134
#> 47     3.0      ca087  455941   165
#> 48     1.4      ca087  455948   172
#>                                                                          muname
#> 1                                   Zayante coarse sand, 5 to 30 percent slopes
#> 2                            Ben Lomond-Felton complex, 50 to 75 percent slopes
#> 3                       Ben Lomond-Catelli-Sur complex, 30 to 75 percent slopes
#> 4                      Lompico-Felton complex, 50 to 75 percent slopes, MLRA 4B
#> 5                                 Nisene-Aptos complex, 50 to 75 percent slopes
#> 6                         Pfeiffer gravelly sandy loam, 15 to 30 percent slopes
#> 7                                  Lompico variant loam, 5 to 30 percent slopes
#> 8                                            Soquel loam, 2 to 9 percent slopes
#> 9                       Bonnydoon-Rock outcrop complex, 50 to 85 percent slopes
#> 10                                    Elkhorn sandy loam, 2 to 9 percent slopes
#> 11                               Lompico-Felton complex, 5 to 30 percent slopes
#> 12                      Watsonville loam, thick surface, 2 to 15 percent slopes
#> 13                     Lompico-Felton complex, 30 to 50 percent slopes, MLRA 4B
#> 14                                     Watsonville loam, 2 to 15 percent slopes
#> 15                        Pfeiffer gravelly sandy loam, 30 to 50 percent slopes
#> 16                                      Bonnydoon loam, 30 to 50 percent slopes
#> 17                                   Maymen stony loam, 30 to 75 percent slopes
#> 18                                                           Pits-Dumps complex
#> 19                                Ben Lomond sandy loam, 5 to 15 percent slopes
#> 20                        Zayante-Rock outcrop complex, 15 to 75 percent slopes
#> 21                          Tierra-Watsonville complex, 15 to 30 percent slopes
#> 22                                 Sur-Catelli complex, 50 to 75 percent slopes
#> 23                                 Zayante coarse sand, 30 to 50 percent slopes
#> 24                          Tierra-Watsonville complex, 30 to 50 percent slopes
#> 25                             Elder sandy loam, 2 to 9 percent slopes, MLRA 14
#> 26                              Bonnydoon loam, 5 to 50 percent slopes, MLRA 4B
#> 27                            Elder sandy loam, 9 to 15 percent slopes, MLRA 14
#> 28                                Los Osos loam, 30 to 50 percent slopes, moist
#> 29                                  Baywood loamy sand, 15 to 30 percent slopes
#> 30                                    Aptos loam, warm, 30 to 50 percent slopes
#> 31                                   Baywood loamy sand, 2 to 15 percent slopes
#> 32                                    Aptos loam, warm, 15 to 30 percent slopes
#> 33                                         Danville loam, 2 to 9 percent slopes
#> 34                       Watsonville loam, thick surface, 0 to 2 percent slopes
#> 35                                                                        Water
#> 36                                Nisene-Aptos complex, 30 to 50 percent slopes
#> 37                            Elkhorn-Pfeiffer complex, 30 to 50 percent slopes
#> 38                           Ben Lomond-Felton complex, 30 to 50 percent slopes
#> 39                                  Elkhorn sandy loam, 15 to 30 percent slopes
#> 40                                    Baywood loamy sand, 0 to 2 percent slopes
#> 41                               Ben Lomond sandy loam, 15 to 50 percent slopes
#> 42 Fluvaquentic Haploxerolls-Aquic Xerofluvents complex, 0 to 15 percent slopes
#> 43                                           Soquel loam, 0 to 2 percent slopes
#> 44                                        Los Osos loam, 5 to 15 percent slopes
#> 45                    Watsonville loam, thick surface, 15 to 30 percent slope s
#> 46                                   Elkhorn sandy loam, 9 to 15 percent slopes
#> 47                                                                    Riverwash
#> 48                                          Soquel loam, 9 to 15 percent slopes
#>    soilweb_last_update
#> 1           2025-09-02
#> 2           2025-09-02
#> 3           2025-09-02
#> 4           2025-09-02
#> 5           2025-09-02
#> 6           2025-09-02
#> 7           2025-09-02
#> 8           2025-09-02
#> 9           2025-09-02
#> 10          2025-09-02
#> 11          2025-09-02
#> 12          2025-09-02
#> 13          2025-09-02
#> 14          2025-09-02
#> 15          2025-09-02
#> 16          2025-09-02
#> 17          2025-09-02
#> 18          2025-09-02
#> 19          2025-09-02
#> 20          2025-09-02
#> 21          2025-09-02
#> 22          2025-09-02
#> 23          2025-09-02
#> 24          2025-09-02
#> 25          2025-09-02
#> 26          2025-09-02
#> 27          2025-09-02
#> 28          2025-09-02
#> 29          2025-09-02
#> 30          2025-09-02
#> 31          2025-09-02
#> 32          2025-09-02
#> 33          2025-09-02
#> 34          2025-09-02
#> 35          2025-09-02
#> 36          2025-09-02
#> 37          2025-09-02
#> 38          2025-09-02
#> 39          2025-09-02
#> 40          2025-09-02
#> 41          2025-09-02
#> 42          2025-09-02
#> 43          2025-09-02
#> 44          2025-09-02
#> 45          2025-09-02
#> 46          2025-09-02
#> 47          2025-09-02
#> 48          2025-09-02
    
    # query by coordinate pair
    SoilWeb_spatial_query(coords=c(-121, 38))
#>   ogc_fid areasymbol musym  mukey soilweb_last_update dist_meters
#> 1 1524073      ca077   220 462112          2025-09-09 92.17456302
# }
```

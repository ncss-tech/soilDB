# Get mapunit ecological sites from Soil Data Access

`get_SDA_coecoclass()` retrieves ecological site information from the
Soil Data Access (SDA) database for a given set of map unit keys
(mukeys). It returns a data frame containing ecological site IDs, names,
and associated classification details, enabling users to link soil map
units to ecological site concepts used in land management and
conservation planning.

## Usage

``` r
get_SDA_coecoclass(
  method = "None",
  areasymbols = NULL,
  mukeys = NULL,
  WHERE = NULL,
  query_string = FALSE,
  ecoclasstypename = c("NRCS Rangeland Site", "NRCS Forestland Site"),
  ecoclassref = "Ecological Site Description Database",
  not_rated_value = "Not assigned",
  miscellaneous_areas = TRUE,
  include_minors = TRUE,
  threshold = 0,
  dsn = NULL
)
```

## Arguments

- method:

  aggregation method. One of: `"Dominant Component"`,
  `"Dominant Condition"`, `"All"` or `"None"` (default). If
  `method="all"` multiple numbered columns represent site composition
  within each map unit e.g. `site1...`, `site2...`. If `method="none"`
  is selected one row will be returned per *component*; in all other
  cases one row will be returned per *map unit*.

- areasymbols:

  vector of soil survey area symbols

- mukeys:

  vector of map unit keys

- WHERE:

  character containing SQL WHERE clause specified in terms of fields in
  `legend`, `mapunit`, `component` or `coecosite` tables, used in lieu
  of `mukeys` or `areasymbols`

- query_string:

  Default: `FALSE`; if `TRUE` return a character string containing query
  that would be sent to SDA via `SDA_query`

- ecoclasstypename:

  Default: `c("NRCS Rangeland Site", "NRCS Forestland Site")`. If `NULL`
  no constraint on `ecoclasstypename` is used in the query.

- ecoclassref:

  Default: `"Ecological Site Description Database"`. If `NULL` no
  constraint on `ecoclassref` is used in the query.

- not_rated_value:

  Default: `"Not assigned"`

- miscellaneous_areas:

  logical. Include miscellaneous areas (non-soil components)?

- include_minors:

  logical. Include minor components? Default: `TRUE`.

- threshold:

  integer. Default: `0`. Minimum combined component percentage (RV) for
  inclusion of a mapunit's ecological site in wide-format tabular
  summary. Used only for `method="all"`.

- dsn:

  Path to local SQLite database or a DBIConnection object. If `NULL`
  (default) use Soil Data Access API via
  [`SDA_query()`](http://ncss-tech.github.io/soilDB/reference/SDA_query.md).

## Details

When `method="Dominant Condition"` an additional field `ecoclasspct_r`
is returned in the result with the sum of `comppct_r` that have the
dominant condition `ecoclassid`. The component with the greatest
`comppct_r` is returned for the `component` and `coecosite` level
information.

Note that if there are multiple `coecoclasskey` per `ecoclassid` there
may be more than one record per component.

## Examples

``` r
# Basic usage with a vector of mukeys
get_SDA_coecoclass(mukeys = c(463994, 463995))
#> single result set, returning a data.frame
#>     mukey areasymbol  lkey                                             muname
#> 1  463994      CA671 14129        WRIGHTWOOD-BULL TRAIL ASSOCIATION, SLOPING*
#> 2  463994      CA671 14129        WRIGHTWOOD-BULL TRAIL ASSOCIATION, SLOPING*
#> 3  463994      CA671 14129        WRIGHTWOOD-BULL TRAIL ASSOCIATION, SLOPING*
#> 4  463994      CA671 14129        WRIGHTWOOD-BULL TRAIL ASSOCIATION, SLOPING*
#> 5  463994      CA671 14129        WRIGHTWOOD-BULL TRAIL ASSOCIATION, SLOPING*
#> 6  463994      CA671 14129        WRIGHTWOOD-BULL TRAIL ASSOCIATION, SLOPING*
#> 7  463995      CA671 14129 YERMO GRAVELLY SANDY LOAM, 30 TO 50 PERCENT SLOPES
#> 8  463995      CA671 14129 YERMO GRAVELLY SANDY LOAM, 30 TO 50 PERCENT SLOPES
#> 9  463995      CA671 14129 YERMO GRAVELLY SANDY LOAM, 30 TO 50 PERCENT SLOPES
#> 10 463995      CA671 14129 YERMO GRAVELLY SANDY LOAM, 30 TO 50 PERCENT SLOPES
#>       cokey coecoclasskey comppct_r majcompflag     compname localphase
#> 1  27410839      12765565        60         Yes   Wrightwood       <NA>
#> 2  27410840      12765566        25         Yes   Bull Trail       <NA>
#> 3  27410841            NA         5         No  Haploxeralfs       <NA>
#> 4  27410842            NA         5         No       Avawatz       <NA>
#> 5  27410843            NA         3         No       Hanford       <NA>
#> 6  27410844            NA         2         No       Tujunga       <NA>
#> 7  27410845      12765567        85         Yes        Yermo       <NA>
#> 8  27410846            NA         5         No       Trigger      soils
#> 9  27410847            NA         5         No        Joshua       <NA>
#> 10 27410848            NA         5         No  Rock outcrop       <NA>
#>              compkind   ecoclassid          ecoclassname    ecoclasstypename
#> 1              Series  R019XE018CA                 SANDY NRCS Rangeland Site
#> 2              Series  R019XE003CA          COARSE LOAMY NRCS Rangeland Site
#> 3  Taxon above family Not assigned          Not assigned        Not assigned
#> 4              Series Not assigned          Not assigned        Not assigned
#> 5              Series Not assigned          Not assigned        Not assigned
#> 6              Series Not assigned          Not assigned        Not assigned
#> 7              Series  R030XF025CA GRAVELLY COARSE LOAMY NRCS Rangeland Site
#> 8              Series Not assigned          Not assigned        Not assigned
#> 9              Series Not assigned          Not assigned        Not assigned
#> 10 Miscellaneous area Not assigned          Not assigned        Not assigned
#>                             ecoclassref
#> 1  Ecological Site Description Database
#> 2  Ecological Site Description Database
#> 3                          Not assigned
#> 4                          Not assigned
#> 5                          Not assigned
#> 6                          Not assigned
#> 7  Ecological Site Description Database
#> 8                          Not assigned
#> 9                          Not assigned
#> 10                         Not assigned

# Using a custom WHERE clause (all "range" sites in Hawaii)
get_SDA_coecoclass(WHERE = "ecoclassid LIKE 'R%' AND areasymbol LIKE 'HI%'")
#> single result set, returning a data.frame
#>       mukey areasymbol  lkey
#> 1   2371903      HI701 20281
#> 2   2371935      HI701 20281
#> 3   2371936      HI701 20281
#> 4   2371965      HI701 20281
#> 5   2371966      HI701 20281
#> 6   2371971      HI701 20281
#> 7   2371979      HI701 20281
#> 8   2371985      HI701 20281
#> 9   2371986      HI701 20281
#> 10  2372076      HI701 20281
#> 11  2372077      HI701 20281
#> 12  2372078      HI701 20281
#> 13  2372080      HI701 20281
#> 14  2372081      HI701 20281
#> 15  2372082      HI701 20281
#> 16  2372125      HI701 20281
#> 17  2372140      HI701 20281
#> 18  2372170      HI701 20281
#> 19  2372171      HI701 20281
#> 20  2372177      HI701 20281
#> 21  2372226      HI701 20281
#> 22  2372189      HI701 20281
#> 23  2372191      HI701 20281
#> 24  2372191      HI701 20281
#> 25  2372192      HI701 20281
#> 26  2372192      HI701 20281
#> 27  2372193      HI701 20281
#> 28  2372194      HI701 20281
#> 29  2372195      HI701 20281
#> 30  2372196      HI701 20281
#> 31  2372197      HI701 20281
#> 32  2372198      HI701 20281
#> 33  2372199      HI701 20281
#> 34  2372199      HI701 20281
#> 35  2372199      HI701 20281
#> 36  2372199      HI701 20281
#> 37  2372200      HI701 20281
#> 38  2372200      HI701 20281
#> 39  2372205      HI701 20281
#> 40  2372206      HI701 20281
#> 41  2372209      HI701 20281
#> 42  2372210      HI701 20281
#> 43  2372211      HI701 20281
#> 44  2372212      HI701 20281
#> 45  2372213      HI701 20281
#> 46  2372213      HI701 20281
#> 47  2372216      HI701 20281
#> 48  2372217      HI701 20281
#> 49  2372218      HI701 20281
#> 50  2372220      HI701 20281
#> 51  2372221      HI701 20281
#> 52  2372225      HI701 20281
#> 53  2372225      HI701 20281
#> 54  2371586      HI801 20280
#> 55  2371593      HI801 20280
#> 56  2371618      HI801 20280
#> 57  2371619      HI801 20280
#> 58  2371648      HI801 20280
#> 59  2371649      HI801 20280
#> 60  2371650      HI801 20280
#> 61  2371651      HI801 20280
#> 62  2371652      HI801 20280
#> 63  2371222      HI801 20280
#> 64  2371654      HI801 20280
#> 65  2371655      HI801 20280
#> 66  2371656      HI801 20280
#> 67  2371659      HI801 20280
#> 68  2371223      HI801 20280
#> 69  2371228      HI801 20280
#> 70  2371229      HI801 20280
#> 71  2371719      HI801 20280
#> 72  2371720      HI801 20280
#> 73  2401297      HI801 20280
#> 74  2371721      HI801 20280
#> 75  2371722      HI801 20280
#> 76  2371723      HI801 20280
#> 77  2371724      HI801 20280
#> 78  2371725      HI801 20280
#> 79  2371794      HI801 20280
#> 80  2371795      HI801 20280
#> 81  2371796      HI801 20280
#> 82  2371796      HI801 20280
#> 83  2371797      HI801 20280
#> 84  2371798      HI801 20280
#> 85  2371798      HI801 20280
#> 86  2371799      HI801 20280
#> 87  2371729      HI801 20280
#> 88  2371801      HI801 20280
#> 89  2371730      HI801 20280
#> 90  2371803      HI801 20280
#> 91  2371804      HI801 20280
#> 92  2371807      HI801 20280
#> 93  2371807      HI801 20280
#> 94  2371808      HI801 20280
#> 95  2371808      HI801 20280
#> 96  2371810      HI801 20280
#> 97  2371810      HI801 20280
#> 98  2371810      HI801 20280
#> 99  2371811      HI801 20280
#> 100 2371811      HI801 20280
#> 101 2371812      HI801 20280
#> 102 2371813      HI801 20280
#> 103 2371814      HI801 20280
#> 104 2371815      HI801 20280
#> 105 2371816      HI801 20280
#> 106 2371817      HI801 20280
#> 107 2371818      HI801 20280
#> 108 2371819      HI801 20280
#> 109 2371820      HI801 20280
#> 110 2371748      HI801 20280
#> 111 2371242      HI801 20280
#> 112 2371243      HI801 20280
#> 113 2371244      HI801 20280
#> 114 2371245      HI801 20280
#> 115 2371246      HI801 20280
#> 116 2371247      HI801 20280
#> 117 2371248      HI801 20280
#> 118 2371750      HI801 20280
#> 119 2371821      HI801 20280
#> 120 2371822      HI801 20280
#> 121 2371823      HI801 20280
#> 122 2371824      HI801 20280
#> 123 2371825      HI801 20280
#> 124 2371826      HI801 20280
#> 125 2371827      HI801 20280
#> 126 2371828      HI801 20280
#> 127 2371829      HI801 20280
#> 128 2371829      HI801 20280
#> 129 2401411      HI801 20280
#> 130 2401412      HI801 20280
#> 131 2401413      HI801 20280
#> 132 2401414      HI801 20280
#> 133 2401415      HI801 20280
#> 134 2401416      HI801 20280
#> 135 2401417      HI801 20280
#> 136 2452572      HI801 20280
#> 137 2452573      HI801 20280
#> 138 2452574      HI801 20280
#> 139 2371765      HI801 20280
#> 140 2371766      HI801 20280
#> 141 2452576      HI801 20280
#> 142 2371767      HI801 20280
#> 143 2371768      HI801 20280
#> 144 2452577      HI801 20280
#> 145 2371770      HI801 20280
#> 146 2401428      HI801 20280
#> 147 2371249      HI801 20280
#> 148 2371250      HI801 20280
#> 149 2371251      HI801 20280
#> 150 2371252      HI801 20280
#> 151 2371252      HI801 20280
#> 152 2371254      HI801 20280
#> 153 2371255      HI801 20280
#> 154 2371255      HI801 20280
#> 155 2371264      HI801 20280
#> 156 2371279      HI801 20280
#> 157 2371280      HI801 20280
#> 158 2371287      HI801 20280
#> 159 2371309      HI801 20280
#> 160 2371310      HI801 20280
#> 161 2371316      HI801 20280
#> 162 2371320      HI801 20280
#> 163 2371322      HI801 20280
#> 164 2371331      HI801 20280
#> 165 2371341      HI801 20280
#> 166 2371342      HI801 20280
#> 167 2371342      HI801 20280
#> 168 2371345      HI801 20280
#> 169 2371345      HI801 20280
#> 170 2371349      HI801 20280
#> 171 2371373      HI801 20280
#> 172 2371876      HI801 20280
#> 173 2371374      HI801 20280
#> 174 2371877      HI801 20280
#> 175 2371878      HI801 20280
#> 176 2495917      HI801 20280
#> 177 2495917      HI801 20280
#> 178 2384139      HI801 20280
#> 179 2375673      HI801 20280
#> 180 2371378      HI801 20280
#> 181 2371378      HI801 20280
#> 182 2371381      HI801 20280
#> 183 2371382      HI801 20280
#> 184 2371882      HI801 20280
#> 185 2371884      HI801 20280
#> 186 2371839      HI801 20280
#> 187 2371839      HI801 20280
#> 188 2371839      HI801 20280
#> 189 2371840      HI801 20280
#> 190 2371840      HI801 20280
#> 191 2495912      HI801 20280
#> 192 2375448      HI801 20280
#> 193 2371858      HI801 20280
#> 194 2371859      HI801 20280
#> 195 2371861      HI801 20280
#> 196 2371867      HI801 20280
#> 197 2371868      HI801 20280
#> 198 2371868      HI801 20280
#> 199 2421571      HI801 20280
#> 200 2421571      HI801 20280
#> 201 2424226      HI801 20280
#> 202 2424227      HI801 20280
#> 203 2424228      HI801 20280
#> 204 2431435      HI801 20280
#> 205 2431209      HI801 20280
#> 206 2431216      HI801 20280
#> 207 2452925      HI801 20280
#> 208 2437839      HI801 20280
#> 209 2452926      HI801 20280
#> 210 2452927      HI801 20280
#> 211 2452586      HI801 20280
#> 212 2478619      HI801 20280
#> 213 2512231      HI801 20280
#> 214  468142      HI950 14193
#> 215  468139      HI950 14193
#> 216  468140      HI950 14193
#> 217  468141      HI950 14193
#> 218  468148      HI950 14193
#> 219  468149      HI950 14193
#> 220  468146      HI950 14193
#> 221  468147      HI950 14193
#> 222  468150      HI950 14193
#> 223  468151      HI950 14193
#> 224  468152      HI950 14193
#> 225  468153      HI950 14193
#> 226  468154      HI950 14193
#> 227  468155      HI950 14193
#> 228  468155      HI950 14193
#> 229  468156      HI950 14193
#> 230  468157      HI950 14193
#> 231  468158      HI950 14193
#> 232  468159      HI950 14193
#> 233  468161      HI950 14193
#> 234  468160      HI950 14193
#> 235  468167      HI950 14193
#> 236  468168      HI950 14193
#> 237  468169      HI950 14193
#> 238  468170      HI950 14193
#> 239  468171      HI950 14193
#> 240  468163      HI950 14193
#> 241  468164      HI950 14193
#> 242  468172      HI950 14193
#> 243  468173      HI950 14193
#> 244  468174      HI950 14193
#> 245  468175      HI950 14193
#> 246  468165      HI950 14193
#> 247  468165      HI950 14193
#> 248  468166      HI950 14193
#> 249  468176      HI950 14193
#> 250  468177      HI950 14193
#> 251  468178      HI950 14193
#> 252  468179      HI950 14193
#> 253  468180      HI950 14193
#> 254  468181      HI950 14193
#> 255  468182      HI950 14193
#> 256  468182      HI950 14193
#> 257  468184      HI950 14193
#> 258  468185      HI950 14193
#> 259  468186      HI950 14193
#> 260  468187      HI950 14193
#> 261  468188      HI950 14193
#> 262  468189      HI950 14193
#> 263  468190      HI950 14193
#> 264  468192      HI950 14193
#> 265  468192      HI950 14193
#> 266  468192      HI950 14193
#> 267  468192      HI950 14193
#> 268  468193      HI950 14193
#> 269  468194      HI950 14193
#> 270  468195      HI950 14193
#> 271  468196      HI950 14193
#> 272  468197      HI950 14193
#> 273  468197      HI950 14193
#> 274  468198      HI950 14193
#> 275  468198      HI950 14193
#> 276  468200      HI950 14193
#> 277  468201      HI950 14193
#> 278  468202      HI950 14193
#> 279  468204      HI950 14193
#> 280  468203      HI950 14193
#> 281  468205      HI950 14193
#> 282  468207      HI950 14193
#> 283  468208      HI950 14193
#> 284  468209      HI950 14193
#> 285  468210      HI950 14193
#> 286  468211      HI950 14193
#> 287  468212      HI950 14193
#> 288  468213      HI950 14193
#> 289  467800      HI960 14194
#> 290  467807      HI960 14194
#> 291  467808      HI960 14194
#> 292  467809      HI960 14194
#> 293  467810      HI960 14194
#> 294  467811      HI960 14194
#> 295  467811      HI960 14194
#> 296  467804      HI960 14194
#> 297  467812      HI960 14194
#> 298  467813      HI960 14194
#> 299  467814      HI960 14194
#> 300  467815      HI960 14194
#> 301  467816      HI960 14194
#> 302  467817      HI960 14194
#> 303  467818      HI960 14194
#> 304  467819      HI960 14194
#> 305  467820      HI960 14194
#> 306  467821      HI960 14194
#> 307  467822      HI960 14194
#> 308  467823      HI960 14194
#> 309  467824      HI960 14194
#> 310  467825      HI960 14194
#> 311  467826      HI960 14194
#> 312  467835      HI960 14194
#> 313  467836      HI960 14194
#> 314  467837      HI960 14194
#> 315  467838      HI960 14194
#> 316  467839      HI960 14194
#> 317  467840      HI960 14194
#> 318  467827      HI960 14194
#> 319  467841      HI960 14194
#> 320  467841      HI960 14194
#> 321  467842      HI960 14194
#> 322  467842      HI960 14194
#> 323  467843      HI960 14194
#> 324  467844      HI960 14194
#> 325  467845      HI960 14194
#> 326  467846      HI960 14194
#> 327  467847      HI960 14194
#> 328  467848      HI960 14194
#> 329  467849      HI960 14194
#> 330  467828      HI960 14194
#> 331  467831      HI960 14194
#> 332  467850      HI960 14194
#> 333  467851      HI960 14194
#> 334  467852      HI960 14194
#> 335  467833      HI960 14194
#> 336  467853      HI960 14194
#> 337  467855      HI960 14194
#> 338  467856      HI960 14194
#> 339  467857      HI960 14194
#> 340  467858      HI960 14194
#> 341  467859      HI960 14194
#> 342  467860      HI960 14194
#> 343  467861      HI960 14194
#> 344  467862      HI960 14194
#> 345  467863      HI960 14194
#> 346  467854      HI960 14194
#> 347  467864      HI960 14194
#> 348  467864      HI960 14194
#> 349  467865      HI960 14194
#> 350  467867      HI960 14194
#> 351  467868      HI960 14194
#> 352  467869      HI960 14194
#> 353  467870      HI960 14194
#> 354  467871      HI960 14194
#> 355  467872      HI960 14194
#> 356  467873      HI960 14194
#> 357  467874      HI960 14194
#> 358  467875      HI960 14194
#> 359  467876      HI960 14194
#> 360  467877      HI960 14194
#> 361  467878      HI960 14194
#> 362  467879      HI960 14194
#> 363  467880      HI960 14194
#> 364  467881      HI960 14194
#> 365  467882      HI960 14194
#> 366  467883      HI960 14194
#> 367  467884      HI960 14194
#> 368  467885      HI960 14194
#> 369  467886      HI960 14194
#> 370  467887      HI960 14194
#> 371  467888      HI960 14194
#> 372  467889      HI960 14194
#> 373  467890      HI960 14194
#> 374  467891      HI960 14194
#> 375  467892      HI960 14194
#> 376  467893      HI960 14194
#> 377  467896      HI960 14194
#> 378  467894      HI960 14194
#> 379  467894      HI960 14194
#> 380  467895      HI960 14194
#> 381  467895      HI960 14194
#> 382  467900      HI960 14194
#> 383  467901      HI960 14194
#> 384  467899      HI960 14194
#> 385  467902      HI960 14194
#> 386  467903      HI960 14194
#> 387  467904      HI960 14194
#> 388  467905      HI960 14194
#> 389  467906      HI960 14194
#> 390  467907      HI960 14194
#> 391  467908      HI960 14194
#> 392  467909      HI960 14194
#> 393  467910      HI960 14194
#> 394  467911      HI960 14194
#> 395  467912      HI960 14194
#> 396  467913      HI960 14194
#> 397  467914      HI960 14194
#> 398  467915      HI960 14194
#> 399  467916      HI960 14194
#> 400  467918      HI960 14194
#> 401  467919      HI960 14194
#> 402  467920      HI960 14194
#> 403  467921      HI960 14194
#> 404  467987      HI970 14195
#> 405  467932      HI970 14195
#> 406  467935      HI970 14195
#> 407  467936      HI970 14195
#> 408  467937      HI970 14195
#> 409  467938      HI970 14195
#> 410  467939      HI970 14195
#> 411  467940      HI970 14195
#> 412  467941      HI970 14195
#> 413  467942      HI970 14195
#> 414  467943      HI970 14195
#> 415  467944      HI970 14195
#> 416  467934      HI970 14195
#> 417  467945      HI970 14195
#> 418  467946      HI970 14195
#> 419  467947      HI970 14195
#> 420  467948      HI970 14195
#> 421  467949      HI970 14195
#> 422  467950      HI970 14195
#> 423  467951      HI970 14195
#> 424  467952      HI970 14195
#> 425  467952      HI970 14195
#> 426  467953      HI970 14195
#> 427  467954      HI970 14195
#> 428  467955      HI970 14195
#> 429  467956      HI970 14195
#> 430  467957      HI970 14195
#> 431  467958      HI970 14195
#> 432  467960      HI970 14195
#> 433  467960      HI970 14195
#> 434  467960      HI970 14195
#> 435  467960      HI970 14195
#> 436  467962      HI970 14195
#> 437  467963      HI970 14195
#> 438  467965      HI970 14195
#> 439  467964      HI970 14195
#> 440  467966      HI970 14195
#> 441  467967      HI970 14195
#> 442  467968      HI970 14195
#> 443  467969      HI970 14195
#> 444  467971      HI970 14195
#> 445  467972      HI970 14195
#> 446  467973      HI970 14195
#> 447  467974      HI970 14195
#> 448  467975      HI970 14195
#> 449  467976      HI970 14195
#> 450  467977      HI970 14195
#> 451  468228      HI980 14196
#> 452  468229      HI980 14196
#> 453  468230      HI980 14196
#> 454  468231      HI980 14196
#> 455  468232      HI980 14196
#> 456  468233      HI980 14196
#> 457  468236      HI980 14196
#> 458  468237      HI980 14196
#> 459  468238      HI980 14196
#> 460  468239      HI980 14196
#> 461  468240      HI980 14196
#> 462  468241      HI980 14196
#> 463  468248      HI980 14196
#> 464  468249      HI980 14196
#> 465  468250      HI980 14196
#> 466  468251      HI980 14196
#> 467  468252      HI980 14196
#> 468  468253      HI980 14196
#> 469  468254      HI980 14196
#> 470  468254      HI980 14196
#> 471  468254      HI980 14196
#> 472  468255      HI980 14196
#> 473  468243      HI980 14196
#> 474  468256      HI980 14196
#> 475  468244      HI980 14196
#> 476  468245      HI980 14196
#> 477  468246      HI980 14196
#> 478  468247      HI980 14196
#> 479  468257      HI980 14196
#> 480  468258      HI980 14196
#> 481  468259      HI980 14196
#> 482  468260      HI980 14196
#> 483  468261      HI980 14196
#> 484  468263      HI980 14196
#> 485  468264      HI980 14196
#> 486  468265      HI980 14196
#> 487  468266      HI980 14196
#> 488  468267      HI980 14196
#> 489  468268      HI980 14196
#> 490  468262      HI980 14196
#> 491  468269      HI980 14196
#> 492  468270      HI980 14196
#> 493  468282      HI980 14196
#> 494  468283      HI980 14196
#> 495  468284      HI980 14196
#> 496  468271      HI980 14196
#> 497  468272      HI980 14196
#> 498  468275      HI980 14196
#> 499  468276      HI980 14196
#> 500  468277      HI980 14196
#> 501  468278      HI980 14196
#> 502  468279      HI980 14196
#> 503  468279      HI980 14196
#> 504  468287      HI980 14196
#> 505  468288      HI980 14196
#> 506  468289      HI980 14196
#> 507  468285      HI980 14196
#> 508  468290      HI980 14196
#> 509  468286      HI980 14196
#> 510  468291      HI980 14196
#> 511  468292      HI980 14196
#> 512  468293      HI980 14196
#> 513  468280      HI980 14196
#> 514  468281      HI980 14196
#> 515  468296      HI980 14196
#> 516  468297      HI980 14196
#> 517  468294      HI980 14196
#> 518  468295      HI980 14196
#> 519  468301      HI980 14196
#> 520  468302      HI980 14196
#> 521  468303      HI980 14196
#> 522  468309      HI980 14196
#> 523  468309      HI980 14196
#> 524  468309      HI980 14196
#> 525  468310      HI980 14196
#> 526  468304      HI980 14196
#> 527  468305      HI980 14196
#> 528  468311      HI980 14196
#> 529  468312      HI980 14196
#> 530  468313      HI980 14196
#> 531  468306      HI980 14196
#> 532  468307      HI980 14196
#> 533  468307      HI980 14196
#> 534  468307      HI980 14196
#> 535  468308      HI980 14196
#> 536  468314      HI980 14196
#> 537  468314      HI980 14196
#> 538  468314      HI980 14196
#> 539  468315      HI980 14196
#> 540  468316      HI980 14196
#> 541  468317      HI980 14196
#> 542  468318      HI980 14196
#> 543  468319      HI980 14196
#> 544  468320      HI980 14196
#> 545  468326      HI980 14196
#> 546  468327      HI980 14196
#> 547  468328      HI980 14196
#> 548  468329      HI980 14196
#> 549  468329      HI980 14196
#> 550  468329      HI980 14196
#> 551  468330      HI980 14196
#> 552  468331      HI980 14196
#> 553  468332      HI980 14196
#> 554  468333      HI980 14196
#> 555  468334      HI980 14196
#> 556  468335      HI980 14196
#> 557  468336      HI980 14196
#> 558  468337      HI980 14196
#> 559  468338      HI980 14196
#> 560  468323      HI980 14196
#> 561  468324      HI980 14196
#> 562  468324      HI980 14196
#> 563  468324      HI980 14196
#> 564  468325      HI980 14196
#> 565  468340      HI980 14196
#> 566  468343      HI980 14196
#> 567  468346      HI980 14196
#> 568  468347      HI980 14196
#> 569  468348      HI980 14196
#> 570  468349      HI980 14196
#> 571  468350      HI980 14196
#> 572  468351      HI980 14196
#> 573  468352      HI980 14196
#> 574  468353      HI980 14196
#> 575  468354      HI980 14196
#> 576  468355      HI980 14196
#> 577  468356      HI980 14196
#> 578  468345      HI980 14196
#> 579  468357      HI980 14196
#> 580  468358      HI980 14196
#> 581  468358      HI980 14196
#> 582  468358      HI980 14196
#> 583  468359      HI980 14196
#> 584  468360      HI980 14196
#> 585  468361      HI980 14196
#> 586  468362      HI980 14196
#> 587  468363      HI980 14196
#> 588  468379      HI990 14197
#> 589  468378      HI990 14197
#> 590  468381      HI990 14197
#> 591  468382      HI990 14197
#> 592  468383      HI990 14197
#> 593  468384      HI990 14197
#> 594  468385      HI990 14197
#> 595  468386      HI990 14197
#> 596  468387      HI990 14197
#> 597  468388      HI990 14197
#> 598  468394      HI990 14197
#> 599  468395      HI990 14197
#> 600  468391      HI990 14197
#> 601  468392      HI990 14197
#> 602  468393      HI990 14197
#> 603  468396      HI990 14197
#> 604  468397      HI990 14197
#> 605  468398      HI990 14197
#> 606  468399      HI990 14197
#> 607  468400      HI990 14197
#> 608  468401      HI990 14197
#> 609  468402      HI990 14197
#> 610  468410      HI990 14197
#> 611  468411      HI990 14197
#> 612  468412      HI990 14197
#> 613  468413      HI990 14197
#> 614  468414      HI990 14197
#> 615  468415      HI990 14197
#> 616  468416      HI990 14197
#> 617  468416      HI990 14197
#> 618  468417      HI990 14197
#> 619  468417      HI990 14197
#> 620  468418      HI990 14197
#> 621  468419      HI990 14197
#> 622  468403      HI990 14197
#> 623  468404      HI990 14197
#> 624  468405      HI990 14197
#> 625  468406      HI990 14197
#> 626  468407      HI990 14197
#> 627  468420      HI990 14197
#> 628  468423      HI990 14197
#> 629  468424      HI990 14197
#> 630  468421      HI990 14197
#> 631  468425      HI990 14197
#> 632  468422      HI990 14197
#> 633  468426      HI990 14197
#> 634  468426      HI990 14197
#> 635  468427      HI990 14197
#> 636  468428      HI990 14197
#> 637  468429      HI990 14197
#> 638  468430      HI990 14197
#> 639  468431      HI990 14197
#> 640  468432      HI990 14197
#> 641  468433      HI990 14197
#> 642  468408      HI990 14197
#> 643  468434      HI990 14197
#> 644  468435      HI990 14197
#> 645  468436      HI990 14197
#> 646  468437      HI990 14197
#> 647  468409      HI990 14197
#> 648  468438      HI990 14197
#> 649  468439      HI990 14197
#> 650  468440      HI990 14197
#> 651  468441      HI990 14197
#> 652  468442      HI990 14197
#> 653  468443      HI990 14197
#> 654  468445      HI990 14197
#> 655  468446      HI990 14197
#> 656  468447      HI990 14197
#> 657  468448      HI990 14197
#> 658  468449      HI990 14197
#> 659  468450      HI990 14197
#> 660  468451      HI990 14197
#> 661  468451      HI990 14197
#> 662  468451      HI990 14197
#> 663  468452      HI990 14197
#> 664  468453      HI990 14197
#> 665  468454      HI990 14197
#> 666  468455      HI990 14197
#> 667  468444      HI990 14197
#> 668  468456      HI990 14197
#> 669  468456      HI990 14197
#> 670  468457      HI990 14197
#> 671  468458      HI990 14197
#> 672  468459      HI990 14197
#> 673  468460      HI990 14197
#> 674  468462      HI990 14197
#> 675  468463      HI990 14197
#> 676  468464      HI990 14197
#> 677  468465      HI990 14197
#> 678  468465      HI990 14197
#> 679  468466      HI990 14197
#> 680  468467      HI990 14197
#> 681  468468      HI990 14197
#> 682  468469      HI990 14197
#> 683  468470      HI990 14197
#> 684  468471      HI990 14197
#> 685  468472      HI990 14197
#> 686  468473      HI990 14197
#> 687  468474      HI990 14197
#> 688  468475      HI990 14197
#> 689  468476      HI990 14197
#> 690  468477      HI990 14197
#> 691  468478      HI990 14197
#> 692  468479      HI990 14197
#> 693  468480      HI990 14197
#> 694  468481      HI990 14197
#> 695  468482      HI990 14197
#> 696  468483      HI990 14197
#> 697  468484      HI990 14197
#> 698  468485      HI990 14197
#> 699  468491      HI990 14197
#> 700  468491      HI990 14197
#> 701  468491      HI990 14197
#> 702  468492      HI990 14197
#> 703  468493      HI990 14197
#> 704  468494      HI990 14197
#> 705  468495      HI990 14197
#> 706  468496      HI990 14197
#> 707  468497      HI990 14197
#> 708  468498      HI990 14197
#> 709  468486      HI990 14197
#> 710  468499      HI990 14197
#> 711  468499      HI990 14197
#> 712  468499      HI990 14197
#> 713  468500      HI990 14197
#> 714  468501      HI990 14197
#> 715  468502      HI990 14197
#> 716  468503      HI990 14197
#> 717  468487      HI990 14197
#> 718  468488      HI990 14197
#> 719  468489      HI990 14197
#> 720  468490      HI990 14197
#> 721  468504      HI990 14197
#> 722  468505      HI990 14197
#> 723  468506      HI990 14197
#> 724  468507      HI990 14197
#> 725  468510      HI990 14197
#> 726  468511      HI990 14197
#> 727  468512      HI990 14197
#> 728  468513      HI990 14197
#> 729  468514      HI990 14197
#> 730  468515      HI990 14197
#> 731  468516      HI990 14197
#> 732  468517      HI990 14197
#> 733  468518      HI990 14197
#> 734  468519      HI990 14197
#> 735  468525      HI990 14197
#> 736  468520      HI990 14197
#> 737  468521      HI990 14197
#> 738  468522      HI990 14197
#> 739  468522      HI990 14197
#> 740  468522      HI990 14197
#> 741  468522      HI990 14197
#> 742  468523      HI990 14197
#> 743  468524      HI990 14197
#> 744  468526      HI990 14197
#> 745  468527      HI990 14197
#> 746  468528      HI990 14197
#>                                                                                         muname
#> 1                                              Lava flows association, 40 to 99 percent slopes
#> 2                                             Mawae-Lava flows complex, 2 to 10 percent slopes
#> 3                                            Mawae-Lava flows complex, 10 to 20 percent slopes
#> 4                                           Lava flows-Kekake complex, 10 to 20 percent slopes
#> 5                                            Lava flows-Kekake complex, 2 to 20 percent slopes
#> 6                                    Iwalani-Lava flows complex, moist, 2 to 10 percent slopes
#> 7                                   Iwalani-Lava flows complex, moist, 10 to 20 percent slopes
#> 8                          Ihuanu very cobbly medial silt loam, moist, 10 to 20 percent slopes
#> 9                           Ihuanu very cobbly medial silt loam, moist, 2 to 10 percent slopes
#> 10                          Ihuanu-Lava flows, `a`a complex, 10 to 20 percent slopes, MLRA 160
#> 11                      Ihuanu very cobbly medial silt loam, 10 to 20 percent slopes, MLRA 160
#> 12                       Ihuanu very cobbly medial silt loam, 2 to 10 percent slopes, MLRA 160
#> 13                                         Lava flows-Iwalani complex, 10 to 20 percent slopes
#> 14                                          Iwalani-Lava flows complex, 2 to 10 percent slopes
#> 15                                         Iwalani-Lava flows complex, 10 to 20 percent slopes
#> 16                 Lalaau very cobbly highly decomposed plant material, 2 to 10 percent slopes
#> 17                                          Kahaluu-Lava flows complex, 2 to 10 percent slopes
#> 18                      Ihuanu-Lava flows, pahoehoe complex, 10 to 20 percent slopes, MLRA 160
#> 19                                              Durustands medial loam, 2 to 10 percent slopes
#> 20                                                 Kaholimo-Ki complex, 3 to 10 percent slopes
#> 21                                                Kaholimo-Ki complex, 10 to 20 percent slopes
#> 22                             Lava flows-Lithic Ustipsamments complex, 2 to 10 percent slopes
#> 23                               Lithic Haplustands-Lava flows complex, 2 to 10 percent slopes
#> 24                               Lithic Haplustands-Lava flows complex, 2 to 10 percent slopes
#> 25                                               Alahapa-Heake complex, 2 to 10 percent slopes
#> 26                                               Alahapa-Heake complex, 2 to 10 percent slopes
#> 27                             Alahapa extremely stony ashy sandy loam, 2 to 10 percent slopes
#> 28                                          Alahapa-Lava flows complex, 2 to 10 percent slopes
#> 29                      Halemaumau extremely gravelly ashy coarse sand, 2 to 10 percent slopes
#> 30                                                     Heake ashy loam, 2 to 10 percent slopes
#> 31                                            Heake-Lava flows complex, 2 to 10 percent slopes
#> 32                                     Kilauea very gravelly ashy sand, 2 to 10 percent slopes
#> 33                                   Lava flows-Kaholimo-Puiwa complex, 2 to 15 percent slopes
#> 34                                   Lava flows-Kaholimo-Puiwa complex, 2 to 15 percent slopes
#> 35                                   Lava flows-Kaholimo-Puiwa complex, 2 to 15 percent slopes
#> 36                                   Lava flows-Kaholimo-Puiwa complex, 2 to 15 percent slopes
#> 37                                         Lava flows-Kaholimo complex, 2 to 15 percent slopes
#> 38                                         Lava flows-Kaholimo complex, 2 to 15 percent slopes
#> 39                                          Kahalii-Lava flows complex, 2 to 10 percent slopes
#> 40                                         Kanohina-Lava flows complex, 2 to 10 percent slopes
#> 41                                          Nakanui-Lava flows complex, 2 to 10 percent slopes
#> 42                                                  Vitric Haplustands, 2 to 20 percent slopes
#> 43                                             Ahiu-Lava flows complex, 2 to 10 percent slopes
#> 44                        Pakini medial very fine sandy loam, 2 to 10 percent slopes, MLRA 157
#> 45                                                 Haa-Keamoku complex, 2 to 10 percent slopes
#> 46                                                 Haa-Keamoku complex, 2 to 10 percent slopes
#> 47                                  Oneula extremely stony medial loam, 2 to 10 percent slopes
#> 48                                         Lava flows-Menehune complex, 2 to 20 percent slopes
#> 49                                   Wahi extremely cobbly medial loam, 2 to 20 percent slopes
#> 50                   Oneula extremely stony medial loam, low elevation, 2 to 10 percent slopes
#> 51                                      Oneula-Keamoku-Maunaiu complex, 2 to 10 percent slopes
#> 52                                     Ahiu-Vitric Haplustands complex, 2 to 10 percent slopes
#> 53                                     Ahiu-Vitric Haplustands complex, 2 to 10 percent slopes
#> 54                                                            Badland, 10 to 50 percent slopes
#> 55                                        Lava flows-Honokohau complex, 2 to 20 percent slopes
#> 56                                            Mawae-Lava flows complex, 2 to 10 percent slopes
#> 57                                           Mawae-Lava flows complex, 10 to 20 percent slopes
#> 58                                          Lava flows-Kekake complex, 10 to 20 percent slopes
#> 59                                           Lava flows-Kekake complex, 2 to 20 percent slopes
#> 60                                           Puuiki-Lava flows complex, 2 to 10 percent slopes
#> 61                                          Puuiki-Lava flows complex, 10 to 20 percent slopes
#> 62                                           Lava flows-Puuiki complex, 2 to 20 percent slopes
#> 63                                   Iwalani-Lava flows complex, moist, 2 to 10 percent slopes
#> 64                                              Puuiki-Kamawai complex, 2 to 10 percent slopes
#> 65                                              Kamawai-Puuiki complex, 2 to 10 percent slopes
#> 66                                             Kamawai-Puuiki complex, 10 to 20 percent slopes
#> 67                                          Lava flows-Kamawai complex, 2 to 20 percent slopes
#> 68                                  Iwalani-Lava flows complex, moist, 10 to 20 percent slopes
#> 69                                         Lava flows-Kanohina complex, 2 to 20 percent slopes
#> 70                                  Kanohina ashy very fine sandy loam, 2 to 10 percent slopes
#> 71                             Auwaiakeakua extremely cobbly silt loam, 2 to 10 percent slopes
#> 72                            Auwaiakeakua extremely cobbly silt loam, 10 to 20 percent slopes
#> 73                                     Lava flows-Auwaiakeakua complex, 2 to 20 percent slopes
#> 74                                           Lava flows-Kiholo complex, 2 to 20 percent slopes
#> 75                                           Kiholo-Lava flows complex, 2 to 10 percent slopes
#> 76                                        Kahaumanu-Lava flows complex, 2 to 10 percent slopes
#> 77                                       Kahaumanu-Lava flows complex, 10 to 20 percent slopes
#> 78                                        Lava flows-Kahaumanu complex, 2 to 20 percent slopes
#> 79                      Puu Pa very cobbly medial very fine sandy loam, 6 to 12 percent slopes
#> 80                     Puu Pa very cobbly medial very fine sandy loam, 12 to 20 percent slopes
#> 81                                            Waikaloa-Puu Pa complex, 10 to 20 percent slopes
#> 82                                            Waikaloa-Puu Pa complex, 10 to 20 percent slopes
#> 83                                                Puako fine sandy loam, 0 to 6 percent slopes
#> 84                                             Waikaloa-Puu Pa complex, 2 to 10 percent slopes
#> 85                                             Waikaloa-Puu Pa complex, 2 to 10 percent slopes
#> 86                                Waikaloa medial very fine sandy loam, 2 to 10 percent slopes
#> 87                               Waikaloa medial very fine sandy loam, 10 to 20 percent slopes
#> 88                               Waikaloa medial very fine sandy loam, 20 to 50 percent slopes
#> 89                                   Nanuku extremely cobbly silt loam, 2 to 10 percent slopes
#> 90                                  Nanuku extremely cobbly silt loam, 10 to 20 percent slopes
#> 91                                           Lava flows-Nanuku complex, 2 to 20 percent slopes
#> 92                                               Waikui-Hapuna complex, 2 to 10 percent slopes
#> 93                                               Waikui-Hapuna complex, 2 to 10 percent slopes
#> 94                                              Waikui-Hapuna complex, 10 to 20 percent slopes
#> 95                                              Waikui-Hapuna complex, 10 to 20 percent slopes
#> 96                                      Hapuna-Waikui-Lalamilo complex, 0 to 20 percent slopes
#> 97                                      Hapuna-Waikui-Lalamilo complex, 0 to 20 percent slopes
#> 98                                      Hapuna-Waikui-Lalamilo complex, 0 to 20 percent slopes
#> 99                                  Rock outcrop-Waikui-Hapuna complex, 2 to 70 percent slopes
#> 100                                 Rock outcrop-Waikui-Hapuna complex, 2 to 70 percent slopes
#> 101                          Kawaihae very cobbly very fine sandy loam, 6 to 12 percent slopes
#> 102                         Kawaihae very cobbly very fine sandy loam, 12 to 20 percent slopes
#> 103                                    Kawaihae very cobbly silt loam,  6 to 20 percent slopes
#> 104                                       Rock outcrop-Kamakoa complex, 6 to 20 percent slopes
#> 105                                       Kamakoa very fine sandy loam, 2 to 10 percent slopes
#> 106                                 Waimea medial very fine sandy loam, 6 to 12 percent slopes
#> 107                                Waimea medial very fine sandy loam, 12 to 20 percent slopes
#> 108                                  Waimea medial very fine sandy loam, 0 to 6 percent slopes
#> 109                                Waimea medial very fine sandy loam, 20 to 50 percent slopes
#> 110                         Ihuanu-Lava flows, `a`a complex, 10 to 20 percent slopes, MLRA 160
#> 111                     Ihuanu very cobbly medial silt loam, 10 to 20 percent slopes, MLRA 160
#> 112                      Ihuanu very cobbly medial silt loam, 2 to 10 percent slopes, MLRA 160
#> 113                                               Iwalani medial loam, 10 to 20 percent slopes
#> 114                                        Lava flows-Iwalani complex, 10 to 20 percent slopes
#> 115                                         Iwalani-Lava flows complex, 2 to 10 percent slopes
#> 116                                        Iwalani-Lava flows complex, 10 to 20 percent slopes
#> 117                                                Iwalani medial loam, 3 to 10 percent slopes
#> 118                                Nawahine gravelly medial silt loam, 20 to 50 percent slopes
#> 119                                           Mahukona silty clay loam, 3 to 12 percent slopes
#> 120                                Mahukona very stony silty clay loam, 6 to 12 percent slopes
#> 121                                         Hawi very stony silty clay, 6 to 12 percent slopes
#> 122                                                     Hawi silty clay, 0 to 3 percent slopes
#> 123                                                    Hawi silty clay, 3 to 12 percent slopes
#> 124                                                  Kohala silty clay, 3 to 12 percent slopes
#> 125                                            Waimea medial silt loam, 6 to 12 percent slopes
#> 126                                           Waimea medial silt loam, 12 to 20 percent slopes
#> 127                                           Waimea medial silt loam, 20 to 50 percent slopes
#> 128                                           Waimea medial silt loam, 20 to 50 percent slopes
#> 129                                                   Kohala silty clay, 0 to 3 percent slopes
#> 130                                                 Kohala silty clay, 12 to 20 percent slopes
#> 131                                                 Kohala silty clay, 20 to 35 percent slopes
#> 132                                                 Kohala silty clay, 35 to 70 percent slopes
#> 133                                     Ainakea medial silty clay loam, 3 to 12 percent slopes
#> 134                                    Ainakea medial silty clay loam, 12 to 20 percent slopes
#> 135                                    Ainakea medial silty clay loam, 20 to 35 percent slopes
#> 136                                     Kemole stony medial silt loam, 12 to 20 percent slopes
#> 137                                      Kemole stony medial silt loam, 6 to 12 percent slopes
#> 138                                     Paauhau medial silty clay loam, 0 to 10 percent slopes
#> 139                                         Nenenui-Lava flows complex, 2 to 10 percent slopes
#> 140                                        Nenenui-Lava flows complex, 10 to 20 percent slopes
#> 141                                    Paauhau medial silty clay loam, 20 to 35 percent slopes
#> 142                         Ohianui extremely gravelly ashy loamy sand, 2 to 10 percent slopes
#> 143                        Ohianui extremely gravelly ashy loamy sand, 10 to 20 percent slopes
#> 144                                     Paauhau- Rock outcrop complex, 35 to 70 percent slopes
#> 145                                            Huikau ashy loamy sand, 10 to 20 percent slopes
#> 146                                            Kikoni medial silt loam,  0 to 3 percent slopes
#> 147                                  Kaalualu cobbly medial loamy sand, 2 to 10 percent slopes
#> 148                                 Kaalualu cobbly medial loamy sand, 10 to 20 percent slopes
#> 149                        Kaalualu extremely cobbly medial loamy sand, 2 to 10 percent slopes
#> 150                                            Kaalualu-Pakini complex, 2 to 10 percent slopes
#> 151                                            Kaalualu-Pakini complex, 2 to 10 percent slopes
#> 152                                Pakini medial very fine sandy loam, 10 to 20 percent slopes
#> 153                                           Pakini-Kaalualu complex, 50 to 70 percent slopes
#> 154                                           Pakini-Kaalualu complex, 50 to 70 percent slopes
#> 155                Lalaau very cobbly highly decomposed plant material, 2 to 10 percent slopes
#> 156                                         Kahaluu-Lava flows complex, 2 to 10 percent slopes
#> 157                            Lithic Haplotorrands-Lava flows complex, 2 to 10 percent slopes
#> 158                                                  Puiwa medial loam, 2 to 20 percent slopes
#> 159                     Ihuanu-Lava flows, pahoehoe complex, 10 to 20 percent slopes, MLRA 160
#> 160                                                   Humic Durustands, 2 to 10 percent slopes
#> 161                                        Kaholimo-Lava flows complex, 3 to 10 percent slopes
#> 162                                                Kaholimo-Ki complex, 3 to 10 percent slopes
#> 163                                               Kaholimo-Ki complex, 10 to 20 percent slopes
#> 164                           Kahaluu highly decomposed plant material, 3 to 10 percent slopes
#> 165                                          Lalaau-Lava flows complex, 2 to 20 percent slopes
#> 166                                 Kahaluu-lava flows-Ainahou complex, 2 to 10 percent slopes
#> 167                                 Kahaluu-lava flows-Ainahou complex, 2 to 10 percent slopes
#> 168                                             Nanaia-Ohaikea complex, 2 to 10 percent slopes
#> 169                                             Nanaia-Ohaikea complex, 2 to 10 percent slopes
#> 170                                         Nanaia-Lava flows complex, 20 to 40 percent slopes
#> 171                            Lava flows-Lithic Ustipsamments complex, 2 to 10 percent slopes
#> 172                            Alahapa extremely stony ashy sandy loam, 2 to 10 percent slopes
#> 173                                         Alahapa-Lava flows complex, 2 to 10 percent slopes
#> 174                                                    Heake ashy loam, 2 to 10 percent slopes
#> 175                                           Heake-Lava flows complex, 2 to 10 percent slopes
#> 176                                        Lava flows-Kaholimo complex, 2 to 15 percent slopes
#> 177                                        Lava flows-Kaholimo complex, 2 to 15 percent slopes
#> 178                                        Kanohina-Lava flows complex, 2 to 10 percent slopes
#> 179                       Pakini medial very fine sandy loam, 2 to 10 percent slopes, MLRA 157
#> 180                                                Haa-Keamoku complex, 2 to 10 percent slopes
#> 181                                                Haa-Keamoku complex, 2 to 10 percent slopes
#> 182                                        Oneula extremely stony loam, 2 to 10 percent slopes
#> 183                                        Lava flows-Menehune complex, 2 to 20 percent slopes
#> 184                                  Wahi extremely cobbly medial loam, 2 to 20 percent slopes
#> 185                                     Oneula-Keamoku-Maunaiu complex, 2 to 10 percent slopes
#> 186                                            Puu Pa-Waikaloa complex, 2 to 10 percent slopes
#> 187                                            Puu Pa-Waikaloa complex, 2 to 10 percent slopes
#> 188                                            Puu Pa-Waikaloa complex, 2 to 10 percent slopes
#> 189                                                     Puu Pa complex, 2 to 20 percent slopes
#> 190                                                     Puu Pa complex, 2 to 20 percent slopes
#> 191                            Pohakulehu cobbly ashy loamy fine sand, 20 to 40 percent slopes
#> 192                                               Puako, dry substratum, 3 to 6 percent slopes
#> 193                             Pohakulehu cobbly ashy loamy fine sand, 6 to 12 percent slopes
#> 194                            Pohakulehu cobbly ashy loamy fine sand, 12 to 20 percent slopes
#> 195                                      Lanapohaku-Lava flows complex, 6 to 40 percent slopes
#> 196      Pohakulehu very stony highly organic medial sandy loam, cool, 20 to 40 percent slopes
#> 197                          Lava flows-Lapa-Pohakulehu complex, cool, 30 to 50 percent slopes
#> 198                          Lava flows-Lapa-Pohakulehu complex, cool, 30 to 50 percent slopes
#> 199                           Lanapohaku-Pohakulehu-Lava flows complex, 6 to 20 percent slopes
#> 200                           Lanapohaku-Pohakulehu-Lava flows complex, 6 to 20 percent slopes
#> 201                                       Hilo hydrous silty clay loam, 0 to 10 percent slopes
#> 202                                      Hilo hydrous silty clay loam, 20 to 35 percent slopes
#> 203                                      Hilo hydrous silty clay loam, 10 to 20 percent slopes
#> 204                                               Akaka-Onomea complex, 0 to 10 percent slopes
#> 205                                              Akaka-Kaiwiki complex, 0 to 10 percent slopes
#> 206                                        Hilo-Rock outcrop complex, 35 to 100 percent slopes
#> 207                                      Ookala medial silty clay loam, 0 to 10 percent slopes
#> 208                                     Ookala medial silty clay loam, 10 to 20 percent slopes
#> 209                                     Ookala medial silty clay loam, 20 to 35 percent slopes
#> 210                                      Ookala-Rock outcrop complex, 35 to 100 percent slopes
#> 211                                    Paauhau medial silty clay loam, 10 to 20 percent slopes
#> 212                          Olaa cobbly hydrous loam, older substrate, 2 to 20 percent slopes
#> 213                                             Waiakea-Onomea complex, 0 to 20 percent slopes
#> 214                                      Alaeloa silty clay, 15 to 35 percent slopes, MLRA 164
#> 215                      Alaeloa silty clay, 5 to 25 percent slopes, severely eroded, MLRA 164
#> 216                Alaeloa stony silty clay, 3 to 15 percent slopes, severely eroded, MLRA 164
#> 217                                Alaeloa stony silty clay, overwash, 15 to 35 percent slopes
#> 218                                  Haleiwa silty clay loam, 0 to 10 percent slopes, MLRA 164
#> 219                       Haleiwa very stony silty clay loam, 0 to 15 percent slopes, MLRA 164
#> 220                                        Halawa silty clay, 3 to 25 percent slopes, MLRA 165
#> 221                                 Halawa silty clay, 3 to 25 percent slopes, severely eroded
#> 222                                                   Holomua silt loam, 0 to 3 percent slopes
#> 223                                                   Holomua silt loam, 3 to 7 percent slopes
#> 224                                  Holomua silt loam, 3 to 7 percent slopes, severely eroded
#> 225                                                  Holomua silt loam, 7 to 15 percent slopes
#> 226                                 Holomua silt loam, 7 to 15 percent slopes, severely eroded
#> 227                Hoolehua silty clay loam, 3 to 10 percent slopes, severely eroded, MLRA 158
#> 228                Hoolehua silty clay loam, 3 to 10 percent slopes, severely eroded, MLRA 158
#> 229                                                 Hoolehua silty clay, 0 to 3 percent slopes
#> 230                                                 Hoolehua silty clay, 3 to 7 percent slopes
#> 231                                                Hoolehua silty clay, 7 to 15 percent slopes
#> 232                                     Hoolehua silty clay, 15 to 35 percent slopes, MLRA 164
#> 233                                              Jaucas sand, 0 to 15 percent slopes, MLRA 163
#> 234                                 Jaucas-Dune land complex, 0 to 15 percent slopes, MLRA 158
#> 235                                          Kalae silty clay, 2 to 7 percent slopes, MLRA 165
#> 236                                         Kalae silty clay, 7 to 15 percent slopes, MLRA 165
#> 237                        Kalae silty clay, 5 to 15 percent slopes, severely eroded, MLRA 165
#> 238                       Kalae silty clay, 15 to 25 percent slopes, severely eroded, MLRA 165
#> 239                                 Kalae silty clay, 25 to 40 percent slopes, severely eroded
#> 240                             Kalaupapa-Lava flows complex, 3 to 25 percent slopes, MLRA 158
#> 241                                  Kapuhikani-Rock outcrop, 3 to 15 percent slopes, MLRA 163
#> 242                               Kawaihapai clay loam, moist, 0 to 2 percent slopes, MLRA 164
#> 243                         Kawaihapai stony clay loam, moist, 2 to 6 percent slopes, MLRA 164
#> 244                   Kawaihapai very stony clay loam, moist, 0 to 15 percent slopes, MLRA 164
#> 245                                          Kawaihapai silty clay loam, 2 to 7 percent slopes
#> 246                                          Kealia silt loam, 0 to 1 percent slopes, MLRA 163
#> 247                                          Kealia silt loam, 0 to 1 percent slopes, MLRA 163
#> 248                                                     Koele-Badland complex, moist, MLRA 164
#> 249                                        Lahaina silty clay, 0 to 3 percent slopes, MLRA 158
#> 250                                        Lahaina silty clay, 3 to 7 percent slopes, MLRA 158
#> 251                                       Lahaina silty clay, 7 to 15 percent slopes, MLRA 158
#> 252                      Lahaina silty clay, 7 to 15 percent slopes, severely eroded, MLRA 158
#> 253                     Lahaina silty clay, 15 to 25 percent slopes, severely eroded, MLRA 158
#> 254                     Lahaina silty clay, 25 to 40 percent slopes, severely eroded, MLRA 158
#> 255                                            Lualualei clay, 0 to 2 percent slopes, MLRA 163
#> 256                                            Lualualei clay, 0 to 2 percent slopes, MLRA 163
#> 257                                           Mala silty clay, 0 to 3 percent slopes, MLRA 166
#> 258                                           Mala silty clay, 3 to 7 percent slopes, MLRA 166
#> 259                                   Molokai silty clay loam, 0 to 3 percent slopes, MLRA 158
#> 260                                   Molokai silty clay loam, 3 to 7 percent slopes, MLRA 158
#> 261                            Molokai silty clay loam, 3 to 7 percent slopes, severely eroded
#> 262                                  Molokai silty clay loam, 7 to 15 percent slopes, MLRA 158
#> 263                 Molokai silty clay loam, 7 to 15 percent slopes, severely eroded, MLRA 158
#> 264                                    Naiwa silty clay loam, 3 to 13 percent slopes, MLRA 165
#> 265                                    Naiwa silty clay loam, 3 to 13 percent slopes, MLRA 165
#> 266                                    Naiwa silty clay loam, 3 to 13 percent slopes, MLRA 165
#> 267                                    Naiwa silty clay loam, 3 to 13 percent slopes, MLRA 165
#> 268                             Naiwa silty clay loam, 7 to 15 percent slopes, severely eroded
#> 269                                             Niulii silty clay loam, 7 to 30 percent slopes
#> 270   Niulii medial silty clay loam, medium textured subsoil, 7 to 30 percent slopes, MLRA 164
#> 271                                         Olelo silty clay, 3 to 15 percent slopes, MLRA 164
#> 272                                    Oli medial silt loam, 10 to 30 percent slopes, MLRA 165
#> 273                                    Oli medial silt loam, 10 to 30 percent slopes, MLRA 165
#> 274                                    Oli medial silt loam, 30 to 70 percent slopes, MLRA 165
#> 275                                    Oli medial silt loam, 30 to 70 percent slopes, MLRA 165
#> 276                                         Pamoa silty clay, 5 to 20 percent slopes, MLRA 158
#> 277                                 Pamoa silty clay, 5 to 20 percent slopes, eroded, MLRA 158
#> 278                           Pamoa stony silty clay, 5 to 20 percent slopes, eroded, MLRA 158
#> 279                                   Pulehu stony sandy loam, 0 to 7 percent slopes, MLRA 166
#> 280                                         Pulehu sandy loam, 2 to 6 percent slopes, MLRA 166
#> 281                                         Pulehu clay loam, 0 to 3 percent slopes , MLRA 163
#> 282                                              Waihuna clay, 3 to 7 percent slopes, MLRA 158
#> 283                                             Waihuna clay, 7 to 15 percent slopes, MLRA 158
#> 284                                            Waihuna clay, 15 to 25 percent slopes, MLRA 158
#> 285                                   Waikapu silty clay loam, 0 to 3 percent slopes, MLRA 158
#> 286                                             Waikapu silty clay loam, 3 to 7 percent slopes
#> 287                            Waikapu silty clay loam, 3 to 7 percent slopes, severely eroded
#> 288                 Waikapu silty clay loam, 7 to 15 percent slopes, severely eroded, MLRA 158
#> 289                                                                     Badland-Mahana complex
#> 290                                           Halii gravelly silty clay, 3 to 8 percent slopes
#> 291                                          Halii gravelly silty clay, 8 to 15 percent slopes
#> 292                                 Halii gravelly silty clay, 15 to 25 percent slopes, eroded
#> 293                                 Halii gravelly silty clay, 25 to 40 percent slopes, eroded
#> 294                                             Hanalei silty clay loam, 0 to 2 percent slopes
#> 295                                             Hanalei silty clay loam, 0 to 2 percent slopes
#> 296                                          Hihimanu silty clay loam, 40 to 70 percent slopes
#> 297                                        Hanalei silty clay, 0 to 2 percent slopes, MLRA 167
#> 298                                       Hanalei peaty silty clay loam, 0 to 2 percent slopes
#> 299                                Hanalei silty clay, deep water table, 0 to 6 percent slopes
#> 300                                                Hanamaulu silty clay, 3 to 8 percent slopes
#> 301                                               Hanamaulu silty clay, 8 to 15 percent slopes
#> 302                                              Hanamaulu silty clay, 15 to 25 percent slopes
#> 303                                              Hanamaulu silty clay, 25 to 40 percent slopes
#> 304                                        Hanamaulu stony silty clay, 10 to 35 percent slopes
#> 305                                     Hanamaulu bouldery silty clay, 10 to 35 percent slopes
#> 306                                              Ioleau silty clay loam, 2 to 6 percent slopes
#> 307                                             Ioleau silty clay loam, 6 to 12 percent slopes
#> 308                                    Ioleau silty clay loam, 12 to 20 percent slopes, eroded
#> 309                                    Ioleau silty clay loam, 20 to 30 percent slopes, eroded
#> 310                                              Jaucas loamy fine sand, 0 to 8 percent slopes
#> 311                                Jaucas loamy fine sand, dark variant, 0 to 8 percent slopes
#> 312                                           Kaena clay, brown variant, 1 to 6 percent slopes
#> 313                                          Kaena clay, brown variant, 6 to 12 percent slopes
#> 314                                                  Kalapa silty clay, 8 to 20 percent slopes
#> 315                                                 Kalapa silty clay, 20 to 40 percent slopes
#> 316                                                 Kalapa silty clay, 40 to 70 percent slopes
#> 317                                                         Kalihi clay, 0 to 2 percent slopes
#> 318                           Kalapa - Rock outcrop complex, 40 to 70 percent slopes, MLRA 164
#> 319                                                                           Kaloko clay loam
#> 320                                                                           Kaloko clay loam
#> 321                                               Kaloko clay, 0 to 2 percent slopes, MLRA 163
#> 322                                               Kaloko clay, 0 to 2 percent slopes, MLRA 163
#> 323                                                    Kapaa silty clay, 3 to 8 percent slopes
#> 324                                                   Kapaa silty clay, 8 to 15 percent slopes
#> 325                                                  Kapaa silty clay, 15 to 25 percent slopes
#> 326                                                  Kapaa silty clay, 25 to 40 percent slopes
#> 327                                                   Kekaha silty clay, 0 to 2 percent slopes
#> 328                                                   Kekaha silty clay, 2 to 6 percent slopes
#> 329                                                         Kekaha clay, 0 to 2 percent slopes
#> 330                   Kekaha extremely stony silty clay loam, 0 to 35 percent slopes, MLRA 158
#> 331                                  Kolokolo extremely stony clay loam, 0 to 2 percent slopes
#> 332                                    Koloa stony silty clay, 3 to 8 percent slopes, MLRA 158
#> 333                                          Koloa stony silty clay, 8 to 15 percent, MLRA 158
#> 334                                            Koloa stony silty clay, 15 to 25 percent slopes
#> 335                                                  Koolau silty clay, 8 to 30 percent slopes
#> 336                                                  Kolokolo clay loam, 0 to 2 percent slopes
#> 337                                                    Lawai silty clay, 0 to 8 percent slopes
#> 338                                                   Lawai silty clay, 8 to 15 percent slopes
#> 339                                                  Lawai silty clay, 15 to 25 percent slopes
#> 340                                                    Lihue silty clay, 0 to 8 percent slopes
#> 341                                                   Lihue silty clay, 8 to 15 percent slopes
#> 342                                                  Lihue silty clay, 15 to 25 percent slopes
#> 343                                          Lihue silty clay, 25 to 40 percent slopes, eroded
#> 344                                           Lihue gravelly silty clay, 0 to 8 percent slopes
#> 345                                          Lihue gravelly silty clay, 8 to 15 percent slopes
#> 346                   Lualualei extremely cobbly clay, moist, 3 to 35 percent slopes, MLRA 164
#> 347                                            Lualualei clay, 0 to 2 percent slopes, MLRA 163
#> 348                                            Lualualei clay, 0 to 2 percent slopes, MLRA 163
#> 349                                            Lualualei clay, 2 to 6 percent slopes, MLRA 163
#> 350                                                   Mahana silt loam, 6 to 12 percent slopes
#> 351                                                  Mahana silt loam, 12 to 20 percent slopes
#> 352                                 Mahana silt loam, 12 to 20 percent slopes, severely eroded
#> 353                                                  Mahana silt loam, 20 to 35 percent slopes
#> 354                                    Mahana silt loam, 20-35 percent slopes, severely eroded
#> 355                                                 Makapili silty clay, 0 to 8 percent slopes
#> 356                                                Makapili silty clay, 8 to 15 percent slopes
#> 357                                               Makapili silty clay, 15 to 25 percent slopes
#> 358                                               Makapili silty clay, 25 to 40 percent slopes
#> 359                                  Makaweli silty clay loam, 0 to 6 percent slopes, MLRA 158
#> 360                                 Makaweli silty clay loam, 6 to 12 percent slopes, MLRA 158
#> 361                                Makaweli silty clay loam, 12 to 20 percent slopes, MLRA 158
#> 362                                  Makaweli silty clay loam, 20 to 35 percent slopes, eroded
#> 363                                      Makaweli stony silty clay loam, 0 to 6 percent slopes
#> 364                                     Makaweli stony silty clay loam, 6 to 12 percent slopes
#> 365                                    Makaweli stony silty clay loam, 12 to 20 percent slopes
#> 366                          Makaweli stony silty clay loam, 20 to 35 percent slopes, MLRA 158
#> 367                            Mamala cobbly silty clay loam, 0 to 12 percent slopes, MLRA 163
#> 368                                                                   Mokuleia fine sandy loam
#> 369                                                 Mokuleia clay loam, poorly drained variant
#> 370                                      Niu silty clay loam, 6 to 12 percent slopes, MLRA 158
#> 371                                     Niu silty clay loam, 12 to 20 percent slopes, MLRA 158
#> 372                              Niu silty clay loam, 6 to 20 percent slopes, eroded, MLRA 158
#> 373                              Niu silty clay loam 20 to 35 percent slopes, eroded, MLRA 158
#> 374                                                                                Nohili clay
#> 375                                                      Nonopahu clay, 2 to 10 percent slopes
#> 376                                                Nonopahu stony clay, 2 to 12 percent slopes
#> 377                                         Oli medial loam, 10 to 40 percent slopes, MLRA 158
#> 378                                    Oli medial silt loam, 10 to 30 percent slopes, MLRA 165
#> 379                                    Oli medial silt loam, 10 to 30 percent slopes, MLRA 165
#> 380                                    Oli medial silt loam, 30 to 70 percent slopes, MLRA 165
#> 381                                    Oli medial silt loam, 30 to 70 percent slopes, MLRA 165
#> 382                                                    Pakala clay loam, 0 to 2 percent slopes
#> 383                                                   Pakala clay loam, 2 to 10 percent slopes
#> 384                   Pakala extremely stony sandy clay loam, 0 to 12 percent slopes, MLRA 158
#> 385                                  Pohakupu silty clay loam, 0 to 8 percent slopes, MLRA 158
#> 386                                               Pooku silty clay loam, 3 to 8 percent slopes
#> 387                                              Pooku silty clay loam, 8 to 25 percent slopes
#> 388                                                    Pooku silty clay, 0 to 8 percent slopes
#> 389                                                   Pooku silty clay, 8 to 15 percent slopes
#> 390                                                  Pooku silty clay, 15 to 25 percent slopes
#> 391                                                  Pooku silty clay, 25 to 40 percent slopes
#> 392                                                Puhi silty clay loam, 0 to 3 percent slopes
#> 393                                                Puhi silty clay loam, 3 to 8 percent slopes
#> 394                                               Puhi silty clay loam, 8 to 15 percent slopes
#> 395                                              Puhi silty clay loam, 15 to 25 percent slopes
#> 396                                              Puhi silty clay loam, 25 to 40 percent slopes
#> 397                                           Puu Opae silty clay loam, 8 to 15 percent slopes
#> 398                                          Puu Opae silty clay loam, 15 to 25 percent slopes
#> 399                                          Puu Opae silty clay loam, 25 to 40 percent slopes
#> 400                             Waiawa-Rock outcrop complex, 30 to 80 percent slopes, MLRA 158
#> 401                                                                   Waikomo stony silty clay
#> 402                                                              Waikomo very rocky silty clay
#> 403                            Waikomo - Rock outcrop complex, 2 to 6 percent slopes, MLRA 158
#> 404                                                                              Coral outcrop
#> 405                                              Jaucas sand, 0 to 15 percent slopes, MLRA 163
#> 406                                    Kalae silty clay, dry, 2 to 7  percent slopes, MLRA 158
#> 407                                    Kalae silty clay, dry, 7 to 15 percent slopes, MLRA 158
#> 408                  Kalae silty clay, dry, 15 to 25 percent slopes, severely eroded, MLRA 158
#> 409                                        Kanepuu silty clay, 3 to 7 percent slopes, MLRA 158
#> 410                         Kanepuu-Dune land complex, 3 to 7 percent slopes, eroded, MLRA 158
#> 411                                       Kanepuu silty clay, 7 to 15 percent slopes, MLRA 158
#> 412                        Kanepuu-Dune land complex, 7 to 15 percent slopes, eroded, MLRA 158
#> 413                                     Koele silty clay loam, 3 to 7 percent slopes, MLRA 158
#> 414                                    Koele silty clay loam, 7 to 15 percent slopes, MLRA 158
#> 415                                   Koele silty clay loam, 15 to 25 percent slopes, MLRA 158
#> 416                                    Koele-Badland complex, 7 to 70 percent slopes, MLRA 158
#> 417                                        Lahaina silty clay, 0 to 3 percent slopes, MLRA 158
#> 418                                        Lahaina silty clay, 3 to 7 percent slopes, MLRA 158
#> 419                       Lahaina silty clay, 3 to 7 percent slopes, severely eroded, MLRA 158
#> 420                                       Lahaina silty clay, 7 to 15 percent slopes, MLRA 158
#> 421                      Lahaina silty clay, 7 to 15 percent slopes, severely eroded, MLRA 158
#> 422                     Lahaina silty clay, 15 to 25 percent slopes, severely eroded, MLRA 158
#> 423                     Lahaina silty clay, 25 to 40 percent slopes, severely eroded, MLRA 158
#> 424                                            Lualualei clay, 0 to 2 percent slopes, MLRA 163
#> 425                                            Lualualei clay, 0 to 2 percent slopes, MLRA 163
#> 426                                           Mala silty clay, 0 to 3 percent slopes, MLRA 163
#> 427                                           Mala silty clay, 3 to 7 percent slopes, MLRA 163
#> 428                                   Molokai silty clay loam, 0 to 3 percent slopes, MLRA 158
#> 429                                   Molokai silty clay loam, 3 to 7 percent slopes, MLRA 158
#> 430                                  Molokai silty clay loam, 7 to 15 percent slopes, MLRA 158
#> 431                 Molokai silty clay loam, 7 to 15 percent slopes, severely eroded, MLRA 158
#> 432                                    Naiwa silty clay loam, 3 to 13 percent slopes, MLRA 165
#> 433                                    Naiwa silty clay loam, 3 to 13 percent slopes, MLRA 165
#> 434                                    Naiwa silty clay loam, 3 to 13 percent slopes, MLRA 165
#> 435                                    Naiwa silty clay loam, 3 to 13 percent slopes, MLRA 165
#> 436                                         Pamoa silty clay, 5 to 20 percent slopes, MLRA 158
#> 437                                 Pamoa silty clay, 5 to 20 percent slopes, eroded, MLRA 158
#> 438                                   Pulehu stony sandy loam, 0 to 7 percent slopes, MLRA 163
#> 439                                         Pulehu sandy loam, 2 to 6 percent slopes, MLRA 163
#> 440                                         Pulehu clay loam, 0 to 3 percent slopes , MLRA 163
#> 441                                     Uwala silty clay loam, 2 to 7 percent slopes, MLRA 158
#> 442                                    Uwala silty clay loam, 7 to 15 percent slopes, MLRA 158
#> 443                   Uwala silty clay loam, 7 to 15 percent slopes, severely eroded, MLRA 158
#> 444                                              Waihuna clay, 0 to 3 percent slopes, MLRA 158
#> 445                                              Waihuna clay, 3 to 7 percent slopes, MLRA 158
#> 446                                             Waihuna clay, 7 to 15 percent slopes, MLRA 158
#> 447                                            Waihuna clay, 15 to 25 percent slopes, MLRA 158
#> 448                                     Waihuna gravelly clay, 3 to 7 percent slopes, MLRA 158
#> 449                                   Waikapu silty clay loam, 0 to 3 percent slopes, MLRA 158
#> 450                 Waikapu silty clay loam, 7 to 15 percent slopes, severely eroded, MLRA 158
#> 451                                           Alae sandy loam, 3 to 7 percent slopes, MLRA 158
#> 452                                    Alae cobbly sandy loam, 0 to 3 percent slopes, MLRA 158
#> 453                                    Alae cobbly sandy loam, 3 to 7 percent slopes, MLRA 158
#> 454                                        Alaeloa silty clay, 3 to 7 percent slopes, MLRA 158
#> 455                                       Alaeloa silty clay, 7 to 15 percent slopes, MLRA 158
#> 456                                      Alaeloa silty clay, 15 to 35 percent slopes, MLRA 164
#> 457                                       Ewa silty clay loam, 0 to 3 percent slopes, MLRA 158
#> 458                                Ewa cobbly silty clay loam, 0 to 3 percent slopes, MLRA 158
#> 459                                Ewa cobbly silty clay loam, 3 to 7 percent slopes, MLRA 163
#> 460                                                      Ewa silty clay, 0 to 3 percent slopes
#> 461                                            Ewa silty clay, 3 to 7 percent slopes, MLRA 163
#> 462                                     Ewa cobbly silty clay, 3 to 7 percent slopes, MLRA 163
#> 463                                          Haiku silty clay, 3 to 7 percent slopes, MLRA 167
#> 464                                         Haiku silty clay, 7 to 15 percent slopes, MLRA 167
#> 465                                                Haiku clay, 3 to 7 percent slopes, MLRA 167
#> 466                                               Haiku clay, 7 to 15 percent slopes, MLRA 167
#> 467                                          Haliimaile silty clay loam, 3 to 7 percent slopes
#> 468                                         Haliimaile silty clay loam, 7 to 15 percent slopes
#> 469                                               Haliimaile silty clay, 3 to 7 percent slopes
#> 470                                               Haliimaile silty clay, 3 to 7 percent slopes
#> 471                                               Haliimaile silty clay, 3 to 7 percent slopes
#> 472                                              Haliimaile silty clay, 7 to 15 percent slopes
#> 473                                        Halawa silty clay, 3 to 25 percent slopes, MLRA 165
#> 474                             Haliimaile gravelly silty clay, 7 to 15 percent slopes, eroded
#> 475                                    Hana very stony silty clay loam, 3 to 25 percent slopes
#> 476                               Hana extremely stony silty clay loam, 3 to 25 percent slopes
#> 477                      Hana silty clay loam, moderately deep variant, 3 to 15 percent slopes
#> 478      Hana extremely stony silty clay loam, moderately deep variant, 3 to 15 percent slopes
#> 479                                              Hamakuapoko silty clay, 3 to 7 percent slopes
#> 480                                             Hamakuapoko silty clay, 7 to 15 percent slopes
#> 481                                     Hamakuapoko silty clay, 7 to 15 percent slopes, eroded
#> 482                                                 Honolua silty clay, 7 to 15 percent slopes
#> 483                                                Honolua silty clay, 15 to 25 percent slopes
#> 484                                            Iao silty clay, 0 to 3 percent slopes, MLRA 163
#> 485                                            Iao silty clay, 3 to 7 percent slopes, MLRA 158
#> 486                                               Iao cobbly silty clay, 3 to 7 percent slopes
#> 487                                              Iao cobbly silty clay, 7 to 15 percent slopes
#> 488                                                  Iao clay, 3 to 7 percent slopes, MLRA 163
#> 489                                                 Iao clay, 7 to 15 percent slopes, MLRA 163
#> 490                                                       Io silt loam, 7 to 25 percent slopes
#> 491                                              Jaucas sand, 0 to 15 percent slopes, MLRA 163
#> 492                                      Jaucas sand, saline, 0 to 12 percent slopes, MLRA 163
#> 493                                                   Kahana silty clay, 3 to 7 percent slopes
#> 494                                                  Kahana silty clay, 7 to 15 percent slopes
#> 495                                                 Kahana silty clay, 15 to 25 percent slopes
#> 496                                                  Kailua silty clay, 3 to 25 percent slopes
#> 497                                         Kaimu extremely stony peat, 7 to 25 percent slopes
#> 498                                       Kamaole very stony silt loam, 3 to 15 percent slopes
#> 499                                  Kamaole extremely stony silt loam, 3 to 15 percent slopes
#> 500                        Kaupo very stony silty clay loam, 3 to 25 percent slopes, MLRA 159A
#> 501                        Kaupo extremely stony silty clay, 3 to 25 percent slopes, MLRA 159A
#> 502                        Kealia silt loam, frequent ponding, 0 to 1 percent slopes, MLRA 163
#> 503                        Kealia silt loam, frequent ponding, 0 to 1 percent slopes, MLRA 163
#> 504                                       Keahua cobbly silty clay loam, 3 to 7 percent slopes
#> 505                                      Keahua cobbly silty clay loam, 7 to 15 percent slopes
#> 506                                     Keahua cobbly silty clay loam, 15 to 25 percent slopes
#> 507                                              Keahua silty clay loam, 3 to 7 percent slopes
#> 508                                  Keahua very stony silty clay loam, 7 to 25 percent slopes
#> 509                                             Keahua silty clay loam, 7 to 15 percent slopes
#> 510                                                  Keahua silty clay, 7 to 15 percent slopes
#> 511                                           Keahua cobbly silty clay, 7 to 15 percent slopes
#> 512                                            Keahua stony silty clay, 7 to 15 percent slopes
#> 513                Keawakapu extremely stony silty clay loam, 3 to 25 percent slopes, MLRA 157
#> 514                                                                        Koele-Rocky complex
#> 515                                 Kula cobbly medial loam, 12 to 20 percent slopes, MLRA 160
#> 516                             Kula - Rock outcrop complex, 12 to 40 percent slopes, MLRA 160
#> 517                                                          Kula loam, 4 to 12 percent slopes
#> 518                                                         Kula loam, 12 to 20 percent slopes
#> 519                                        Lahaina silty clay, 3 to 7 percent slopes, MLRA 158
#> 520                                       Lahaina silty clay, 7 to 15 percent slopes, MLRA 158
#> 521                                                Lahaina silty clay, 15 to 25 percent slopes
#> 522                                                  Makawao silty clay, 3 to 7 percent slopes
#> 523                                                  Makawao silty clay, 3 to 7 percent slopes
#> 524                                                  Makawao silty clay, 3 to 7 percent slopes
#> 525                                                 Makawao silty clay, 7 to 15 percent slopes
#> 526                                                Makaalae silty clay, 7 to 25 percent slopes
#> 527                                Makaalae extremely stony silty clay, 7 to 25 percent slopes
#> 528                                   Molokai silty clay loam, 0 to 3 percent slopes, MLRA 158
#> 529                                   Molokai silty clay loam, 3 to 7 percent slopes, MLRA 158
#> 530                                  Molokai silty clay loam, 7 to 15 percent slopes, MLRA 158
#> 531                                                      Makaalae clay, 7 to 40 percent slopes
#> 532                                         Makena-Rubble land complex, 3 to 15 percent slopes
#> 533                                         Makena-Rubble land complex, 3 to 15 percent slopes
#> 534                                         Makena-Rubble land complex, 3 to 15 percent slopes
#> 535 Malama extremely stony highly decomposed plant material, 3 to 20 percent slopes, MLRA 159A
#> 536                                   Naiwa silty clay loam, 13 to 45 percent slopes, MLRA 164
#> 537                                   Naiwa silty clay loam, 13 to 45 percent slopes, MLRA 164
#> 538                                   Naiwa silty clay loam, 13 to 45 percent slopes, MLRA 164
#> 539                            Oanapuka very stony silt loam, 7 to 25 percent slopes, MLRA 157
#> 540                                 Oanapuka extremely stony silt loam, 7 to 25 percent slopes
#> 541                                        Olelo silty clay, 15 to 50 percent slopes, MLRA 164
#> 542                                    Oli medial silt loam, 13 to 45 percent slopes, MLRA 164
#> 543                                                        Olinda loam, 4 to 12 percent slopes
#> 544                                                       Olinda loam, 12 to 20 percent slopes
#> 545                                                     Paia silty clay, 3 to 7 percent slopes
#> 546                                                    Paia silty clay, 7 to 15 percent slopes
#> 547                                            Paia silty clay, 7 to 15 percent slopes, eroded
#> 548                                                        Pauwela clay, 3 to 7 percent slopes
#> 549                                                        Pauwela clay, 3 to 7 percent slopes
#> 550                                                        Pauwela clay, 3 to 7 percent slopes
#> 551                                                       Pauwela clay, 7 to 15 percent slopes
#> 552                                                      Pauwela clay, 15 to 25 percent slopes
#> 553                                                    Pulehu silt loam, 0 to 3 percent slopes
#> 554                                                    Pulehu silt loam, 3 to 7 percent slopes
#> 555                                             Pulehu cobbly silt loam, 0 to 3 percent slopes
#> 556                                             Pulehu cobbly silt loam, 3 to 7 percent slopes
#> 557                                         Pulehu clay loam, 0 to 3 percent slopes , MLRA 163
#> 558                                             Pulehu cobbly clay loam, 0 to 3 percent slopes
#> 559                                             Pulehu cobbly clay loam, 3 to 7 percent slopes
#> 560                                                     Pane silt loam, 7 to 25 percent slopes
#> 561                                                        Puuone sand, 7 to 30 percent slopes
#> 562                                                        Puuone sand, 7 to 30 percent slopes
#> 563                                                        Puuone sand, 7 to 30 percent slopes
#> 564                      Puu Pa very stony medial silt loam, 15 to 50 percent slopes, MLRA 157
#> 565                                               Ulupalakua silt loam, 7 to 25 percent slopes
#> 566                                       Uma loamy coarse sand, 7 to 25 percent slopes, rocky
#> 567                                       Wahikuli silty clay, 3 to 7 percent slopes, MLRA 158
#> 568                                 Wahikuli stony silty clay, 3 to 7 percent slopes, MLRA 158
#> 569                                Wahikuli stony silty clay, 7 to 15 percent slopes, MLRA 158
#> 570                            Wahikuli very stony silty clay, 3 to 7 percent slopes, MLRA 158
#> 571                                             Waiakoa silty clay loam, 3 to 7 percent slopes
#> 572                                            Waiakoa silty clay loam, 7 to 15 percent slopes
#> 573                                      Waiakoa cobbly silty clay loam, 3 to 7 percent slopes
#> 574                        Waiakoa very stony silty clay loam, 3 to 7 percent slopes, MLRA 158
#> 575                       Waiakoa very stony silty clay loam, 7 to 15 percent slopes, MLRA 158
#> 576                   Waiakoa extremely stony silty clay loam, 3 to 7 percent slopes, MLRA 157
#> 577                  Waiakoa extremely stony silty clay loam, 7 to 15 percent slopes, MLRA 158
#> 578          Waiakoa extremely stony silty clay loam, 3 to 25 percent slopes, eroded, MLRA 157
#> 579                                        Wailuku silty clay, 3 to 7 percent slopes, MLRA 163
#> 580                                       Wailuku silty clay, 7 to 15 percent slopes, MLRA 163
#> 581                                       Wailuku silty clay, 7 to 15 percent slopes, MLRA 163
#> 582                                       Wailuku silty clay, 7 to 15 percent slopes, MLRA 163
#> 583                                Wailuku cobbly silty clay, 7 to 15 percent slopes, MLRA 163
#> 584                                        Wainee very stony silty clay, 3 to 7 percent slopes
#> 585                                       Wainee very stony silty clay, 7 to 15 percent slopes
#> 586                                   Wainee extremely stony silty clay, 3 to 7 percent slopes
#> 587                        Wainee extremely stony silty clay, 7 to 15 percent slopes, MLRA 158
#> 588                     Alaeloa silty clay, older substrate, 15 to 35 percent slopes, MLRA 167
#> 589                                                Alaeloa silty clay, 40 to 70 percent slopes
#> 590                                                                              Coral outcrop
#> 591                                                 Ewa silty clay loam, 3 to 6 percent slopes
#> 592                                                Ewa silty clay loam, 6 to 12 percent slopes
#> 593                             Ewa silty clay loam, moderately shallow, 0 to 2 percent slopes
#> 594                             Ewa silty clay loam, moderately shallow, 2 to 6 percent slopes
#> 595                                                Ewa stony silty clay, 0 to 2 percent slopes
#> 596                                                Ewa stony silty clay, 2 to 6 percent slopes
#> 597                                               Ewa stony silty clay, 6 to 12 percent slopes
#> 598                                                  Haleiwa silty clay, 0 to 2 percent slopes
#> 599                                                  Haleiwa silty clay, 2 to 6 percent slopes
#> 600                                                  Halawa silt loam, 20 to 35 percent slopes
#> 601                                          Halawa silt loam, 35 to 70 percent slopes, eroded
#> 602                                               Helemano silty clay, 30 to 90 percent slopes
#> 603                                        Hanalei silty clay, 0 to 2 percent slopes, MLRA 167
#> 604                                                  Hanalei silty clay, 2 to 6 percent slopes
#> 605                                            Hanalei stony silty clay, 2 to 6 percent slopes
#> 606                                                     Honouliuli clay, 0 to 2 percent slopes
#> 607                                                     Honouliuli clay, 2 to 6 percent slopes
#> 608                                              Jaucas sand, 0 to 15 percent slopes, MLRA 163
#> 609                                      Jaucas sand, saline, 0 to 12 percent slopes, MLRA 163
#> 610                                                          Kaena clay, 2 to 6 percent slopes
#> 611                                                         Kaena clay, 6 to 12 percent slopes
#> 612                                                    Kaena stony clay, 2 to 6 percent slopes
#> 613                                                   Kaena stony clay, 6 to 12 percent slopes
#> 614                                                  Kaena stony clay, 12 to 20 percent slopes
#> 615                                             Kaena very stony clay, 10 to 35 percent slopes
#> 616                                               Kaloko clay, 0 to 2 percent slopes, MLRA 163
#> 617                                               Kaloko clay, 0 to 2 percent slopes, MLRA 163
#> 618                                  Kaloko clay, 0 to 2 percent slopes, noncalcareous variant
#> 619                                  Kaloko clay, 0 to 2 percent slopes, noncalcareous variant
#> 620                                                  Kaneohe silty clay, 3 to 8 percent slopes
#> 621                                                 Kaneohe silty clay, 8 to 15 percent slopes
#> 622                                            Kaneohe silty clay loam, 5 to 15 percent slopes
#> 623                                           Kaneohe silty clay loam, 15 to 30 percent slopes
#> 624                                           Kaneohe silty clay loam, 30 to 65 percent slopes
#> 625                                                Kaneohe silty clay, 30 to 65 percent slopes
#> 626                                                 Kapaa silty clay, 40 to 100 percent slopes
#> 627                                      Kawaihapai clay loam, 0 to 2 percent slopes, MLRA 158
#> 628                                          Kawaihapai stony clay loam, 0 to 2 percent slopes
#> 629                                Kawaihapai stony clay loam, 2 to 6 percent slopes, MLRA 158
#> 630                                                Kawaihapai clay loam, 2 to 6 percent slopes
#> 631                          Kawaihapai very stony clay loam, 0 to 15 percent slopes, MLRA 158
#> 632                                               Kawaihapai clay loam, 6 to 15 percent slopes
#> 633                                                          Keaau clay, 0 to 2 percent slopes
#> 634                                                          Keaau clay, 0 to 2 percent slopes
#> 635                                                    Keaau stony clay, 2 to 6 percent slopes
#> 636                                                  Keaau clay, saline, 0 to 2 percent slopes
#> 637                                                    Kemoo silty clay, 2 to 6 percent slopes
#> 638                                                   Kemoo silty clay, 6 to 12 percent slopes
#> 639                                                  Kemoo silty clay, 12 to 20 percent slopes
#> 640                                                  Kemoo silty clay, 20 to 35 percent slopes
#> 641                                                  Kemoo silty clay, 35 to 70 percent slopes
#> 642                                                                      Kemoo-Badland complex
#> 643                                                      Koko silt loam, 2 to 6 percent slopes
#> 644                                                     Koko silt loam, 6 to 12 percent slopes
#> 645                                                    Koko silt loam, 12 to 25 percent slopes
#> 646                                                      Kokokahi clay, 6 to 12 percent slopes
#> 647                                           Kokokahi very stony clay, 0 to 35 percent slopes
#> 648                                            Kolekole silty clay loam, 1 to 6 percent slopes
#> 649                                           Kolekole silty clay loam, 6 to 12 percent slopes
#> 650                                          Kolekole silty clay loam, 12 to 25 percent slopes
#> 651                                                    Kunia silty clay, 0 to 3 percent slopes
#> 652                                                    Kunia silty clay, 3 to 8 percent slopes
#> 653                                                   Kunia silty clay, 8 to 15 percent slopes
#> 654                                        Lahaina silty clay, 0 to 3 percent slopes, MLRA 158
#> 655                                 Lahaina silty clay, moist, 3 to 7 percent slopes, MLRA 167
#> 656                                       Lahaina silty clay, 7 to 15 percent slopes, MLRA 158
#> 657                      Lahaina silty clay, 7 to 15 percent slopes, severely eroded, MLRA 158
#> 658                                                 Leilehua silty clay, 2 to 6 percent slopes
#> 659                                                       Leilehua silty clay, 6 to 12 percent
#> 660                                                  Lolekaa silty clay, 3 to 8 percent slopes
#> 661                                                  Lolekaa silty clay, 3 to 8 percent slopes
#> 662                                                  Lolekaa silty clay, 3 to 8 percent slopes
#> 663                                                 Lolekaa silty clay, 8 to 15 percent slopes
#> 664                                                Lolekaa silty clay, 15 to 25 percent slopes
#> 665                                                Lolekaa silty clay, 25 to 40 percent slopes
#> 666                                                Lolekaa silty clay, 40 to 70 percent slopes
#> 667                          Lualualei extremely cobbly clay, 3 to 35 percent slopes, MLRA 166
#> 668                                            Lualualei clay, 0 to 2 percent slopes, MLRA 163
#> 669                                            Lualualei clay, 0 to 2 percent slopes, MLRA 163
#> 670                                            Lualualei clay, 2 to 6 percent slopes, MLRA 163
#> 671                                                Lualualei stony clay, 0 to 2 percent slopes
#> 672                                                Lualualei stony clay, 2 to 6 percent slopes
#> 673                                                                     Mahana-Badland complex
#> 674                                     Mahana silty clay loam, 6 to 12 percent slopes, eroded
#> 675                                    Mahana silty clay loam, 12 to 20 percent slopes, eroded
#> 676                                    Mahana silty clay loam, 20 to 35 percent slopes, eroded
#> 677                                                       Makalapa clay, 2 to 6 percent slopes
#> 678                                                       Makalapa clay, 2 to 6 percent slopes
#> 679                                                      Makalapa clay, 6 to 12 percent slopes
#> 680                                                     Makalapa clay, 12 to 20 percent slopes
#> 681                                                    Makiki clay loam, 0 to 2 percent slopes
#> 682                                              Makiki stony clay loam, 0 to 3 percent slopes
#> 683                            Mamala cobbly silty clay loam, 0 to 12 percent slopes, MLRA 163
#> 684                                              Manana silty clay loam, 2 to 6 percent slopes
#> 685                                             Manana silty clay loam, 6 to 12 percent slopes
#> 686                                    Manana silty clay loam, 12 to 25 percent slopes, eroded
#> 687                                                   Manana silty clay, 3 to 8 percent slopes
#> 688                                                  Manana silty clay, 8 to 15 percent slopes
#> 689                                                 Manana silty clay, 15 to 25 percent slopes
#> 690                                         Manana silty clay, 12 to 25 percent slopes, eroded
#> 691                                                 Manana silty clay, 25 to 40 percent slopes
#> 692                                                                              Mokuleia loam
#> 693                                                                         Mokuleia clay loam
#> 694                                                                              Mokuleia clay
#> 695                                   Molokai silty clay loam, 0 to 3 percent slopes, MLRA 158
#> 696                                   Molokai silty clay loam, 3 to 7 percent slopes, MLRA 158
#> 697                                  Molokai silty clay loam, 7 to 15 percent slopes, MLRA 158
#> 698                                           Molokai silty clay loam, 15 to 25 percent slopes
#> 699                                                  Paaloa silty clay, 3 to 12 percent slopes
#> 700                                                  Paaloa silty clay, 3 to 12 percent slopes
#> 701                                                  Paaloa silty clay, 3 to 12 percent slopes
#> 702                                                        Paaloa clay, 2 to 12 percent slopes
#> 703                                                  Paumalu silty clay, 3 to 8 percent slopes
#> 704                                                    Paumalu silty clay, 8-15 percent slopes
#> 705                                                Paumalu silty clay, 15 to 25 percent slopes
#> 706                                                Paumalu silty clay, 25 to 40 percent slopes
#> 707                                                Paumalu silty clay, 40 to 70 percent slopes
#> 708                                         Pearl Harbor clay, 0 to 2 percent slopes, MLRA 163
#> 709                                         Pamoa silty clay, 5 to 20 percent slopes, MLRA 158
#> 710                                  Pohakupu silty clay loam, 0 to 3 percent slopes, MLRA 167
#> 711                                  Pohakupu silty clay loam, 0 to 3 percent slopes, MLRA 167
#> 712                                  Pohakupu silty clay loam, 0 to 3 percent slopes, MLRA 167
#> 713                                           Pohakupu silty clay loam, 8 to 15 percent slopes
#> 714                                         Pulehu clay loam, 0 to 3 percent slopes , MLRA 163
#> 715                                              Pulehu stony clay loam, 2 to 6 percent slopes
#> 716                                        Pulehu very stony clay loam, 0 to 12 percent slopes
#> 717                                                         Papaa clay, 6 to 25 percent slopes
#> 718                                                        Papaa clay, 20 to 35 percent slopes
#> 719                                                        Papaa clay, 35 to 70 percent slopes
#> 720                                                                    Paumalu-Badland complex
#> 721                                                Tantalus silt loam, 15 to 40 percent slopes
#> 722                                                Tantalus silt loam, 40 to 70 percent slopes
#> 723                                           Tantalus silty clay loam, 8 to 15 percent slopes
#> 724                                          Tantalus silty clay loam, 15 to 40 percent slopes
#> 725                                                  Wahiawa silty clay, 0 to 3 percent slopes
#> 726                                                  Wahiawa silty clay, 3 to 8 percent slopes
#> 727                                                 Wahiawa silty clay, 8 to 15 percent slopes
#> 728                                        Wahiawa silty clay, 15 to 25 percent slopes, eroded
#> 729                                                  Waialua silty clay, 0 to 3 percent slopes
#> 730                                                  Waialua silty clay, 3 to 8 percent slopes
#> 731                                            Waialua stony silty clay, 3 to 8 percent slopes
#> 732                                          Waialua stony silty clay, 12 to 30 percent slopes
#> 733                                     Waialua very stony silty clay, 12 to 20 percent slopes
#> 734                                                        Waialua clay, 2 to 6 percent slopes
#> 735                                          Waikane stony silty clay, 15 to 30 percent slopes
#> 736                                                  Waikane silty clay, 3 to 8 percent slopes
#> 737                                                 Waikane silty clay, 8 to 15 percent slopes
#> 738                                                Waikane silty clay, 25 to 40 percent slopes
#> 739                                                Waikane silty clay, 25 to 40 percent slopes
#> 740                                                Waikane silty clay, 25 to 40 percent slopes
#> 741                                                Waikane silty clay, 25 to 40 percent slopes
#> 742                                                Waikane silty clay, 40 to 70 percent slopes
#> 743                                        Waikane silty clay, 40 to 70 percent slopes, eroded
#> 744                                                  Waipahu silty clay, 0 to 2 percent slopes
#> 745                                                  Waipahu silty clay, 2 to 6 percent slopes
#> 746                                                 Waipahu silty clay, 6 to 12 percent slopes
#>        cokey coecoclasskey comppct_r majcompflag             compname
#> 1   27482249      12838179        55         Yes           Lava flows
#> 2   27482265      12838183        60         Yes                Mawae
#> 3   27482269      12838184        60         Yes                Mawae
#> 4   27482285      12838189        25         Yes               Kekake
#> 5   27482287      12838190        20         Yes               Kekake
#> 6   27482289      12838191        60         Yes              Iwalani
#> 7   27482291      12838192        70         Yes              Iwalani
#> 8   27482293      12838193        95         Yes               Ihuanu
#> 9   27482295      12838194        95         Yes               Ihuanu
#> 10  27482345      12838217        60         Yes               Ihuanu
#> 11  27482347      12838218        95         Yes               Ihuanu
#> 12  27482349      12838219        95         Yes               Ihuanu
#> 13  27482351      12838220        30         Yes              Iwalani
#> 14  27482353      12838221        60         Yes              Iwalani
#> 15  27482355      12838222        70         Yes              Iwalani
#> 16  27482373      12838234        95         Yes               Lalaau
#> 17  27482376      12838236        60         Yes              Kahaluu
#> 18  27482381      12838238        55         Yes               Ihuanu
#> 19  27482383      12838239        95         Yes     Humic Durustands
#> 20  27482392      12838245        60         Yes             Kaholimo
#> 21  27482543      12838312        60         Yes             Kaholimo
#> 22  27482407      12838254        40         Yes Lithic Ustipsamments
#> 23  27482412      12838256        50         Yes   Lithic Haplustands
#> 24  27482413      12838257        25         Yes   Lithic Haplustands
#> 25  27482415      12838258        65         Yes              Alahapa
#> 26  27482416      12838259        25         Yes                Heake
#> 27  27482420      12838260        90         Yes              Alahapa
#> 28  27482422      12838261        50         Yes              Alahapa
#> 29  27482426      12838262        90         Yes           Halemaumau
#> 30  27482429      12838263        85         Yes                Heake
#> 31  27482433      12838264        45         Yes                Heake
#> 32  27482438      12838265        90         Yes              Kilauea
#> 33  27482440      12838266        20         Yes             Kaholimo
#> 34  27482441      12838267        13         Yes                Puiwa
#> 35  27482442      12838268        20         Yes             Kaholimo
#> 36  27482443      12838269        12         Yes                Puiwa
#> 37  27482448      12838270        20         Yes             Kaholimo
#> 38  27482449      12838271        10         No              Kaholimo
#> 39  27482462      12838278        55         Yes              Kahalii
#> 40  27482466      12838279        60         Yes             Kanohina
#> 41  27482475      12838282        55         Yes              Nakanui
#> 42  27482479      12838283        90         Yes   Vitric Haplustands
#> 43  27482481      12838284        60         Yes                 Ahiu
#> 44  27482484      12838285        95         Yes               Pakini
#> 45  27482487      12838287         2         No                Oneula
#> 46  27482488      12838288         3         No               Maunaiu
#> 47  27482506      12838298        90         Yes               Oneula
#> 48  27482508      12838299        25         Yes             Menehune
#> 49  27482514      12838300        76         Yes                 Wahi
#> 50  27482522      12838302        95         Yes               Oneula
#> 51  27482528      12838305        40         Yes               Oneula
#> 52  27482538      12838309        45         Yes                 Ahiu
#> 53  27482539      12838310        40         Yes   Vitric Haplustands
#> 54  27482920      12838540        15         No              Waikaloa
#> 55  27482946      12838585        40         Yes            Honokohau
#> 56  27483042      12838814        60         Yes                Mawae
#> 57  27483046      12838739        60         Yes                Mawae
#> 58  27483158      12838439        25         Yes               Kekake
#> 59  27483160      12838511        20         Yes               Kekake
#> 60  27483163      12838756        60         Yes               Puuiki
#> 61  27483166      12838866        60         Yes               Puuiki
#> 62  27483169      12838947        25         Yes               Puuiki
#> 63  27482558      12838864        60         Yes              Iwalani
#> 64  27483172      12838904        60         Yes               Puuiki
#> 65  27483177      12838846        35         Yes               Puuiki
#> 66  27483180      12838686        35         Yes               Puuiki
#> 67  27483187      12838936        35         Yes              Kamawai
#> 68  27482560      12838933        70         Yes              Iwalani
#> 69  27482562      12838521        40         Yes             Kanohina
#> 70  27482564      12838415        90         Yes             Kanohina
#> 71  27483308      12838423        90         Yes         Auwaiakeakua
#> 72  27483310      12838524        90         Yes         Auwaiakeakua
#> 73  27483556      12838712        40         Yes         Auwaiakeakua
#> 74  27483313      12838563        30         Yes               Kiholo
#> 75  27483315      12838852        60         Yes               Kiholo
#> 76  27483317      12838967        60         Yes            Kahaumanu
#> 77  27483319      12838442        60         Yes            Kahaumanu
#> 78  27483321      12838504        30         Yes            Kahaumanu
#> 79  27483454      12838973       100         Yes               Puu Pa
#> 80  27483455      12838590       100         Yes               Puu Pa
#> 81  27483456      12838660        70         Yes             Waikaloa
#> 82  27483457      12838526        30         Yes               Puu Pa
#> 83  27483458      12838572       100         Yes                Puako
#> 84  27483459      12838924        70         Yes             Waikaloa
#> 85  27483461      12838638        28         Yes               Puu Pa
#> 86  27483462      12838407        90         Yes             Waikaloa
#> 87  27483327      12838994        90         Yes             Waikaloa
#> 88  27483465      12838416        90         Yes             Waikaloa
#> 89  27483328      12838443        90         Yes               Nanuku
#> 90  27483467      12838826        90         Yes               Nanuku
#> 91  27483469      12838758        40         Yes               Nanuku
#> 92  27483471      12838991        30         Yes               Hapuna
#> 93  27483472      12838460        70         Yes               Waikui
#> 94  27483473      12838747        70         Yes               Waikui
#> 95  27483474      12838679        30         Yes               Hapuna
#> 96  27483475      12838796        35         Yes               Waikui
#> 97  27483476      12838688        20         Yes             Lalamilo
#> 98  27483477      12838885        40         Yes               Hapuna
#> 99  27483479      12838983        30         Yes               Waikui
#> 100 27483480      12838619        20         Yes               Hapuna
#> 101 27483483      12838804        95         Yes             Kawaihae
#> 102 27483485      12838853        90         Yes             Kawaihae
#> 103 27483487      12838767        90         Yes             Kawaihae
#> 104 27483489      12838772        25         Yes              Kamakoa
#> 105 27483495      12838529        95         Yes              Kamakoa
#> 106 27483496      12838748       100         Yes               Waimea
#> 107 27483497      12838560       100         Yes               Waimea
#> 108 27483498      12838419       100         Yes               Waimea
#> 109 27483499      12838921       100         Yes               Waimea
#> 110 27483375      12838591        60         Yes               Ihuanu
#> 111 27482592      12838573        95         Yes               Ihuanu
#> 112 27482594      12838669        95         Yes               Ihuanu
#> 113 27482596      12838676        90         Yes              Iwalani
#> 114 27482598      12838620        30         Yes              Iwalani
#> 115 27482600      12838611        60         Yes              Iwalani
#> 116 27482602      12838714        70         Yes              Iwalani
#> 117 27482604      12838890        95         Yes              Iwalani
#> 118 27483377      12838496       100         Yes             Nawahine
#> 119 27483501      12838909        85         Yes             Mahukona
#> 120 27483504      12838447        85         Yes             Mahukona
#> 121 27483506      12838409        95         Yes                 Hawi
#> 122 27483508      12838665        95         Yes                 Hawi
#> 123 27483510      12838680        95         Yes                 Hawi
#> 124 27483512      12839000       100         Yes               Kohala
#> 125 27483513      12838762        85         Yes               Waimea
#> 126 27483516      12838963        85         Yes               Waimea
#> 127 27483519      12838628        85         Yes               Waimea
#> 128 27483520      12838856        15         No                Waimea
#> 129 27483578      12838682       100         Yes               Kohala
#> 130 27483579      12838733       100         Yes               Kohala
#> 131 27483580      12838888       100         Yes               Kohala
#> 132 27483581      12838832       100         Yes               Kohala
#> 133 27483582      12838773       100         Yes              Ainakea
#> 134 27483583      12838720       100         Yes              Ainakea
#> 135 27483584      12838552       100         Yes              Ainakea
#> 136 27483658      12838574        95         Yes               Kemole
#> 137 27483660      12838672        95         Yes               Kemole
#> 138 27483662      12838684       100         Yes              Paauhau
#> 139 27483420      12838857        60         Yes              Nenenui
#> 140 27483422      12838805        60         Yes              Nenenui
#> 141 27483666      12838774       100         Yes              Paauhau
#> 142 27483424      12838851        90         Yes              Ohianui
#> 143 27483427      12838960        90         Yes              Ohianui
#> 144 27483667      12838768        70         Yes              Paauhau
#> 145 27483433      12838775        90         Yes               Huikau
#> 146 27483600      12838730       100         Yes               Kikoni
#> 147 27482607      12838723        95         Yes             Kaalualu
#> 148 27482608      12838928        95         Yes             Kaalualu
#> 149 27482611      12838493        95         Yes             Kaalualu
#> 150 27482612      12838968        40         Yes               Pakini
#> 151 27482613      12838435        60         Yes             Kaalualu
#> 152 27482614      12838800        95         Yes               Pakini
#> 153 27482616      12838878        40         Yes             Kaalualu
#> 154 27482617      12838475        60         Yes               Pakini
#> 155 27482628      12838964        95         Yes               Lalaau
#> 156 27482645      12838440        60         Yes              Kahaluu
#> 157 27482647      12838411        70         Yes Lithic Haplotorrands
#> 158 27482660      12838623        88         Yes                Puiwa
#> 159 27482706      12838578        55         Yes               Ihuanu
#> 160 27482708      12838629        95         Yes     Humic Durustands
#> 161 27482723      12838958        75         Yes             Kaholimo
#> 162 27482733      12838882        60         Yes             Kaholimo
#> 163 27482740      12838760        60         Yes             Kaholimo
#> 164 27482757      12838470        90         Yes              Kahaluu
#> 165 27482780      12838535        50         Yes               Lalaau
#> 166 27482783      12838873        45         Yes              Kahaluu
#> 167 27482784      12838883        25         Yes              Ainahou
#> 168 27482791      12838855        50         Yes               Nanaia
#> 169 27482792      12838432        40         Yes              Ohaikea
#> 170 27482805      12838884        60         Yes               Nanaia
#> 171 27482868      12838507        40         Yes Lithic Ustipsamments
#> 172 27483827      12838879        90         Yes              Alahapa
#> 173 27482870      12838465        50         Yes              Alahapa
#> 174 27483829      12838910        85         Yes                Heake
#> 175 27483833      12838603        45         Yes                Heake
#> 176 27483747      12838697        20         Yes             Kaholimo
#> 177 27483748      12838427        10         No              Kaholimo
#> 178 27483554      12838494        60         Yes             Kanohina
#> 179 27483550      12838527        95         Yes               Pakini
#> 180 27482883      12838750         2         No                Oneula
#> 181 27482884      12838955         3         No               Maunaiu
#> 182 27482902      12838597        90         Yes               Oneula
#> 183 27482904      12838588        25         Yes             Menehune
#> 184 27483849      12838731        76         Yes                 Wahi
#> 185 27483855      12838445        40         Yes               Oneula
#> 186 27483537      12838449        70         Yes               Puu Pa
#> 187 27483538      12838709        20         Yes             Waikaloa
#> 188 27483540      12838514         5         No                Puu Pa
#> 189 27483543      12838703        25         Yes               Puu Pa
#> 190 27483544      12838911        65         Yes               Puu Pa
#> 191 27483736      12838437        15         No            Lanapohaku
#> 192 27483549      12838580       100         Yes                Puako
#> 193 27483783      12838428        15         No            Lanapohaku
#> 194 27483785      12838434        15         No            Lanapohaku
#> 195 27483789      12838735        65         Yes           Lanapohaku
#> 196 27483805      12838663        85         Yes           Pohakulehu
#> 197 27483808      12838875        30         Yes           Pohakulehu
#> 198 27483809      12838642        30         Yes                 Lapa
#> 199 27483609      12838728        45         Yes           Lanapohaku
#> 200 27483610      12838922        35         Yes           Pohakulehu
#> 201 27483621      12838457        85         Yes                 Hilo
#> 202 27483624      12838807        80         Yes                 Hilo
#> 203 27483627      12838781        80         Yes                 Hilo
#> 204 27483647      12838771        40         Yes               Onomea
#> 205 27483631      12838920        10         No                Onomea
#> 206 27483643      12838932        65         Yes                 Hilo
#> 207 27483698      12838566        95         Yes               Ookala
#> 208 27483654      12839012        95         Yes               Ookala
#> 209 27483701      12838732        95         Yes               Ookala
#> 210 27483704      12838757        55         Yes               Ookala
#> 211 27483687      12838982        95         Yes              Paauhau
#> 212 27483720      12838710        95         Yes                 Olaa
#> 213 27483751      12838907        30         Yes               Onomea
#> 214 27483863      12839098       100         Yes              Alaeloa
#> 215 27483860      12839095       100         Yes              Alaeloa
#> 216 27483861      12839096       100         Yes              Alaeloa
#> 217 27483862      12839097       100         Yes              Alaeloa
#> 218 27483869      12839101       100         Yes              Haleiwa
#> 219 27483870      12839102       100         Yes              Haleiwa
#> 220 27483867      12839099       100         Yes               Halawa
#> 221 27483868      12839100       100         Yes               Halawa
#> 222 27483871      12839103       100         Yes              Holomua
#> 223 27483872      12839104       100         Yes              Holomua
#> 224 27483873      12839105       100         Yes              Holomua
#> 225 27483874      12839106       100         Yes              Holomua
#> 226 27483875      12839107       100         Yes              Holomua
#> 227 27483877      12839108         5         No               Molokai
#> 228 27483878      12839109        90         Yes             Hoolehua
#> 229 27483879      12839110       100         Yes             Hoolehua
#> 230 27483880      12839111       100         Yes             Hoolehua
#> 231 27483881      12839112       100         Yes             Hoolehua
#> 232 27483882      12839113       100         Yes             Hoolehua
#> 233 27483885      12839115       100         Yes               Jaucas
#> 234 27483883      12839114        55         Yes               Jaucas
#> 235 27483896      12839122       100         Yes                Kalae
#> 236 27483897      12839123       100         Yes                Kalae
#> 237 27483898      12839124       100         Yes                Kalae
#> 238 27483899      12839125       100         Yes                Kalae
#> 239 27483900      12839126       100         Yes                Kalae
#> 240 27483887      12839117        85         Yes            Kalaupapa
#> 241 27483889      12839118        90         Yes           Kapuhikani
#> 242 27483901      12839127       100         Yes           Kawaihapai
#> 243 27483902      12839128       100         Yes           Kawaihapai
#> 244 27483903      12839129       100         Yes           Kawaihapai
#> 245 27483904      12839130       100         Yes           Kawaihapai
#> 246 27483892      12839119        85         Yes               Kealia
#> 247 27483893      12839120        10         No                Kealia
#> 248 27483895      12839121        70         Yes                Koele
#> 249 27483905      12839131       100         Yes              Lahaina
#> 250 27483906      12839132       100         Yes              Lahaina
#> 251 27483907      12839133       100         Yes              Lahaina
#> 252 27483908      12839134       100         Yes              Lahaina
#> 253 27483909      12839135       100         Yes              Lahaina
#> 254 27483910      12839136       100         Yes              Lahaina
#> 255 27483911      12839137        95         Yes            Lualualei
#> 256 27483912      12839138         5         No                   Ewa
#> 257 27483914      12839139       100         Yes                 Mala
#> 258 27483915      12839140       100         Yes                 Mala
#> 259 27483916      12839141       100         Yes              Molokai
#> 260 27483917      12839142       100         Yes              Molokai
#> 261 27483918      12839143       100         Yes              Molokai
#> 262 27483919      12839144       100         Yes              Molokai
#> 263 27483920      12839145       100         Yes              Molokai
#> 264 27483923      12839146         5         No                   Oli
#> 265 27483924      12839147        85         Yes                Naiwa
#> 266 27483925      12839148         5         No                 Naiwa
#> 267 27483926      12839149         5         No                 Olelo
#> 268 27483927      12839150       100         Yes                Naiwa
#> 269 27483928      12839151       100         Yes               Niulii
#> 270 27483929      12839152       100         Yes               Niulii
#> 271 27483930      12839153       100         Yes                Olelo
#> 272 27483931      12839154        90         Yes                  Oli
#> 273 27483932      12839155        10         No                 Naiwa
#> 274 27483933      12839156        90         Yes                  Oli
#> 275 27483934      12839157        10         No                 Naiwa
#> 276 27483937      12839160       100         Yes                Pamoa
#> 277 27483938      12839161       100         Yes                Pamoa
#> 278 27483939      12839162       100         Yes                Pamoa
#> 279 27483941      12839164       100         Yes               Pulehu
#> 280 27483940      12839163       100         Yes               Pulehu
#> 281 27483942      12839165        85         Yes               Pulehu
#> 282 27483947      12839166       100         Yes              Waihuna
#> 283 27483948      12839167       100         Yes              Waihuna
#> 284 27483949      12839168       100         Yes              Waihuna
#> 285 27483953      12839169        85         Yes              Waikapu
#> 286 27483954      12839170       100         Yes              Waikapu
#> 287 27483955      12839171       100         Yes              Waikapu
#> 288 27483959      12839172        80         Yes              Waikapu
#> 289 27483982      12839177        40         Yes               Mahana
#> 290 27483989      12839181       100         Yes                Halii
#> 291 27483990      12839182       100         Yes                Halii
#> 292 27483991      12839183       100         Yes                Halii
#> 293 27483992      12839184       100         Yes                Halii
#> 294 27483993      12839185        95         Yes              Hanalei
#> 295 27483994      12839186         5         No              Mokuleia
#> 296 27483986      12839178       100         Yes             Hihimanu
#> 297 27483996      12839188        85         Yes              Hanalei
#> 298 27483997      12839189       100         Yes              Hanalei
#> 299 27483998      12839190       100         Yes              Hanalei
#> 300 27483999      12839191       100         Yes            Hanamaulu
#> 301 27484000      12839192       100         Yes            Hanamaulu
#> 302 27484001      12839193       100         Yes            Hanamaulu
#> 303 27484002      12839194       100         Yes            Hanamaulu
#> 304 27484003      12839195       100         Yes            Hanamaulu
#> 305 27484004      12839196       100         Yes            Hanamaulu
#> 306 27484005      12839197       100         Yes               Ioleau
#> 307 27484006      12839198       100         Yes               Ioleau
#> 308 27484007      12839199       100         Yes               Ioleau
#> 309 27484008      12839200       100         Yes               Ioleau
#> 310 27484009      12839201       100         Yes               Jaucas
#> 311 27484010      12839202       100         Yes       Jaucas variant
#> 312 27484020      12839211       100         Yes        Kaena variant
#> 313 27484021      12839212       100         Yes        Kaena variant
#> 314 27484022      12839213       100         Yes               Kalapa
#> 315 27484023      12839214       100         Yes               Kalapa
#> 316 27484024      12839215       100         Yes               Kalapa
#> 317 27484025      12839216       100         Yes               Kalihi
#> 318 27484011      12839203        75         Yes               Kalapa
#> 319 27484026      12839217        60         Yes               Kaloko
#> 320 27484027      12839218        40         Yes      Kaloko, drained
#> 321 27484028      12839219        85         Yes               Kaloko
#> 322 27484029      12839220        15         No                Kaloko
#> 323 27484030      12839221       100         Yes                Kapaa
#> 324 27484031      12839222       100         Yes                Kapaa
#> 325 27484032      12839223       100         Yes                Kapaa
#> 326 27484033      12839224       100         Yes                Kapaa
#> 327 27484034      12839225       100         Yes               Kekaha
#> 328 27484035      12839226       100         Yes               Kekaha
#> 329 27484036      12839227       100         Yes               Kekaha
#> 330 27484013      12839204       100         Yes               Kekaha
#> 331 27484016      12839207       100         Yes             Kolokolo
#> 332 27484037      12839228       100         Yes                Koloa
#> 333 27484038      12839229       100         Yes                Koloa
#> 334 27484039      12839230       100         Yes                Koloa
#> 335 27484018      12839209       100         Yes               Koolau
#> 336 27484040      12839231       100         Yes             Kolokolo
#> 337 27484042      12839233       100         Yes                Lawai
#> 338 27484043      12839234       100         Yes                Lawai
#> 339 27484044      12839235       100         Yes                Lawai
#> 340 27484045      12839236       100         Yes                Lihue
#> 341 27484046      12839237       100         Yes                Lihue
#> 342 27484047      12839238       100         Yes                Lihue
#> 343 27484048      12839239       100         Yes                Lihue
#> 344 27484049      12839240       100         Yes                Lihue
#> 345 27484050      12839241       100         Yes                Lihue
#> 346 27484041      12839232       100         Yes            Lualualei
#> 347 27484051      12839242        95         Yes            Lualualei
#> 348 27484052      12839243         5         No                   Ewa
#> 349 27484053      12839244       100         Yes            Lualualei
#> 350 27484055      12839245       100         Yes               Mahana
#> 351 27484056      12839246       100         Yes               Mahana
#> 352 27484057      12839247       100         Yes               Mahana
#> 353 27484058      12839248       100         Yes               Mahana
#> 354 27484059      12839249       100         Yes               Mahana
#> 355 27484060      12839250       100         Yes             Makapili
#> 356 27484061      12839251       100         Yes             Makapili
#> 357 27484062      12839252       100         Yes             Makapili
#> 358 27484063      12839253       100         Yes             Makapili
#> 359 27484064      12839254       100         Yes             Makaweli
#> 360 27484065      12839255       100         Yes             Makaweli
#> 361 27484066      12839256       100         Yes             Makaweli
#> 362 27484067      12839257       100         Yes             Makaweli
#> 363 27484068      12839258       100         Yes             Makaweli
#> 364 27484069      12839259       100         Yes             Makaweli
#> 365 27484070      12839260       100         Yes             Makaweli
#> 366 27484071      12839261       100         Yes             Makaweli
#> 367 27484072      12839262        90         Yes               Mamala
#> 368 27484074      12839263       100         Yes             Mokuleia
#> 369 27484075      12839264       100         Yes     Mokuleia variant
#> 370 27484076      12839265       100         Yes                  Niu
#> 371 27484077      12839266       100         Yes                  Niu
#> 372 27484078      12839267       100         Yes                  Niu
#> 373 27484079      12839268       100         Yes                  Niu
#> 374 27484080      12839269       100         Yes               Nohili
#> 375 27484081      12839270       100         Yes             Nonopahu
#> 376 27484082      12839271       100         Yes             Nonopahu
#> 377 27484088      12839276        90         Yes                  Oli
#> 378 27484083      12839272        90         Yes                  Oli
#> 379 27484084      12839273        10         No                 Naiwa
#> 380 27484085      12839274        90         Yes                  Oli
#> 381 27484086      12839275        10         No                 Naiwa
#> 382 27484092      12839280       100         Yes               Pakala
#> 383 27484093      12839281       100         Yes               Pakala
#> 384 27484091      12839279       100         Yes               Pakala
#> 385 27484094      12839282        90         Yes             Pohakupu
#> 386 27484097      12839283       100         Yes                Pooku
#> 387 27484098      12839284       100         Yes                Pooku
#> 388 27484099      12839285       100         Yes                Pooku
#> 389 27484100      12839286       100         Yes                Pooku
#> 390 27484101      12839287       100         Yes                Pooku
#> 391 27484102      12839288       100         Yes                Pooku
#> 392 27484103      12839289       100         Yes                 Puhi
#> 393 27484104      12839290       100         Yes                 Puhi
#> 394 27484105      12839291       100         Yes                 Puhi
#> 395 27484106      12839292       100         Yes                 Puhi
#> 396 27484107      12839293       100         Yes                 Puhi
#> 397 27484108      12839294       100         Yes             Puu Opae
#> 398 27484109      12839295       100         Yes             Puu Opae
#> 399 27484110      12839296       100         Yes             Puu Opae
#> 400 27484112      12839297        60         Yes               Waiawa
#> 401 27484114      12839298       100         Yes              Waikomo
#> 402 27484115      12839299        65         Yes              Waikomo
#> 403 27484117      12839300        60         Yes              Waikomo
#> 404 27486179      12839353        15         No                Mamala
#> 405 27486103      12839304       100         Yes               Jaucas
#> 406 27486107      12839307       100         Yes                Kalae
#> 407 27486108      12839308       100         Yes                Kalae
#> 408 27486109      12839309       100         Yes                Kalae
#> 409 27486110      12839310        95         Yes              Kanepuu
#> 410 27486112      12839311        85         Yes              Kanepuu
#> 411 27486115      12839312        95         Yes              Kanepuu
#> 412 27486116      12839313        85         Yes              Kanepuu
#> 413 27486118      12839314       100         Yes                Koele
#> 414 27486119      12839315       100         Yes                Koele
#> 415 27486120      12839316       100         Yes                Koele
#> 416 27486106      12839306        70         Yes                Koele
#> 417 27486121      12839317       100         Yes              Lahaina
#> 418 27486122      12839318       100         Yes              Lahaina
#> 419 27486123      12839319       100         Yes              Lahaina
#> 420 27486124      12839320       100         Yes              Lahaina
#> 421 27486125      12839321       100         Yes              Lahaina
#> 422 27486126      12839322       100         Yes              Lahaina
#> 423 27486127      12839323       100         Yes              Lahaina
#> 424 27486128      12839324        95         Yes            Lualualei
#> 425 27486129      12839325         5         No                   Ewa
#> 426 27486130      12839326       100         Yes                 Mala
#> 427 27486131      12839327       100         Yes                 Mala
#> 428 27486132      12839328       100         Yes              Molokai
#> 429 27486133      12839329       100         Yes              Molokai
#> 430 27486134      12839330       100         Yes              Molokai
#> 431 27486135      12839331       100         Yes              Molokai
#> 432 27486138      12839332         5         No                   Oli
#> 433 27486139      12839333        85         Yes                Naiwa
#> 434 27486140      12839334         5         No                 Naiwa
#> 435 27486141      12839335         5         No                 Olelo
#> 436 27486144      12839338       100         Yes                Pamoa
#> 437 27486145      12839339       100         Yes                Pamoa
#> 438 27486147      12839341       100         Yes               Pulehu
#> 439 27486146      12839340       100         Yes               Pulehu
#> 440 27486148      12839342        85         Yes               Pulehu
#> 441 27486152      12839343       100         Yes                Uwala
#> 442 27486153      12839344       100         Yes                Uwala
#> 443 27486154      12839345       100         Yes                Uwala
#> 444 27486156      12839346       100         Yes              Waihuna
#> 445 27486157      12839347       100         Yes              Waihuna
#> 446 27486158      12839348       100         Yes              Waihuna
#> 447 27486159      12839349       100         Yes              Waihuna
#> 448 27486160      12839350       100         Yes              Waihuna
#> 449 27486164      12839351        85         Yes              Waikapu
#> 450 27486168      12839352        80         Yes              Waikapu
#> 451 27486186      12839354        90         Yes                 Alae
#> 452 27486189      12839355        90         Yes                 Alae
#> 453 27486190      12839356       100         Yes                 Alae
#> 454 27486191      12839357       100         Yes              Alaeloa
#> 455 27486192      12839358       100         Yes              Alaeloa
#> 456 27486193      12839359       100         Yes              Alaeloa
#> 457 27486196      12839360       100         Yes                  Ewa
#> 458 27486197      12839361       100         Yes                  Ewa
#> 459 27486198      12839362       100         Yes                  Ewa
#> 460 27486199      12839363       100         Yes                  Ewa
#> 461 27486200      12839364       100         Yes                  Ewa
#> 462 27486201      12839365       100         Yes                  Ewa
#> 463 27486210      12839371        82         Yes                Haiku
#> 464 27486214      12839372        85         Yes                Haiku
#> 465 27486218      12839373        85         Yes                Haiku
#> 466 27486221      12839374        85         Yes                Haiku
#> 467 27486224      12839375       100         Yes           Haliimaile
#> 468 27486225      12839376       100         Yes           Haliimaile
#> 469 27486226      12839377         5         No                Keahua
#> 470 27486227      12839378         5         No                  Paia
#> 471 27486228      12839379        90         Yes           Haliimaile
#> 472 27486229      12839380       100         Yes           Haliimaile
#> 473 27486203      12839366       100         Yes               Halawa
#> 474 27486230      12839381       100         Yes           Haliimaile
#> 475 27486204      12839367       100         Yes                 Hana
#> 476 27486205      12839368       100         Yes                 Hana
#> 477 27486206      12839369       100         Yes         Hana variant
#> 478 27486207      12839370       100         Yes         Hana variant
#> 479 27486231      12839382       100         Yes          Hamakuapoko
#> 480 27486232      12839383       100         Yes          Hamakuapoko
#> 481 27486233      12839384       100         Yes          Hamakuapoko
#> 482 27486234      12839385       100         Yes              Honolua
#> 483 27486235      12839386       100         Yes              Honolua
#> 484 27486237      12839388        95         Yes                  Iao
#> 485 27486239      12839389        90         Yes                  Iao
#> 486 27486242      12839390       100         Yes                  Iao
#> 487 27486243      12839391       100         Yes                  Iao
#> 488 27486246      12839392        85         Yes                  Iao
#> 489 27486248      12839393        90         Yes                  Iao
#> 490 27486236      12839387       100         Yes                   Io
#> 491 27486252      12839394       100         Yes               Jaucas
#> 492 27486253      12839395       100         Yes               Jaucas
#> 493 27486274      12839408       100         Yes               Kahana
#> 494 27486275      12839409       100         Yes               Kahana
#> 495 27486276      12839410       100         Yes               Kahana
#> 496 27486254      12839396       100         Yes               Kailua
#> 497 27486255      12839397       100         Yes                Kaimu
#> 498 27486259      12839400       100         Yes              Kamaole
#> 499 27486260      12839401       100         Yes              Kamaole
#> 500 27486261      12839402        95         Yes                Kaupo
#> 501 27486263      12839403        95         Yes                Kaupo
#> 502 27486265      12839404        85         Yes               Kealia
#> 503 27486266      12839405        10         No                Kealia
#> 504 27486279      12839413       100         Yes               Keahua
#> 505 27486280      12839414       100         Yes               Keahua
#> 506 27486281      12839415       100         Yes               Keahua
#> 507 27486277      12839411       100         Yes               Keahua
#> 508 27486282      12839416       100         Yes               Keahua
#> 509 27486278      12839412       100         Yes               Keahua
#> 510 27486283      12839417       100         Yes               Keahua
#> 511 27486284      12839418       100         Yes               Keahua
#> 512 27486285      12839419       100         Yes               Keahua
#> 513 27486268      12839406        75         Yes            Keawakapu
#> 514 27486272      12839407        40         Yes                Koele
#> 515 27486288      12839422       100         Yes                 Kula
#> 516 27486289      12839423        80         Yes                 Kula
#> 517 27486286      12839420       100         Yes                 Kula
#> 518 27486287      12839421       100         Yes                 Kula
#> 519 27486294      12839427       100         Yes              Lahaina
#> 520 27486295      12839428       100         Yes              Lahaina
#> 521 27486296      12839429       100         Yes              Lahaina
#> 522 27486307      12839437         5         No                Kailua
#> 523 27486308      12839438         5         No                 Haiku
#> 524 27486309      12839439        90         Yes              Makawao
#> 525 27486310      12839440       100         Yes              Makawao
#> 526 27486297      12839430       100         Yes             Makaalae
#> 527 27486298      12839431       100         Yes             Makaalae
#> 528 27486311      12839441       100         Yes              Molokai
#> 529 27486312      12839442       100         Yes              Molokai
#> 530 27486313      12839443       100         Yes              Molokai
#> 531 27486299      12839432       100         Yes             Makaalae
#> 532 27486300      12839433         3         No             Keawakapu
#> 533 27486302      12839434        46         Yes               Makena
#> 534 27486303      12839435         2         No              Oanapuka
#> 535 27486305      12839436        90         Yes               Malama
#> 536 27486314      12839444        90         Yes                Naiwa
#> 537 27486315      12839445         5         No                 Naiwa
#> 538 27486316      12839446         5         No                 Olelo
#> 539 27486319      12839447        90         Yes             Oanapuka
#> 540 27486320      12839448       100         Yes             Oanapuka
#> 541 27486321      12839449       100         Yes                Olelo
#> 542 27486322      12839450        90         Yes                  Oli
#> 543 27486324      12839451       100         Yes               Olinda
#> 544 27486325      12839452       100         Yes               Olinda
#> 545 27486335      12839460       100         Yes                 Paia
#> 546 27486336      12839461       100         Yes                 Paia
#> 547 27486337      12839462       100         Yes                 Paia
#> 548 27486338      12839463         5         No                Kailua
#> 549 27486339      12839464         5         No                 Haiku
#> 550 27486340      12839465        90         Yes              Pauwela
#> 551 27486341      12839466       100         Yes              Pauwela
#> 552 27486342      12839467       100         Yes              Pauwela
#> 553 27486343      12839468       100         Yes               Pulehu
#> 554 27486344      12839469       100         Yes               Pulehu
#> 555 27486345      12839470       100         Yes               Pulehu
#> 556 27486346      12839471       100         Yes               Pulehu
#> 557 27486347      12839472        85         Yes               Pulehu
#> 558 27486351      12839473       100         Yes               Pulehu
#> 559 27486352      12839474       100         Yes               Pulehu
#> 560 27486329      12839455       100         Yes                 Pane
#> 561 27486330      12839456         5         No                   Iao
#> 562 27486331      12839457         5         No                Jaucas
#> 563 27486332      12839458        90         Yes               Puuone
#> 564 27486333      12839459        90         Yes               Puu Pa
#> 565 27486354      12839475       100         Yes           Ulupalakua
#> 566 27486357      12839478        93         Yes                  Uma
#> 567 27486361      12839480       100         Yes             Wahikuli
#> 568 27486362      12839481       100         Yes             Wahikuli
#> 569 27486363      12839482       100         Yes             Wahikuli
#> 570 27486364      12839483       100         Yes             Wahikuli
#> 571 27486365      12839484       100         Yes              Waiakoa
#> 572 27486366      12839485       100         Yes              Waiakoa
#> 573 27486367      12839486       100         Yes              Waiakoa
#> 574 27486370      12839487        90         Yes              Waiakoa
#> 575 27486371      12839488       100         Yes              Waiakoa
#> 576 27486372      12839489       100         Yes              Waiakoa
#> 577 27486373      12839490       100         Yes              Waiakoa
#> 578 27486360      12839479       100         Yes              Waiakoa
#> 579 27486374      12839491       100         Yes              Wailuku
#> 580 27486375      12839492         5         No                   Iao
#> 581 27486376      12839493        90         Yes              Wailuku
#> 582 27486377      12839494         5         No                Pulehu
#> 583 27486378      12839495       100         Yes              Wailuku
#> 584 27486379      12839496       100         Yes               Wainee
#> 585 27486380      12839497       100         Yes               Wainee
#> 586 27486381      12839498       100         Yes               Wainee
#> 587 27486382      12839499        90         Yes               Wainee
#> 588 27486409      12840201       100         Yes              Alaeloa
#> 589 27486408      12840200       100         Yes              Alaeloa
#> 590 27486411      12840202        15         No                Mamala
#> 591 27486413      12840203       100         Yes                  Ewa
#> 592 27486414      12840204       100         Yes                  Ewa
#> 593 27486415      12840205       100         Yes                  Ewa
#> 594 27486416      12840206       100         Yes                  Ewa
#> 595 27486417      12840207       100         Yes                  Ewa
#> 596 27486418      12840208       100         Yes                  Ewa
#> 597 27486419      12840209       100         Yes                  Ewa
#> 598 27486425      12840213        95         Yes              Haleiwa
#> 599 27486427      12840215       100         Yes              Haleiwa
#> 600 27486422      12840210       100         Yes               Halawa
#> 601 27486423      12840211       100         Yes               Halawa
#> 602 27486424      12840212       100         Yes             Helemano
#> 603 27486429      12840217        85         Yes              Hanalei
#> 604 27486431      12840219        85         Yes              Hanalei
#> 605 27486432      12840220        85         Yes              Hanalei
#> 606 27486434      12840222       100         Yes           Honouliuli
#> 607 27486435      12840223       100         Yes           Honouliuli
#> 608 27486436      12840224       100         Yes               Jaucas
#> 609 27486437      12840225       100         Yes               Jaucas
#> 610 27486447      12840233       100         Yes                Kaena
#> 611 27486448      12840234       100         Yes                Kaena
#> 612 27486449      12840235       100         Yes                Kaena
#> 613 27486450      12840236       100         Yes                Kaena
#> 614 27486451      12840237       100         Yes                Kaena
#> 615 27486452      12840238       100         Yes                Kaena
#> 616 27486453      12840239        85         Yes               Kaloko
#> 617 27486454      12840240        15         No                Kaloko
#> 618 27486455      12840241        10         No            Kawaihapai
#> 619 27486456      12840242        90         Yes       Kaloko variant
#> 620 27486457      12840243       100         Yes              Kaneohe
#> 621 27486458      12840244       100         Yes              Kaneohe
#> 622 27486438      12840226       100         Yes              Kaneohe
#> 623 27486439      12840227       100         Yes              Kaneohe
#> 624 27486440      12840228       100         Yes              Kaneohe
#> 625 27486441      12840229       100         Yes              Kaneohe
#> 626 27486442      12840230        75         Yes                Kapaa
#> 627 27486459      12840245       100         Yes           Kawaihapai
#> 628 27486462      12840248       100         Yes           Kawaihapai
#> 629 27486463      12840249       100         Yes           Kawaihapai
#> 630 27486460      12840246       100         Yes           Kawaihapai
#> 631 27486464      12840250       100         Yes           Kawaihapai
#> 632 27486461      12840247       100         Yes           Kawaihapai
#> 633 27486466      12840251        10         No          Pearl Harbor
#> 634 27486467      12840252        85         Yes                Keaau
#> 635 27486468      12840253       100         Yes                Keaau
#> 636 27486469      12840254       100         Yes                Keaau
#> 637 27486470      12840255       100         Yes                Kemoo
#> 638 27486471      12840256       100         Yes                Kemoo
#> 639 27486472      12840257       100         Yes                Kemoo
#> 640 27486473      12840258       100         Yes                Kemoo
#> 641 27486474      12840259       100         Yes                Kemoo
#> 642 27486444      12840231        60         Yes                Kemoo
#> 643 27486475      12840260       100         Yes                 Koko
#> 644 27486476      12840261       100         Yes                 Koko
#> 645 27486477      12840262       100         Yes                 Koko
#> 646 27486478      12840263       100         Yes             Kokokahi
#> 647 27486446      12840232       100         Yes             Kokokahi
#> 648 27486479      12840264       100         Yes             Kolekole
#> 649 27486480      12840265       100         Yes             Kolekole
#> 650 27486481      12840266       100         Yes             Kolekole
#> 651 27486482      12840267       100         Yes                Kunia
#> 652 27486483      12840268       100         Yes                Kunia
#> 653 27486484      12840269       100         Yes                Kunia
#> 654 27486486      12840271       100         Yes              Lahaina
#> 655 27486487      12840272       100         Yes              Lahaina
#> 656 27486488      12840273       100         Yes              Lahaina
#> 657 27486489      12840274       100         Yes              Lahaina
#> 658 27486490      12840275       100         Yes             Leilehua
#> 659 27486491      12840276       100         Yes             Leilehua
#> 660 27486492      12840277         3         No               Hanalei
#> 661 27486493      12840278         2         No               Kaneohe
#> 662 27486494      12840279        95         Yes              Lolekaa
#> 663 27486495      12840280       100         Yes              Lolekaa
#> 664 27486496      12840281       100         Yes              Lolekaa
#> 665 27486497      12840282       100         Yes              Lolekaa
#> 666 27486498      12840283       100         Yes              Lolekaa
#> 667 27486485      12840270       100         Yes            Lualualei
#> 668 27486499      12840284        95         Yes            Lualualei
#> 669 27486500      12840285         5         No                   Ewa
#> 670 27486501      12840286       100         Yes            Lualualei
#> 671 27486502      12840287       100         Yes            Lualualei
#> 672 27486503      12840288       100         Yes            Lualualei
#> 673 27486504      12840289        55         Yes               Mahana
#> 674 27486507      12840290       100         Yes               Mahana
#> 675 27486508      12840291       100         Yes               Mahana
#> 676 27486509      12840292       100         Yes               Mahana
#> 677 27486510      12840293         5         No                Mamala
#> 678 27486511      12840294        95         Yes             Makalapa
#> 679 27486512      12840295       100         Yes             Makalapa
#> 680 27486513      12840296       100         Yes             Makalapa
#> 681 27486514      12840297       100         Yes               Makiki
#> 682 27486515      12840298       100         Yes               Makiki
#> 683 27486516      12840299        90         Yes               Mamala
#> 684 27486518      12840300       100         Yes               Manana
#> 685 27486519      12840301       100         Yes               Manana
#> 686 27486520      12840302       100         Yes               Manana
#> 687 27486521      12840303       100         Yes               Manana
#> 688 27486522      12840304       100         Yes               Manana
#> 689 27486523      12840305       100         Yes               Manana
#> 690 27486524      12840306       100         Yes               Manana
#> 691 27486525      12840307       100         Yes               Manana
#> 692 27486526      12840308       100         Yes             Mokuleia
#> 693 27486527      12840309       100         Yes             Mokuleia
#> 694 27486528      12840310       100         Yes             Mokuleia
#> 695 27486529      12840311       100         Yes              Molokai
#> 696 27486530      12840312       100         Yes              Molokai
#> 697 27486531      12840313       100         Yes              Molokai
#> 698 27486532      12840314       100         Yes              Molokai
#> 699 27486539      12840320         3         No                Manana
#> 700 27486540      12840321         2         No              Leilehua
#> 701 27486541      12840322        95         Yes               Paaloa
#> 702 27486542      12840323       100         Yes               Paaloa
#> 703 27486543      12840324       100         Yes              Paumalu
#> 704 27486544      12840325       100         Yes              Paumalu
#> 705 27486545      12840326       100         Yes              Paumalu
#> 706 27486546      12840327       100         Yes              Paumalu
#> 707 27486547      12840328       100         Yes              Paumalu
#> 708 27486548      12840329       100         Yes         Pearl Harbor
#> 709 27486533      12840315       100         Yes                Pamoa
#> 710 27486549      12840330         5         No               Waialua
#> 711 27486550      12840331        90         Yes             Pohakupu
#> 712 27486551      12840332         5         No               Alaeloa
#> 713 27486552      12840333       100         Yes             Pohakupu
#> 714 27486553      12840334        85         Yes               Pulehu
#> 715 27486557      12840335       100         Yes               Pulehu
#> 716 27486558      12840336       100         Yes               Pulehu
#> 717 27486534      12840316       100         Yes                Papaa
#> 718 27486535      12840317       100         Yes                Papaa
#> 719 27486536      12840318       100         Yes                Papaa
#> 720 27486537      12840319        60         Yes              Paumalu
#> 721 27486559      12840337       100         Yes             Tantalus
#> 722 27486560      12840338       100         Yes             Tantalus
#> 723 27486561      12840339       100         Yes             Tantalus
#> 724 27486562      12840340       100         Yes             Tantalus
#> 725 27486566      12840341       100         Yes              Wahiawa
#> 726 27486567      12840342       100         Yes              Wahiawa
#> 727 27486568      12840343       100         Yes              Wahiawa
#> 728 27486569      12840344       100         Yes              Wahiawa
#> 729 27486570      12840345       100         Yes              Waialua
#> 730 27486571      12840346       100         Yes              Waialua
#> 731 27486572      12840347       100         Yes              Waialua
#> 732 27486573      12840348       100         Yes              Waialua
#> 733 27486574      12840349       100         Yes              Waialua
#> 734 27486575      12840350       100         Yes              Waialua
#> 735 27486584      12840359       100         Yes              Waikane
#> 736 27486598      12840351       100         Yes              Waikane
#> 737 27486576      12840352       100         Yes              Waikane
#> 738 27486577      12840353        94         Yes              Waikane
#> 739 27486578      12840354         2         No               Lolekaa
#> 740 27486579      12840355         2         No               Kaneohe
#> 741 27486580      12840356         2         No               Alaeloa
#> 742 27486581      12840357       100         Yes              Waikane
#> 743 27486582      12840358        98         Yes              Waikane
#> 744 27486585      12840360       100         Yes              Waipahu
#> 745 27486586      12840361       100         Yes              Waipahu
#> 746 27486587      12840362       100         Yes              Waipahu
#>                      localphase           compkind  ecoclassid
#> 1      `a`a, sparsely vegetated Miscellaneous area R161AY008HI
#> 2                          <NA>             Series R161AY003HI
#> 3                          <NA>             Series R161AY003HI
#> 4                          <NA>             Series R161AY003HI
#> 5                          <NA>             Series R161AY003HI
#> 6                         moist             Series R161AY003HI
#> 7                         moist             Series R161AY003HI
#> 8                         moist             Series R161AY003HI
#> 9                         moist             Series R161AY003HI
#> 10                         <NA>             Series R161AY003HI
#> 11                         <NA>             Series R161AY003HI
#> 12                         <NA>             Series R161AY003HI
#> 13                         <NA>             Series R161AY003HI
#> 14                         <NA>             Series R161AY003HI
#> 15                         <NA>             Series R161AY003HI
#> 16                         <NA>             Series R161AY003HI
#> 17                         <NA>             Series R161AY003HI
#> 18                         <NA>             Series R161AY003HI
#> 19                         <NA> Taxon above family R161AY003HI
#> 20                         <NA>             Series R161AY003HI
#> 21                         <NA>             Series R161AY003HI
#> 22                         <NA> Taxon above family R161AY009HI
#> 23           cindery substratum Taxon above family R161AY009HI
#> 24                loamy surface Taxon above family R161AY009HI
#> 25                         <NA>             Series R161AY009HI
#> 26                         <NA>             Series R161AY009HI
#> 27                         <NA>             Series R161AY009HI
#> 28                         <NA>             Series R161AY009HI
#> 29                         <NA>             Series R161AY009HI
#> 30                         <NA>             Series R161AY009HI
#> 31                         <NA>             Series R161AY009HI
#> 32                         <NA>             Series R161AY009HI
#> 33                       eroded             Series R161AY003HI
#> 34                       eroded             Series R161AY003HI
#> 35                         <NA>             Series R161AY003HI
#> 36                         <NA>             Series R161AY003HI
#> 37                         <NA>             Series R161AY003HI
#> 38                       eroded             Series R161AY003HI
#> 39                         <NA>             Series R161AY009HI
#> 40                         <NA>             Series R161AY008HI
#> 41                         <NA>             Series R161AY008HI
#> 42                         <NA> Taxon above family R161AY009HI
#> 43                         <NA>             Series R161AY009HI
#> 44                         <NA>             Series R157XY001HI
#> 45                         <NA>             Series R161AY003HI
#> 46                         <NA>             Series R161AY003HI
#> 47                         <NA>             Series R161AY003HI
#> 48                         <NA>             Series R161AY003HI
#> 49                         <NA>             Series R161AY003HI
#> 50                low elevation             Series R161AY003HI
#> 51                low elevation             Series R161AY003HI
#> 52                         <NA>             Series R161AY009HI
#> 53                         <NA> Taxon above family R161AY009HI
#> 54                         <NA>             Series R160XY007HI
#> 55                         <NA>             Series R161AY001HI
#> 56                         <NA>             Series R161AY003HI
#> 57                         <NA>             Series R161AY003HI
#> 58                         <NA>             Series R161AY003HI
#> 59                         <NA>             Series R161AY003HI
#> 60                         <NA>             Series R161AY003HI
#> 61                         <NA>             Series R161AY003HI
#> 62                         <NA>             Series R161AY003HI
#> 63                        moist             Series R161AY003HI
#> 64                         <NA>             Series R161AY003HI
#> 65                         <NA>             Series R161AY003HI
#> 66                         <NA>             Series R161AY003HI
#> 67                         <NA>             Series R161AY003HI
#> 68                        moist             Series R161AY003HI
#> 69                         <NA>             Series R161AY008HI
#> 70                         <NA>             Series R161AY008HI
#> 71                         <NA>             Series R161AY001HI
#> 72                         <NA>             Series R161AY001HI
#> 73                         <NA>             Series R161AY001HI
#> 74                         <NA>             Series R161AY001HI
#> 75                         <NA>             Series R161AY001HI
#> 76                         <NA>             Series R160XY007HI
#> 77                         <NA>             Series R160XY007HI
#> 78                         <NA>             Series R160XY007HI
#> 79                         <NA>             Series R160XY007HI
#> 80                         <NA>             Series R160XY007HI
#> 81                         <NA>             Series R160XY007HI
#> 82  medial very fine sandy loam             Series R160XY007HI
#> 83                         <NA>             Series R157XY004HI
#> 84                         <NA>             Series R160XY007HI
#> 85  medial very fine sandy loam             Series R160XY007HI
#> 86                         <NA>             Series R160XY007HI
#> 87                         <NA>             Series R160XY007HI
#> 88                         <NA>             Series R160XY007HI
#> 89                         <NA>             Series R161AY001HI
#> 90                         <NA>             Series R161AY001HI
#> 91                         <NA>             Series R161AY001HI
#> 92                         <NA>             Series R157XY003HI
#> 93                         <NA>             Series R157XY003HI
#> 94                         <NA>             Series R157XY003HI
#> 95                         <NA>             Series R157XY003HI
#> 96                         <NA>             Series R157XY003HI
#> 97                         <NA>             Series R157XY003HI
#> 98                         <NA>             Series R157XY003HI
#> 99                         <NA>             Series R157XY003HI
#> 100                        <NA>             Series R157XY003HI
#> 101                        <NA>             Series R157XY003HI
#> 102                        <NA>             Series R157XY003HI
#> 103                      eroded             Series R157XY003HI
#> 104                        <NA>             Series R160XY007HI
#> 105                        <NA>             Series R160XY007HI
#> 106 medial very fine sandy loam             Series R160XY007HI
#> 107 medial very fine sandy loam             Series R160XY007HI
#> 108 medial very fine sandy loam             Series R160XY007HI
#> 109 medial very fine sandy loam             Series R160XY007HI
#> 110                        <NA>             Series R161AY003HI
#> 111                        <NA>             Series R161AY003HI
#> 112                        <NA>             Series R161AY003HI
#> 113                        <NA>             Series R161AY003HI
#> 114                        <NA>             Series R161AY003HI
#> 115                        <NA>             Series R161AY003HI
#> 116                        <NA>             Series R161AY003HI
#> 117                        <NA>             Series R161AY003HI
#> 118                        <NA>             Series R161AY003HI
#> 119                        <NA>             Series R157XY003HI
#> 120                  very stony             Series R157XY003HI
#> 121                  very stony             Series R158XY401HI
#> 122                        <NA>             Series R158XY401HI
#> 123                        <NA>             Series R158XY401HI
#> 124                        <NA>             Series R158XY401HI
#> 125                        <NA>             Series R160XY007HI
#> 126                        <NA>             Series R160XY007HI
#> 127                        <NA>             Series R160XY007HI
#> 128             extremely stony             Series R160XY007HI
#> 129                        <NA>             Series R158XY401HI
#> 130                        <NA>             Series R158XY401HI
#> 131                        <NA>             Series R158XY401HI
#> 132                        <NA>             Series R158XY401HI
#> 133                        <NA>             Series R159AY403HI
#> 134                        <NA>             Series R159AY403HI
#> 135                        <NA>             Series R159AY403HI
#> 136      stony medial silt loam             Series R160XY007HI
#> 137      stony medial silt loam             Series R160XY007HI
#> 138                        <NA>             Series R159AY403HI
#> 139                        <NA>             Series R161AY003HI
#> 140                        <NA>             Series R161AY003HI
#> 141                        <NA>             Series R159AY403HI
#> 142                        <NA>             Series R161AY003HI
#> 143                        <NA>             Series R161AY003HI
#> 144                        <NA>             Series R159AY403HI
#> 145             ashy loamy sand             Series R161AY003HI
#> 146            medial silt loam             Series R160XY007HI
#> 147                        <NA>             Series R157XY001HI
#> 148                        <NA>             Series R157XY001HI
#> 149            extremely cobbly             Series R157XY001HI
#> 150                        <NA>             Series R157XY001HI
#> 151                        <NA>             Series R157XY001HI
#> 152                        <NA>             Series R157XY001HI
#> 153                        <NA>             Series R157XY001HI
#> 154                        <NA>             Series R157XY001HI
#> 155                        <NA>             Series R161AY003HI
#> 156                        <NA>             Series R161AY003HI
#> 157                        <NA> Taxon above family R157XY001HI
#> 158                        <NA>             Series R161AY003HI
#> 159                        <NA>             Series R161AY003HI
#> 160                        <NA> Taxon above family R161AY003HI
#> 161                        <NA>             Series R161AY003HI
#> 162                        <NA>             Series R161AY003HI
#> 163                        <NA>             Series R161AY003HI
#> 164                        <NA>             Series R161AY003HI
#> 165                        <NA>             Series R161AY003HI
#> 166                        <NA>             Series R161AY003HI
#> 167                        <NA>             Series R161AY003HI
#> 168                 medial loam             Series R161AY009HI
#> 169                        <NA>             Series R161AY009HI
#> 170                 medial loam             Series R161AY009HI
#> 171                        <NA> Taxon above family R161AY009HI
#> 172                        <NA>             Series R161AY009HI
#> 173                        <NA>             Series R161AY009HI
#> 174                        <NA>             Series R161AY009HI
#> 175                        <NA>             Series R161AY009HI
#> 176                        <NA>             Series R161AY003HI
#> 177                      eroded             Series R161AY003HI
#> 178                        <NA>             Series R161AY008HI
#> 179                        <NA>             Series R157XY001HI
#> 180                        <NA>             Series R161AY003HI
#> 181                        <NA>             Series R161AY003HI
#> 182                        <NA>             Series R161AY003HI
#> 183                        <NA>             Series R161AY003HI
#> 184                        <NA>             Series R161AY003HI
#> 185               low elevation             Series R161AY003HI
#> 186                        <NA>             Series R160XY007HI
#> 187                        <NA>             Series R160XY007HI
#> 188                cinder cones             Series R160XY007HI
#> 189                        <NA>             Series R160XY007HI
#> 190             extremely stony             Series R160XY007HI
#> 191 cobbly ashy loamy fine sand             Series R161AY002HI
#> 192              dry substratum             Series R157XY003HI
#> 193 cobbly ashy loamy fine sand             Series R161AY002HI
#> 194 cobbly ashy loamy fine sand             Series R161AY002HI
#> 195 cobbly ashy loamy fine sand             Series R161AY002HI
#> 196                        cool             Series R161AY002HI
#> 197                        cool             Series R161AY002HI
#> 198                        cool             Series R161AY002HI
#> 199                        <NA>             Series R161AY002HI
#> 200                        <NA>             Series R161AY002HI
#> 201                        <NA>             Series R159AY003HI
#> 202                        <NA>             Series R159AY003HI
#> 203                        <NA>             Series R159AY003HI
#> 204                        <NA>             Series R159AY009HI
#> 205                        <NA>             Series R159AY009HI
#> 206                        <NA>             Series R159AY003HI
#> 207                        <NA>             Series R159AY003HI
#> 208                        <NA>             Series R159AY003HI
#> 209                        <NA>             Series R159AY003HI
#> 210                        <NA>             Series R159AY003HI
#> 211                        <NA>             Series R159AY403HI
#> 212             older substrate             Series R159AY003HI
#> 213                        <NA>             Series R159AY009HI
#> 214                        <NA>             Series R158XY401HI
#> 215             severely eroded             Series R158XY401HI
#> 216      stony, severely eroded             Series R158XY002HI
#> 217                       stony             Series R158XY401HI
#> 218             silty clay loam             Series R158XY401HI
#> 219  very stony silty clay loam             Series R158XY401HI
#> 220                        <NA>             Series R165XY001HI
#> 221                        <NA>             Series R165XY001HI
#> 222                        <NA>             Series R158XY004HI
#> 223                        <NA>             Series R158XY004HI
#> 224                        <NA>             Series R158XY004HI
#> 225                        <NA>             Series R158XY004HI
#> 226                        <NA>             Series R158XY004HI
#> 227                        <NA>             Series R158XY002HI
#> 228                        <NA>             Series R158XY004HI
#> 229                        <NA>             Series R158XY401HI
#> 230                        <NA>             Series R158XY401HI
#> 231                        <NA>             Series R158XY401HI
#> 232                        <NA>             Series R158XY401HI
#> 233                        <NA>             Series R163XY002HI
#> 234                        <NA>             Series R163XY002HI
#> 235                        <NA>             Series R158XY401HI
#> 236                        <NA>             Series R158XY401HI
#> 237                        <NA>             Series R158XY401HI
#> 238             severely eroded             Series R158XY401HI
#> 239                        <NA>             Series R158XY401HI
#> 240                        <NA>             Series R158XY401HI
#> 241                        <NA>             Series R163XY001HI
#> 242                        <NA>             Series R158XY401HI
#> 243                       moist             Series R158XY401HI
#> 244                       moist             Series R158XY401HI
#> 245                        <NA>             Series R158XY005HI
#> 246                        <NA>             Series R166XY003HI
#> 247            deep water table             Series R166XY003HI
#> 248                       moist             Series R166XY002HI
#> 249                        <NA>             Series R158XY401HI
#> 250                        <NA>             Series R158XY401HI
#> 251                        <NA>             Series R158XY401HI
#> 252                        <NA>             Series R158XY401HI
#> 253                        <NA>             Series R158XY401HI
#> 254                        <NA>             Series R158XY401HI
#> 255                        <NA>             Series R163XY001HI
#> 256                        <NA>             Series R158XY002HI
#> 257                        <NA>             Series R166XY001HI
#> 258                        <NA>             Series R166XY001HI
#> 259                        <NA>             Series R158XY002HI
#> 260                        <NA>             Series R158XY002HI
#> 261                        <NA>             Series R158XY002HI
#> 262                        <NA>             Series R158XY002HI
#> 263                        <NA>             Series R158XY002HI
#> 264                        <NA>             Series R165XY001HI
#> 265                        <NA>             Series R158XY005HI
#> 266             severely eroded             Series R158XY005HI
#> 267                        <NA>             Series R158XY005HI
#> 268                        <NA>             Series R158XY005HI
#> 269                        <NA>             Series R158XY005HI
#> 270     medium textured subsoil             Series R158XY005HI
#> 271                        <NA>             Series R158XY005HI
#> 272                        <NA>             Series R165XY001HI
#> 273                        <NA>             Series R158XY005HI
#> 274                        <NA>             Series R165XY001HI
#> 275                        <NA>             Series R158XY005HI
#> 276                        <NA>             Series R158XY002HI
#> 277                      eroded             Series R158XY002HI
#> 278               stony, eroded             Series R158XY002HI
#> 279                       stony             Series R158XY002HI
#> 280                        <NA>             Series R158XY002HI
#> 281                        <NA>             Series R158XY002HI
#> 282                        <NA>             Series R158XY401HI
#> 283                        <NA>             Series R158XY401HI
#> 284                        <NA>             Series R158XY401HI
#> 285                        <NA>             Series R158XY002HI
#> 286                        <NA>             Series R158XY002HI
#> 287                        <NA>             Series R158XY002HI
#> 288                        <NA>             Series R158XY002HI
#> 289                        <NA>             Series R165XY001HI
#> 290                        <NA>             Series R167XY001HI
#> 291                        <NA>             Series R167XY001HI
#> 292                        <NA>             Series R167XY001HI
#> 293                    gravelly             Series R167XY001HI
#> 294                        <NA>             Series R167XY002HI
#> 295                        <NA>             Series R158XY002HI
#> 296                        <NA>             Series R167XY001HI
#> 297                        <NA>             Series R167XY002HI
#> 298                       peaty             Series R167XY002HI
#> 299                        <NA>             Series R167XY002HI
#> 300                        <NA>             Series R167XY001HI
#> 301                        <NA>             Series R167XY001HI
#> 302                        <NA>             Series R167XY001HI
#> 303                        <NA>             Series R167XY001HI
#> 304                        <NA>             Series R167XY001HI
#> 305                        <NA>             Series R167XY001HI
#> 306                        <NA>             Series R158XY401HI
#> 307                        <NA>             Series R158XY401HI
#> 308                        <NA>             Series R158XY401HI
#> 309                        <NA>             Series R158XY401HI
#> 310                        <NA>             Series R163XY002HI
#> 311                        <NA>            Variant R163XY002HI
#> 312                        <NA>            Variant R163XY001HI
#> 313                        <NA>            Variant R163XY001HI
#> 314                        <NA>             Series R167XY001HI
#> 315                        <NA>             Series R167XY001HI
#> 316                        <NA>             Series R167XY001HI
#> 317                        <NA>             Series R167XY002HI
#> 318                        <NA>             Series R158XY401HI
#> 319                        <NA>             Series R163XY005HI
#> 320                        <NA>             Series R163XY005HI
#> 321                        <NA>             Series R163XY005HI
#> 322                     drained             Series R163XY005HI
#> 323                        <NA>             Series R167XY001HI
#> 324                        <NA>             Series R167XY001HI
#> 325                        <NA>             Series R167XY001HI
#> 326                        <NA>             Series R167XY001HI
#> 327                        <NA>             Series R163XY004HI
#> 328                        <NA>             Series R163XY004HI
#> 329                        <NA>             Series R163XY004HI
#> 330             extremely stony             Series R163XY004HI
#> 331                        <NA>             Series R167XY002HI
#> 332                       stony             Series R158XY401HI
#> 333                       stony             Series R158XY401HI
#> 334                       stony             Series R158XY401HI
#> 335                        <NA>             Series R164XY401HI
#> 336                        <NA>             Series R167XY002HI
#> 337                        <NA>             Series R167XY001HI
#> 338                        <NA>             Series R167XY001HI
#> 339                        <NA>             Series R167XY001HI
#> 340                        <NA>             Series R158XY401HI
#> 341                        <NA>             Series R158XY401HI
#> 342                        <NA>             Series R158XY401HI
#> 343                        <NA>             Series R158XY401HI
#> 344                    gravelly             Series R158XY401HI
#> 345                    gravelly             Series R158XY401HI
#> 346     extremely cobbly, moist             Series R163XY001HI
#> 347                        <NA>             Series R163XY001HI
#> 348                        <NA>             Series R158XY002HI
#> 349                        <NA>             Series R163XY001HI
#> 350                        <NA>             Series R165XY001HI
#> 351                        <NA>             Series R165XY001HI
#> 352                        <NA>             Series R165XY001HI
#> 353                        <NA>             Series R165XY001HI
#> 354                        <NA>             Series R165XY001HI
#> 355                        <NA>             Series R167XY001HI
#> 356                        <NA>             Series R167XY001HI
#> 357                        <NA>             Series R167XY001HI
#> 358                        <NA>             Series R167XY001HI
#> 359                        <NA>             Series R158XY002HI
#> 360                        <NA>             Series R158XY002HI
#> 361                        <NA>             Series R158XY002HI
#> 362                        <NA>             Series R158XY002HI
#> 363                        <NA>             Series R158XY002HI
#> 364                        <NA>             Series R158XY002HI
#> 365                        <NA>             Series R158XY002HI
#> 366                       stony             Series R158XY002HI
#> 367                        <NA>             Series R163XY004HI
#> 368                        <NA>             Series R158XY401HI
#> 369              poorly drained         Taxadjunct R163XY005HI
#> 370                        <NA>             Series R158XY002HI
#> 371                        <NA>             Series R158XY002HI
#> 372                      eroded             Series R158XY002HI
#> 373                      eroded             Series R158XY002HI
#> 374                        <NA>             Series R163XY005HI
#> 375                        <NA>             Series R163XY001HI
#> 376                       stony             Series R163XY001HI
#> 377                        <NA>             Series R165XY001HI
#> 378                        <NA>             Series R165XY001HI
#> 379                        <NA>             Series R158XY005HI
#> 380                        <NA>             Series R165XY001HI
#> 381                        <NA>             Series R158XY005HI
#> 382                        <NA>             Series R158XY002HI
#> 383                        <NA>             Series R158XY002HI
#> 384             extremely stony             Series R158XY004HI
#> 385                       moist             Series R158XY401HI
#> 386                        <NA>             Series R167XY001HI
#> 387                        <NA>             Series R167XY001HI
#> 388                        <NA>             Series R167XY001HI
#> 389                        <NA>             Series R167XY001HI
#> 390                        <NA>             Series R167XY001HI
#> 391                        <NA>             Series R167XY001HI
#> 392                        <NA>             Series R158XY005HI
#> 393                        <NA>             Series R158XY005HI
#> 394                        <NA>             Series R158XY005HI
#> 395                        <NA>             Series R158XY005HI
#> 396                        <NA>             Series R158XY005HI
#> 397                        <NA>             Series R165XY001HI
#> 398                        <NA>             Series R165XY001HI
#> 399                        <NA>             Series R165XY001HI
#> 400                        <NA>             Series R158XY004HI
#> 401                        <NA>             Series R158XY401HI
#> 402                  very rocky             Series R158XY401HI
#> 403                        <NA>             Series R158XY401HI
#> 404                        <NA>             Series R163XY004HI
#> 405                        <NA>             Series R163XY002HI
#> 406                         dry             Series R158XY401HI
#> 407                         dry             Series R158XY401HI
#> 408        severely eroded, dry             Series R158XY401HI
#> 409                        <NA>             Series R166XY002HI
#> 410                      eroded             Series R166XY002HI
#> 411                        <NA>             Series R166XY002HI
#> 412                      eroded             Series R166XY002HI
#> 413                        <NA>             Series R166XY002HI
#> 414                        <NA>             Series R166XY002HI
#> 415                        <NA>             Series R166XY002HI
#> 416                        <NA>             Series R166XY002HI
#> 417                        <NA>             Series R158XY401HI
#> 418                        <NA>             Series R158XY401HI
#> 419              severly eroded             Series R158XY401HI
#> 420                        <NA>             Series R158XY401HI
#> 421                        <NA>             Series R158XY401HI
#> 422                        <NA>             Series R158XY401HI
#> 423                        <NA>             Series R158XY401HI
#> 424                        <NA>             Series R163XY001HI
#> 425                        <NA>             Series R158XY002HI
#> 426                        <NA>             Series R166XY001HI
#> 427                        <NA>             Series R166XY001HI
#> 428                        <NA>             Series R158XY002HI
#> 429                        <NA>             Series R158XY002HI
#> 430                        <NA>             Series R158XY002HI
#> 431                        <NA>             Series R158XY002HI
#> 432                        <NA>             Series R165XY001HI
#> 433                        <NA>             Series R158XY005HI
#> 434             severely eroded             Series R158XY005HI
#> 435                        <NA>             Series R158XY005HI
#> 436                        <NA>             Series R158XY002HI
#> 437                      eroded             Series R158XY002HI
#> 438                       stony             Series R158XY002HI
#> 439                        <NA>             Series R158XY002HI
#> 440                        <NA>             Series R158XY002HI
#> 441                        <NA>             Series R158XY002HI
#> 442                        <NA>             Series R158XY002HI
#> 443             severely eroded             Series R158XY002HI
#> 444                        <NA>             Series R158XY401HI
#> 445                        <NA>             Series R158XY401HI
#> 446                        <NA>             Series R158XY401HI
#> 447                        <NA>             Series R158XY401HI
#> 448                    gravelly             Series R158XY401HI
#> 449                        <NA>             Series R158XY002HI
#> 450                        <NA>             Series R158XY002HI
#> 451                        <NA>             Series R158XY004HI
#> 452                      cobbly             Series R158XY004HI
#> 453                      cobbly             Series R158XY004HI
#> 454                        <NA>             Series R158XY401HI
#> 455                        <NA>             Series R158XY401HI
#> 456                        <NA>             Series R158XY401HI
#> 457                        <NA>             Series R158XY002HI
#> 458                      cobbly             Series R158XY002HI
#> 459                      cobbly             Series R158XY002HI
#> 460                        <NA>             Series R158XY002HI
#> 461                        <NA>             Series R158XY002HI
#> 462                      cobbly             Series R158XY002HI
#> 463                        <NA>             Series R167XY001HI
#> 464                        <NA>             Series R167XY001HI
#> 465                        <NA>             Series R167XY001HI
#> 466                        <NA>             Series R167XY001HI
#> 467                        <NA>             Series R158XY401HI
#> 468                        <NA>             Series R158XY401HI
#> 469                        <NA>             Series R158XY401HI
#> 470                        <NA>             Series R158XY401HI
#> 471                        <NA>             Series R158XY401HI
#> 472                        <NA>             Series R158XY401HI
#> 473                        <NA>             Series R165XY001HI
#> 474                    gravelly             Series R158XY401HI
#> 475                  very stony             Series R159AY003HI
#> 476             extremely stony             Series R159AY003HI
#> 477                        <NA>            Variant R159AY003HI
#> 478             extremely stony            Variant R159AY003HI
#> 479                        <NA>             Series R158XY401HI
#> 480                        <NA>             Series R158XY401HI
#> 481                        <NA>             Series R158XY401HI
#> 482                        <NA>             Series R158XY005HI
#> 483                        <NA>             Series R158XY005HI
#> 484                        <NA>             Series R163XY003HI
#> 485                        <NA>             Series R163XY003HI
#> 486                      cobbly             Series R163XY003HI
#> 487                      cobbly             Series R163XY003HI
#> 488                        <NA>             Series R163XY003HI
#> 489                        <NA>             Series R163XY003HI
#> 490                        <NA>             Series R160XY007HI
#> 491                        <NA>             Series R163XY002HI
#> 492                      saline             Series R163XY002HI
#> 493                        <NA>             Series R158XY401HI
#> 494                        <NA>             Series R158XY401HI
#> 495                        <NA>             Series R158XY401HI
#> 496                        <NA>             Series R167XY001HI
#> 497              etremely stony             Series R157XY008HI
#> 498                  very stony             Series R157XY008HI
#> 499             extremely stony             Series R157XY008HI
#> 500                        <NA>             Series R159AY002HI
#> 501  extremely stony silty clay             Series R159AY002HI
#> 502                        <NA>             Series R166XY003HI
#> 503            deep water table             Series R166XY003HI
#> 504                      cobbly             Series R158XY002HI
#> 505                      cobbly             Series R158XY002HI
#> 506                      cobbly             Series R158XY002HI
#> 507                        <NA>             Series R158XY002HI
#> 508                  very stony             Series R158XY004HI
#> 509                        <NA>             Series R158XY002HI
#> 510                        <NA>             Series R158XY002HI
#> 511                      cobbly             Series R158XY002HI
#> 512                       stony             Series R158XY002HI
#> 513             extremely stony             Series R157XY003HI
#> 514                        <NA>             Series R158XY401HI
#> 515                      cobbly             Series R160XY007HI
#> 516                      cobbly             Series R160XY007HI
#> 517                        <NA>             Series R160XY007HI
#> 518                        <NA>             Series R160XY007HI
#> 519                        <NA>             Series R158XY401HI
#> 520                        <NA>             Series R158XY401HI
#> 521                        <NA>             Series R158XY401HI
#> 522                        <NA>             Series R167XY001HI
#> 523                        <NA>             Series R167XY001HI
#> 524                        <NA>             Series R167XY001HI
#> 525                        <NA>             Series R167XY001HI
#> 526                        <NA>             Series R159AY403HI
#> 527             extremely stony             Series R159AY403HI
#> 528                        <NA>             Series R158XY002HI
#> 529                        <NA>             Series R158XY002HI
#> 530                        <NA>             Series R158XY002HI
#> 531                        <NA>             Series R159AY403HI
#> 532             extremely stony             Series R157XY003HI
#> 533                        <NA>             Series R157XY003HI
#> 534                  very stony             Series R157XY003HI
#> 535             extremely stony             Series R159AY003HI
#> 536                        <NA>             Series R158XY005HI
#> 537             severely eroded             Series R158XY005HI
#> 538                        <NA>             Series R158XY005HI
#> 539                  very stony             Series R157XY003HI
#> 540             extremely stony             Series R157XY003HI
#> 541                        <NA>             Series R158XY005HI
#> 542                        <NA>             Series R165XY001HI
#> 543                        <NA>             Series R160XY007HI
#> 544                        <NA>             Series R160XY007HI
#> 545                        <NA>             Series R158XY401HI
#> 546                        <NA>             Series R158XY401HI
#> 547                        <NA>             Series R158XY401HI
#> 548                        <NA>             Series R167XY001HI
#> 549                        <NA>             Series R167XY001HI
#> 550                        <NA>             Series R167XY001HI
#> 551                        <NA>             Series R167XY001HI
#> 552                        <NA>             Series R167XY001HI
#> 553                        <NA>             Series R158XY002HI
#> 554                        <NA>             Series R158XY002HI
#> 555                        <NA>             Series R158XY002HI
#> 556                        <NA>             Series R158XY002HI
#> 557                        <NA>             Series R158XY002HI
#> 558                        <NA>             Series R158XY002HI
#> 559                        <NA>             Series R158XY002HI
#> 560                        <NA>             Series R160XY007HI
#> 561                        <NA>             Series R163XY003HI
#> 562                        <NA>             Series R163XY002HI
#> 563                        <NA>             Series R163XY002HI
#> 564                  very stony             Series R157XY008HI
#> 565                        <NA>             Series R160XY007HI
#> 566                       rocky             Series R160XY007HI
#> 567                        <NA>             Series R158XY002HI
#> 568                       stony             Series R158XY002HI
#> 569                       stony             Series R158XY002HI
#> 570                  very stony             Series R158XY004HI
#> 571                        <NA>             Series R158XY002HI
#> 572                        <NA>             Series R158XY002HI
#> 573                      cobbly             Series R158XY002HI
#> 574                        <NA>             Series R158XY004HI
#> 575                        <NA>             Series R158XY004HI
#> 576             extremely stony             Series R158XY004HI
#> 577             extremely stony             Series R158XY004HI
#> 578     extremely stony, eroded             Series R158XY004HI
#> 579                        <NA>             Series R163XY003HI
#> 580                        <NA>             Series R163XY003HI
#> 581                        <NA>             Series R163XY003HI
#> 582                        <NA>             Series R163XY003HI
#> 583                      cobbly             Series R163XY003HI
#> 584                  very stony             Series R158XY004HI
#> 585                  very stony             Series R158XY004HI
#> 586             extremely stony             Series R158XY004HI
#> 587             extremely stony             Series R158XY004HI
#> 588             older substrate             Series R158XY002HI
#> 589                        <NA>             Series R158XY401HI
#> 590                        <NA>             Series R163XY004HI
#> 591                        <NA>             Series R158XY002HI
#> 592                        <NA>             Series R158XY401HI
#> 593                        <NA>             Series R158XY002HI
#> 594                        <NA>             Series R158XY002HI
#> 595                        <NA>             Series R158XY002HI
#> 596                        <NA>             Series R158XY002HI
#> 597                        <NA>             Series R158XY401HI
#> 598                        <NA>             Series R158XY002HI
#> 599                        <NA>             Series R158XY401HI
#> 600                        <NA>             Series R158XY401HI
#> 601                        <NA>             Series R158XY401HI
#> 602                        <NA>             Series R158XY401HI
#> 603                        <NA>             Series R167XY002HI
#> 604                        <NA>             Series R167XY002HI
#> 605                        <NA>             Series R167XY002HI
#> 606                        <NA>             Series R163XY001HI
#> 607                        <NA>             Series R163XY001HI
#> 608                        <NA>             Series R163XY002HI
#> 609                      saline             Series R163XY002HI
#> 610                        <NA>             Series R163XY001HI
#> 611                        <NA>             Series R163XY001HI
#> 612                        <NA>             Series R163XY001HI
#> 613                        <NA>             Series R163XY001HI
#> 614                        <NA>             Series R163XY001HI
#> 615                        <NA>             Series R163XY001HI
#> 616                        <NA>             Series R163XY005HI
#> 617                     drained             Series R163XY005HI
#> 618                        <NA>             Series R158XY401HI
#> 619               noncalcareous         Taxadjunct R163XY005HI
#> 620                        <NA>             Series R167XY001HI
#> 621                        <NA>             Series R167XY001HI
#> 622                        <NA>             Series R167XY001HI
#> 623                        <NA>             Series R167XY001HI
#> 624                        <NA>             Series R167XY001HI
#> 625                        <NA>             Series R167XY001HI
#> 626                        <NA>             Series R167XY001HI
#> 627                        <NA>             Series R158XY401HI
#> 628                        <NA>             Series R158XY401HI
#> 629                        <NA>             Series R158XY401HI
#> 630                        <NA>             Series R158XY401HI
#> 631                        <NA>             Series R158XY401HI
#> 632                        <NA>             Series R158XY401HI
#> 633                        <NA>             Series R163XY005HI
#> 634                        <NA>             Series R163XY005HI
#> 635                        <NA>             Series R163XY005HI
#> 636                        <NA>             Series R163XY005HI
#> 637                        <NA>             Series R158XY401HI
#> 638                        <NA>             Series R158XY401HI
#> 639                        <NA>             Series R158XY401HI
#> 640                        <NA>             Series R158XY401HI
#> 641                        <NA>             Series R158XY401HI
#> 642                        <NA>             Series R158XY401HI
#> 643                        <NA>             Series R166XY001HI
#> 644                        <NA>             Series R166XY001HI
#> 645                        <NA>             Series R166XY001HI
#> 646                        <NA>             Series R158XY401HI
#> 647                        <NA>             Series R158XY401HI
#> 648                        <NA>             Series R158XY401HI
#> 649                        <NA>             Series R158XY401HI
#> 650                        <NA>             Series R158XY401HI
#> 651                        <NA>             Series R158XY401HI
#> 652                        <NA>             Series R158XY401HI
#> 653                        <NA>             Series R158XY401HI
#> 654                        <NA>             Series R158XY401HI
#> 655                       moist             Series R158XY401HI
#> 656                        <NA>             Series R158XY401HI
#> 657                        <NA>             Series R158XY401HI
#> 658                        <NA>             Series R158XY005HI
#> 659                        <NA>             Series R158XY005HI
#> 660                        <NA>             Series R167XY002HI
#> 661                        <NA>             Series R167XY001HI
#> 662                        <NA>             Series R167XY001HI
#> 663                        <NA>             Series R167XY001HI
#> 664                        <NA>             Series R167XY001HI
#> 665                        <NA>             Series R167XY001HI
#> 666                        <NA>             Series R167XY001HI
#> 667            extremely cobbly             Series R163XY001HI
#> 668                        <NA>             Series R163XY001HI
#> 669                        <NA>             Series R158XY002HI
#> 670                        <NA>             Series R163XY001HI
#> 671                       stony             Series R163XY001HI
#> 672                       stony             Series R163XY001HI
#> 673                        <NA>             Series R158XY401HI
#> 674                        <NA>             Series R158XY401HI
#> 675                        <NA>             Series R158XY401HI
#> 676                        <NA>             Series R158XY401HI
#> 677                        <NA>             Series R163XY004HI
#> 678                        <NA>             Series R163XY001HI
#> 679                        <NA>             Series R163XY001HI
#> 680                        <NA>             Series R163XY001HI
#> 681                        <NA>             Series R158XY401HI
#> 682                       stony             Series R158XY401HI
#> 683                        <NA>             Series R163XY004HI
#> 684                        <NA>             Series R158XY401HI
#> 685                        <NA>             Series R158XY401HI
#> 686                        <NA>             Series R158XY401HI
#> 687                        <NA>             Series R158XY401HI
#> 688                        <NA>             Series R158XY401HI
#> 689                        <NA>             Series R158XY401HI
#> 690                        <NA>             Series R158XY401HI
#> 691                        <NA>             Series R158XY401HI
#> 692                        <NA>             Series R158XY401HI
#> 693                        <NA>             Series R158XY002HI
#> 694                        <NA>             Series R163XY004HI
#> 695                        <NA>             Series R158XY002HI
#> 696                        <NA>             Series R158XY002HI
#> 697                        <NA>             Series R158XY002HI
#> 698                        <NA>             Series R158XY002HI
#> 699                        <NA>             Series R158XY401HI
#> 700                        <NA>             Series R158XY005HI
#> 701                        <NA>             Series R158XY005HI
#> 702                        <NA>             Series R158XY005HI
#> 703                        <NA>             Series R167XY001HI
#> 704                        <NA>             Series R167XY001HI
#> 705                        <NA>             Series R167XY001HI
#> 706                        <NA>             Series R167XY001HI
#> 707                        <NA>             Series R167XY001HI
#> 708                        <NA>             Series R163XY005HI
#> 709                        <NA>             Series R158XY002HI
#> 710                        <NA>             Series R158XY401HI
#> 711                        <NA>             Series R158XY401HI
#> 712                        <NA>             Series R158XY002HI
#> 713                        <NA>             Series R158XY401HI
#> 714                        <NA>             Series R158XY002HI
#> 715                       stony             Series R158XY002HI
#> 716                  very stony             Series R158XY002HI
#> 717                        <NA>             Series R158XY401HI
#> 718                        <NA>             Series R158XY401HI
#> 719                        <NA>             Series R158XY401HI
#> 720                        <NA>             Series R167XY001HI
#> 721                        <NA>             Series R167XY001HI
#> 722                        <NA>             Series R167XY001HI
#> 723                        <NA>             Series R158XY401HI
#> 724                        <NA>             Series R158XY401HI
#> 725                        <NA>             Series R158XY401HI
#> 726                        <NA>             Series R158XY401HI
#> 727                        <NA>             Series R158XY401HI
#> 728                        <NA>             Series R158XY401HI
#> 729                        <NA>             Series R158XY401HI
#> 730                        <NA>             Series R158XY401HI
#> 731                        <NA>             Series R158XY401HI
#> 732                       stony             Series R158XY401HI
#> 733                  very stony             Series R158XY401HI
#> 734                        <NA>             Series R158XY401HI
#> 735                       stony             Series R167XY001HI
#> 736                        <NA>             Series R167XY001HI
#> 737                        <NA>             Series R167XY001HI
#> 738                        <NA>             Series R167XY001HI
#> 739                        <NA>             Series R167XY001HI
#> 740                        <NA>             Series R167XY001HI
#> 741                        <NA>             Series R158XY401HI
#> 742                        <NA>             Series R167XY001HI
#> 743                        <NA>             Series R167XY001HI
#> 744                        <NA>             Series R163XY004HI
#> 745                        <NA>             Series R163XY004HI
#> 746                        <NA>             Series R163XY004HI
#>                                                                                                                                                          ecoclassname
#> 1                                                                                                                         Isohyperthermic Ustic Naturalized Grassland
#> 2                                                                                                                                                    Isomesic Savanna
#> 3                                                                                                                                                    Isomesic Savanna
#> 4                                                                                                                                                    Isomesic Savanna
#> 5                                                                                                                                                    Isomesic Savanna
#> 6                                                                                                                                                    Isomesic Savanna
#> 7                                                                                                                                                    Isomesic Savanna
#> 8                                                                                                                                                    Isomesic Savanna
#> 9                                                                                                                                                    Isomesic Savanna
#> 10                                                                                                                                                   Isomesic Savanna
#> 11                                                                                                                                                   Isomesic Savanna
#> 12                                                                                                                                                   Isomesic Savanna
#> 13                                                                                                                                                   Isomesic Savanna
#> 14                                                                                                                                                   Isomesic Savanna
#> 15                                                                                                                                                   Isomesic Savanna
#> 16                                                                                                                                                   Isomesic Savanna
#> 17                                                                                                                                                   Isomesic Savanna
#> 18                                                                                                                                                   Isomesic Savanna
#> 19                                                                                                                                                   Isomesic Savanna
#> 20                                                                                                                                                   Isomesic Savanna
#> 21                                                                                                                                                   Isomesic Savanna
#> 22                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 23                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 24                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 25                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 26                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 27                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 28                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 29                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 30                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 31                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 32                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 33                                                                                                                                                   Isomesic Savanna
#> 34                                                                                                                                                   Isomesic Savanna
#> 35                                                                                                                                                   Isomesic Savanna
#> 36                                                                                                                                                   Isomesic Savanna
#> 37                                                                                                                                                   Isomesic Savanna
#> 38                                                                                                                                                   Isomesic Savanna
#> 39                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 40                                                                                                                        Isohyperthermic Ustic Naturalized Grassland
#> 41                                                                                                                        Isohyperthermic Ustic Naturalized Grassland
#> 42                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 43                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 44                                                                                                                                       Torric Naturalized Grassland
#> 45                                                                                                                                                   Isomesic Savanna
#> 46                                                                                                                                                   Isomesic Savanna
#> 47                                                                                                                                                   Isomesic Savanna
#> 48                                                                                                                                                   Isomesic Savanna
#> 49                                                                                                                                                   Isomesic Savanna
#> 50                                                                                                                                                   Isomesic Savanna
#> 51                                                                                                                                                   Isomesic Savanna
#> 52                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 53                                                                                                                             Isothermic Ustic Naturalized Grassland
#> 54                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 55                                                                                                                                             Isohyperthermic Desert
#> 56                                                                                                                                                   Isomesic Savanna
#> 57                                                                                                                                                   Isomesic Savanna
#> 58                                                                                                                                                   Isomesic Savanna
#> 59                                                                                                                                                   Isomesic Savanna
#> 60                                                                                                                                                   Isomesic Savanna
#> 61                                                                                                                                                   Isomesic Savanna
#> 62                                                                                                                                                   Isomesic Savanna
#> 63                                                                                                                                                   Isomesic Savanna
#> 64                                                                                                                                                   Isomesic Savanna
#> 65                                                                                                                                                   Isomesic Savanna
#> 66                                                                                                                                                   Isomesic Savanna
#> 67                                                                                                                                                   Isomesic Savanna
#> 68                                                                                                                                                   Isomesic Savanna
#> 69                                                                                                                        Isohyperthermic Ustic Naturalized Grassland
#> 70                                                                                                                        Isohyperthermic Ustic Naturalized Grassland
#> 71                                                                                                                                             Isohyperthermic Desert
#> 72                                                                                                                                             Isohyperthermic Desert
#> 73                                                                                                                                             Isohyperthermic Desert
#> 74                                                                                                                                             Isohyperthermic Desert
#> 75                                                                                                                                             Isohyperthermic Desert
#> 76                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 77                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 78                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 79                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 80                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 81                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 82                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 83                                                                                                                         Alluvial Woodland (Kiawe/Prosopis pallida)
#> 84                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 85                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 86                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 87                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 88                                                                                                               Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 89                                                                                                                                             Isohyperthermic Desert
#> 90                                                                                                                                             Isohyperthermic Desert
#> 91                                                                                                                                             Isohyperthermic Desert
#> 92                                                                                 Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 93                                                                                 Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 94                                                                                 Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 95                                                                                 Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 96                                                                                 Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 97                                                                                 Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 98                                                                                 Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 99                                                                                 Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 100                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 101                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 102                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 103                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 104                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 105                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 106                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 107                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 108                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 109                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 110                                                                                                                                                  Isomesic Savanna
#> 111                                                                                                                                                  Isomesic Savanna
#> 112                                                                                                                                                  Isomesic Savanna
#> 113                                                                                                                                                  Isomesic Savanna
#> 114                                                                                                                                                  Isomesic Savanna
#> 115                                                                                                                                                  Isomesic Savanna
#> 116                                                                                                                                                  Isomesic Savanna
#> 117                                                                                                                                                  Isomesic Savanna
#> 118                                                                                                                                                  Isomesic Savanna
#> 119                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 120                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 121                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 122                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 123                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 124                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 125                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 126                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 127                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 128                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 129                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 130                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 131                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 132                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 133                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 134                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 135                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 136                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 137                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 138                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 139                                                                                                                                                  Isomesic Savanna
#> 140                                                                                                                                                  Isomesic Savanna
#> 141                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 142                                                                                                                                                  Isomesic Savanna
#> 143                                                                                                                                                  Isomesic Savanna
#> 144                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 145                                                                                                                                                  Isomesic Savanna
#> 146                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 147                                                                                                                                      Torric Naturalized Grassland
#> 148                                                                                                                                      Torric Naturalized Grassland
#> 149                                                                                                                                      Torric Naturalized Grassland
#> 150                                                                                                                                      Torric Naturalized Grassland
#> 151                                                                                                                                      Torric Naturalized Grassland
#> 152                                                                                                                                      Torric Naturalized Grassland
#> 153                                                                                                                                      Torric Naturalized Grassland
#> 154                                                                                                                                      Torric Naturalized Grassland
#> 155                                                                                                                                                  Isomesic Savanna
#> 156                                                                                                                                                  Isomesic Savanna
#> 157                                                                                                                                      Torric Naturalized Grassland
#> 158                                                                                                                                                  Isomesic Savanna
#> 159                                                                                                                                                  Isomesic Savanna
#> 160                                                                                                                                                  Isomesic Savanna
#> 161                                                                                                                                                  Isomesic Savanna
#> 162                                                                                                                                                  Isomesic Savanna
#> 163                                                                                                                                                  Isomesic Savanna
#> 164                                                                                                                                                  Isomesic Savanna
#> 165                                                                                                                                                  Isomesic Savanna
#> 166                                                                                                                                                  Isomesic Savanna
#> 167                                                                                                                                                  Isomesic Savanna
#> 168                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 169                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 170                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 171                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 172                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 173                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 174                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 175                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 176                                                                                                                                                  Isomesic Savanna
#> 177                                                                                                                                                  Isomesic Savanna
#> 178                                                                                                                       Isohyperthermic Ustic Naturalized Grassland
#> 179                                                                                                                                      Torric Naturalized Grassland
#> 180                                                                                                                                                  Isomesic Savanna
#> 181                                                                                                                                                  Isomesic Savanna
#> 182                                                                                                                                                  Isomesic Savanna
#> 183                                                                                                                                                  Isomesic Savanna
#> 184                                                                                                                                                  Isomesic Savanna
#> 185                                                                                                                                                  Isomesic Savanna
#> 186                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 187                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 188                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 189                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 190                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 191                                                                                                                                    High Elevation Isomesic Desert
#> 192                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 193                                                                                                                                    High Elevation Isomesic Desert
#> 194                                                                                                                                    High Elevation Isomesic Desert
#> 195                                                                                                                                    High Elevation Isomesic Desert
#> 196                                                                                                                                    High Elevation Isomesic Desert
#> 197                                                                                                                                    High Elevation Isomesic Desert
#> 198                                                                                                                                    High Elevation Isomesic Desert
#> 199                                                                                                                                    High Elevation Isomesic Desert
#> 200                                                                                                                                    High Elevation Isomesic Desert
#> 201                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 202                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 203                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 204                                                                                                                                              Isothermic Aquic Bog
#> 205                                                                                                                                              Isothermic Aquic Bog
#> 206                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 207                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 208                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 209                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 210                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 211                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 212                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 213                                                                                                                                              Isothermic Aquic Bog
#> 214                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 215                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 216                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 217                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 218                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 219                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 220                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 221                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 222                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 223                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 224                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 225                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 226                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 227                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 228                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 229                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 230                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 231                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 232                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 233                                                                                                                                                   Sandy Shrubland
#> 234                                                                                                                                                   Sandy Shrubland
#> 235                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 236                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 237                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 238                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 239                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 240                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 241                                                                                                                                                 Shrink-Swell Clay
#> 242                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 243                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 244                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 245                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 246                                                                                                                                               Aquisalids Herbland
#> 247                                                                                                                                               Aquisalids Herbland
#> 248                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 249                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 250                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 251                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 252                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 253                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 254                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 255                                                                                                                                                 Shrink-Swell Clay
#> 256                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 257                                                                                                                      Isohyperthermic Torric Naturalized Grassland
#> 258                                                                                                                      Isohyperthermic Torric Naturalized Grassland
#> 259                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 260                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 261                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 262                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 263                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 264                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 265                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 266                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 267                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 268                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 269                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 270                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 271                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 272                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 273                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 274                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 275                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 276                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 277                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 278                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 279                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 280                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 281                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 282                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 283                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 284                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 285                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 286                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 287                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 288                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 289                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 290                                                                                                                                          Oxidic Dissected Lowland
#> 291                                                                                                                                          Oxidic Dissected Lowland
#> 292                                                                                                                                          Oxidic Dissected Lowland
#> 293                                                                                                                                          Oxidic Dissected Lowland
#> 294                                                                                                                                                  Flooded Alluvium
#> 295                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 296                                                                                                                                          Oxidic Dissected Lowland
#> 297                                                                                                                                                  Flooded Alluvium
#> 298                                                                                                                                                  Flooded Alluvium
#> 299                                                                                                                                                  Flooded Alluvium
#> 300                                                                                                                                          Oxidic Dissected Lowland
#> 301                                                                                                                                          Oxidic Dissected Lowland
#> 302                                                                                                                                          Oxidic Dissected Lowland
#> 303                                                                                                                                          Oxidic Dissected Lowland
#> 304                                                                                                                                          Oxidic Dissected Lowland
#> 305                                                                                                                                          Oxidic Dissected Lowland
#> 306                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 307                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 308                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 309                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 310                                                                                                                                                   Sandy Shrubland
#> 311                                                                                                                                                   Sandy Shrubland
#> 312                                                                                                                                                 Shrink-Swell Clay
#> 313                                                                                                                                                 Shrink-Swell Clay
#> 314                                                                                                                                          Oxidic Dissected Lowland
#> 315                                                                                                                                          Oxidic Dissected Lowland
#> 316                                                                                                                                          Oxidic Dissected Lowland
#> 317                                                                                                                                                  Flooded Alluvium
#> 318                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 319                                                                                                                                             Aquic Coastal Wetland
#> 320                                                                                                                                             Aquic Coastal Wetland
#> 321                                                                                                                                             Aquic Coastal Wetland
#> 322                                                                                                                                             Aquic Coastal Wetland
#> 323                                                                                                                                          Oxidic Dissected Lowland
#> 324                                                                                                                                          Oxidic Dissected Lowland
#> 325                                                                                                                                          Oxidic Dissected Lowland
#> 326                                                                                                                                          Oxidic Dissected Lowland
#> 327                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 328                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 329                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 330                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 331                                                                                                                                                  Flooded Alluvium
#> 332                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 333                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 334                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 335                                                                                                                                                Poorly Drained Bog
#> 336                                                                                                                                                  Flooded Alluvium
#> 337                                                                                                                                          Oxidic Dissected Lowland
#> 338                                                                                                                                          Oxidic Dissected Lowland
#> 339                                                                                                                                          Oxidic Dissected Lowland
#> 340                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 341                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 342                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 343                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 344                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 345                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 346                                                                                                                                                 Shrink-Swell Clay
#> 347                                                                                                                                                 Shrink-Swell Clay
#> 348                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 349                                                                                                                                                 Shrink-Swell Clay
#> 350                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 351                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 352                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 353                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 354                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 355                                                                                                                                          Oxidic Dissected Lowland
#> 356                                                                                                                                          Oxidic Dissected Lowland
#> 357                                                                                                                                          Oxidic Dissected Lowland
#> 358                                                                                                                                          Oxidic Dissected Lowland
#> 359                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 360                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 361                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 362                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 363                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 364                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 365                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 366                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 367                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 368                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 369                                                                                                                                             Aquic Coastal Wetland
#> 370                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 371                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 372                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 373                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 374                                                                                                                                             Aquic Coastal Wetland
#> 375                                                                                                                                                 Shrink-Swell Clay
#> 376                                                                                                                                                 Shrink-Swell Clay
#> 377                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 378                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 379                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 380                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 381                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 382                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 383                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 384                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 385                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 386                                                                                                                                          Oxidic Dissected Lowland
#> 387                                                                                                                                          Oxidic Dissected Lowland
#> 388                                                                                                                                          Oxidic Dissected Lowland
#> 389                                                                                                                                          Oxidic Dissected Lowland
#> 390                                                                                                                                          Oxidic Dissected Lowland
#> 391                                                                                                                                          Oxidic Dissected Lowland
#> 392                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 393                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 394                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 395                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 396                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 397                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 398                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 399                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 400                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 401                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 402                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 403                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 404                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 405                                                                                                                                                   Sandy Shrubland
#> 406                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 407                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 408                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 409                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 410                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 411                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 412                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 413                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 414                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 415                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 416                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 417                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 418                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 419                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 420                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 421                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 422                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 423                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 424                                                                                                                                                 Shrink-Swell Clay
#> 425                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 426                                                                                                                      Isohyperthermic Torric Naturalized Grassland
#> 427                                                                                                                      Isohyperthermic Torric Naturalized Grassland
#> 428                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 429                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 430                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 431                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 432                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 433                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 434                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 435                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 436                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 437                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 438                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 439                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 440                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 441                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 442                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 443                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 444                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 445                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 446                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 447                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 448                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 449                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 450                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 451                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 452                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 453                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 454                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 455                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 456                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 457                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 458                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 459                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 460                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 461                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 462                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 463                                                                                                                                          Oxidic Dissected Lowland
#> 464                                                                                                                                          Oxidic Dissected Lowland
#> 465                                                                                                                                          Oxidic Dissected Lowland
#> 466                                                                                                                                          Oxidic Dissected Lowland
#> 467                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 468                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 469                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 470                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 471                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 472                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 473                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 474                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 475                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 476                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 477                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 478                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 479                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 480                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 481                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 482                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 483                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 484                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 485                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 486                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 487                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 488                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 489                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 490                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 491                                                                                                                                                   Sandy Shrubland
#> 492                                                                                                                                                   Sandy Shrubland
#> 493                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 494                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 495                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 496                                                                                                                                          Oxidic Dissected Lowland
#> 497 Rocky Isothermic Naturalized Grassland Koa haole/guineagrass - buffelgrass/glycine (Leucaena leucocephala/Urochloa maxima - Cenchrus ciliaris/Neonotonia wightii)
#> 498 Rocky Isothermic Naturalized Grassland Koa haole/guineagrass - buffelgrass/glycine (Leucaena leucocephala/Urochloa maxima - Cenchrus ciliaris/Neonotonia wightii)
#> 499 Rocky Isothermic Naturalized Grassland Koa haole/guineagrass - buffelgrass/glycine (Leucaena leucocephala/Urochloa maxima - Cenchrus ciliaris/Neonotonia wightii)
#> 500                                                                                              Rocky Alluvium Naturalized Grassland (Koa haole/guineagrass/glycine)
#> 501                                                                                              Rocky Alluvium Naturalized Grassland (Koa haole/guineagrass/glycine)
#> 502                                                                                                                                               Aquisalids Herbland
#> 503                                                                                                                                               Aquisalids Herbland
#> 504                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 505                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 506                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 507                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 508                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 509                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 510                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 511                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 512                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 513                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 514                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 515                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 516                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 517                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 518                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 519                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 520                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 521                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 522                                                                                                                                          Oxidic Dissected Lowland
#> 523                                                                                                                                          Oxidic Dissected Lowland
#> 524                                                                                                                                          Oxidic Dissected Lowland
#> 525                                                                                                                                          Oxidic Dissected Lowland
#> 526                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 527                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 528                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 529                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 530                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 531                                                                                              Isohyperthermic Udic Naturalized Grassland (Guineagrass / Desmodium)
#> 532                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 533                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 534                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 535                                                                                     Isohyperthermic Perudic Naturalized Grassland (Guineagrass - Californiagrass)
#> 536                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 537                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 538                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 539                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 540                                                                                Rocky Volcanic Ash Savanna Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 541                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 542                                                                                                                            Isothermic Ustic Naturalized Grassland
#> 543                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 544                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 545                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 546                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 547                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 548                                                                                                                                          Oxidic Dissected Lowland
#> 549                                                                                                                                          Oxidic Dissected Lowland
#> 550                                                                                                                                          Oxidic Dissected Lowland
#> 551                                                                                                                                          Oxidic Dissected Lowland
#> 552                                                                                                                                          Oxidic Dissected Lowland
#> 553                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 554                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 555                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 556                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 557                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 558                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 559                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 560                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 561                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 562                                                                                                                                                   Sandy Shrubland
#> 563                                                                                                                                                   Sandy Shrubland
#> 564 Rocky Isothermic Naturalized Grassland Koa haole/guineagrass - buffelgrass/glycine (Leucaena leucocephala/Urochloa maxima - Cenchrus ciliaris/Neonotonia wightii)
#> 565                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 566                                                                                                              Isothermic Ustic Naturalized Grassland (Kikuyugrass)
#> 567                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 568                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 569                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 570                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 571                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 572                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 573                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 574                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 575                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 576                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 577                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 578                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 579                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 580                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 581                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 582                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 583                                                                                                                 East Aspect Isohyperthermic Naturalized Grassland
#> 584                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 585                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 586                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 587                                Rocky Isohyperthermic Torric Naturalized Grassland Kiawe/uhaloa/buffelgrass (Prosopis pallida/Waltheria indica/Pennisetum ciliare)
#> 588                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 589                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 590                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 591                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 592                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 593                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 594                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 595                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 596                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 597                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 598                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 599                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 600                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 601                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 602                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 603                                                                                                                                                  Flooded Alluvium
#> 604                                                                                                                                                  Flooded Alluvium
#> 605                                                                                                                                                  Flooded Alluvium
#> 606                                                                                                                                                 Shrink-Swell Clay
#> 607                                                                                                                                                 Shrink-Swell Clay
#> 608                                                                                                                                                   Sandy Shrubland
#> 609                                                                                                                                                   Sandy Shrubland
#> 610                                                                                                                                                 Shrink-Swell Clay
#> 611                                                                                                                                                 Shrink-Swell Clay
#> 612                                                                                                                                                 Shrink-Swell Clay
#> 613                                                                                                                                                 Shrink-Swell Clay
#> 614                                                                                                                                                 Shrink-Swell Clay
#> 615                                                                                                                                                 Shrink-Swell Clay
#> 616                                                                                                                                             Aquic Coastal Wetland
#> 617                                                                                                                                             Aquic Coastal Wetland
#> 618                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 619                                                                                                                                             Aquic Coastal Wetland
#> 620                                                                                                                                          Oxidic Dissected Lowland
#> 621                                                                                                                                          Oxidic Dissected Lowland
#> 622                                                                                                                                          Oxidic Dissected Lowland
#> 623                                                                                                                                          Oxidic Dissected Lowland
#> 624                                                                                                                                          Oxidic Dissected Lowland
#> 625                                                                                                                                          Oxidic Dissected Lowland
#> 626                                                                                                                                          Oxidic Dissected Lowland
#> 627                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 628                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 629                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 630                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 631                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 632                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 633                                                                                                                                             Aquic Coastal Wetland
#> 634                                                                                                                                             Aquic Coastal Wetland
#> 635                                                                                                                                             Aquic Coastal Wetland
#> 636                                                                                                                                             Aquic Coastal Wetland
#> 637                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 638                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 639                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 640                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 641                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 642                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 643                                                                                                                      Isohyperthermic Torric Naturalized Grassland
#> 644                                                                                                                      Isohyperthermic Torric Naturalized Grassland
#> 645                                                                                                                      Isohyperthermic Torric Naturalized Grassland
#> 646                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 647                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 648                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 649                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 650                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 651                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 652                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 653                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 654                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 655                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 656                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 657                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 658                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 659                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 660                                                                                                                                                  Flooded Alluvium
#> 661                                                                                                                                          Oxidic Dissected Lowland
#> 662                                                                                                                                          Oxidic Dissected Lowland
#> 663                                                                                                                                          Oxidic Dissected Lowland
#> 664                                                                                                                                          Oxidic Dissected Lowland
#> 665                                                                                                                                          Oxidic Dissected Lowland
#> 666                                                                                                                                          Oxidic Dissected Lowland
#> 667                                                                                                                                                 Shrink-Swell Clay
#> 668                                                                                                                                                 Shrink-Swell Clay
#> 669                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 670                                                                                                                                                 Shrink-Swell Clay
#> 671                                                                                                                                                 Shrink-Swell Clay
#> 672                                                                                                                                                 Shrink-Swell Clay
#> 673                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 674                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 675                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 676                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 677                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 678                                                                                                                                                 Shrink-Swell Clay
#> 679                                                                                                                                                 Shrink-Swell Clay
#> 680                                                                                                                                                 Shrink-Swell Clay
#> 681                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 682                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 683                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 684                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 685                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 686                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 687                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 688                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 689                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 690                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 691                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 692                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 693                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 694                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 695                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 696                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 697                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 698                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 699                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 700                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 701                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 702                                                   Naturalized Grassland 50 to 90 inch PZ Ohia lehua/kikuyugrass (Metrosideros polymorpha/Pennisetum clandestinum)
#> 703                                                                                                                                          Oxidic Dissected Lowland
#> 704                                                                                                                                          Oxidic Dissected Lowland
#> 705                                                                                                                                          Oxidic Dissected Lowland
#> 706                                                                                                                                          Oxidic Dissected Lowland
#> 707                                                                                                                                          Oxidic Dissected Lowland
#> 708                                                                                                                                             Aquic Coastal Wetland
#> 709                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 710                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 711                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 712                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 713                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 714                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 715                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 716                                                              Isohyperthermic Torric Naturalized Grassland Kiawe/buffelgrass (Prosopis pallida/Pennisetum ciliare)
#> 717                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 718                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 719                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 720                                                                                                                                          Oxidic Dissected Lowland
#> 721                                                                                                                                          Oxidic Dissected Lowland
#> 722                                                                                                                                          Oxidic Dissected Lowland
#> 723                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 724                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 725                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 726                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 727                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 728                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 729                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 730                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 731                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 732                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 733                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 734                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 735                                                                                                                                          Oxidic Dissected Lowland
#> 736                                                                                                                                          Oxidic Dissected Lowland
#> 737                                                                                                                                          Oxidic Dissected Lowland
#> 738                                                                                                                                          Oxidic Dissected Lowland
#> 739                                                                                                                                          Oxidic Dissected Lowland
#> 740                                                                                                                                          Oxidic Dissected Lowland
#> 741                              Isohyperthermic Ustic Naturalized Grassland Koa haole/guineagrass/glycine (Leucaena leucocephala/Urochloa maxima/Neonotonia wightii)
#> 742                                                                                                                                          Oxidic Dissected Lowland
#> 743                                                                                                                                          Oxidic Dissected Lowland
#> 744                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 745                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#> 746                                                                                                       South and West Aspect Isohyperthermic Naturalized Grassland
#>        ecoclasstypename                          ecoclassref
#> 1   NRCS Rangeland Site Ecological Site Description Database
#> 2   NRCS Rangeland Site Ecological Site Description Database
#> 3   NRCS Rangeland Site Ecological Site Description Database
#> 4   NRCS Rangeland Site Ecological Site Description Database
#> 5   NRCS Rangeland Site Ecological Site Description Database
#> 6   NRCS Rangeland Site Ecological Site Description Database
#> 7   NRCS Rangeland Site Ecological Site Description Database
#> 8   NRCS Rangeland Site Ecological Site Description Database
#> 9   NRCS Rangeland Site Ecological Site Description Database
#> 10  NRCS Rangeland Site Ecological Site Description Database
#> 11  NRCS Rangeland Site Ecological Site Description Database
#> 12  NRCS Rangeland Site Ecological Site Description Database
#> 13  NRCS Rangeland Site Ecological Site Description Database
#> 14  NRCS Rangeland Site Ecological Site Description Database
#> 15  NRCS Rangeland Site Ecological Site Description Database
#> 16  NRCS Rangeland Site Ecological Site Description Database
#> 17  NRCS Rangeland Site Ecological Site Description Database
#> 18  NRCS Rangeland Site Ecological Site Description Database
#> 19  NRCS Rangeland Site Ecological Site Description Database
#> 20  NRCS Rangeland Site Ecological Site Description Database
#> 21  NRCS Rangeland Site Ecological Site Description Database
#> 22  NRCS Rangeland Site Ecological Site Description Database
#> 23  NRCS Rangeland Site Ecological Site Description Database
#> 24  NRCS Rangeland Site Ecological Site Description Database
#> 25  NRCS Rangeland Site Ecological Site Description Database
#> 26  NRCS Rangeland Site Ecological Site Description Database
#> 27  NRCS Rangeland Site Ecological Site Description Database
#> 28  NRCS Rangeland Site Ecological Site Description Database
#> 29  NRCS Rangeland Site Ecological Site Description Database
#> 30  NRCS Rangeland Site Ecological Site Description Database
#> 31  NRCS Rangeland Site Ecological Site Description Database
#> 32  NRCS Rangeland Site Ecological Site Description Database
#> 33  NRCS Rangeland Site Ecological Site Description Database
#> 34  NRCS Rangeland Site Ecological Site Description Database
#> 35  NRCS Rangeland Site Ecological Site Description Database
#> 36  NRCS Rangeland Site Ecological Site Description Database
#> 37  NRCS Rangeland Site Ecological Site Description Database
#> 38  NRCS Rangeland Site Ecological Site Description Database
#> 39  NRCS Rangeland Site Ecological Site Description Database
#> 40  NRCS Rangeland Site Ecological Site Description Database
#> 41  NRCS Rangeland Site Ecological Site Description Database
#> 42  NRCS Rangeland Site Ecological Site Description Database
#> 43  NRCS Rangeland Site Ecological Site Description Database
#> 44  NRCS Rangeland Site Ecological Site Description Database
#> 45  NRCS Rangeland Site Ecological Site Description Database
#> 46  NRCS Rangeland Site Ecological Site Description Database
#> 47  NRCS Rangeland Site Ecological Site Description Database
#> 48  NRCS Rangeland Site Ecological Site Description Database
#> 49  NRCS Rangeland Site Ecological Site Description Database
#> 50  NRCS Rangeland Site Ecological Site Description Database
#> 51  NRCS Rangeland Site Ecological Site Description Database
#> 52  NRCS Rangeland Site Ecological Site Description Database
#> 53  NRCS Rangeland Site Ecological Site Description Database
#> 54  NRCS Rangeland Site Ecological Site Description Database
#> 55  NRCS Rangeland Site Ecological Site Description Database
#> 56  NRCS Rangeland Site Ecological Site Description Database
#> 57  NRCS Rangeland Site Ecological Site Description Database
#> 58  NRCS Rangeland Site Ecological Site Description Database
#> 59  NRCS Rangeland Site Ecological Site Description Database
#> 60  NRCS Rangeland Site Ecological Site Description Database
#> 61  NRCS Rangeland Site Ecological Site Description Database
#> 62  NRCS Rangeland Site Ecological Site Description Database
#> 63  NRCS Rangeland Site Ecological Site Description Database
#> 64  NRCS Rangeland Site Ecological Site Description Database
#> 65  NRCS Rangeland Site Ecological Site Description Database
#> 66  NRCS Rangeland Site Ecological Site Description Database
#> 67  NRCS Rangeland Site Ecological Site Description Database
#> 68  NRCS Rangeland Site Ecological Site Description Database
#> 69  NRCS Rangeland Site Ecological Site Description Database
#> 70  NRCS Rangeland Site Ecological Site Description Database
#> 71  NRCS Rangeland Site Ecological Site Description Database
#> 72  NRCS Rangeland Site Ecological Site Description Database
#> 73  NRCS Rangeland Site Ecological Site Description Database
#> 74  NRCS Rangeland Site Ecological Site Description Database
#> 75  NRCS Rangeland Site Ecological Site Description Database
#> 76  NRCS Rangeland Site Ecological Site Description Database
#> 77  NRCS Rangeland Site Ecological Site Description Database
#> 78  NRCS Rangeland Site Ecological Site Description Database
#> 79  NRCS Rangeland Site Ecological Site Description Database
#> 80  NRCS Rangeland Site Ecological Site Description Database
#> 81  NRCS Rangeland Site Ecological Site Description Database
#> 82  NRCS Rangeland Site Ecological Site Description Database
#> 83  NRCS Rangeland Site Ecological Site Description Database
#> 84  NRCS Rangeland Site Ecological Site Description Database
#> 85  NRCS Rangeland Site Ecological Site Description Database
#> 86  NRCS Rangeland Site Ecological Site Description Database
#> 87  NRCS Rangeland Site Ecological Site Description Database
#> 88  NRCS Rangeland Site Ecological Site Description Database
#> 89  NRCS Rangeland Site Ecological Site Description Database
#> 90  NRCS Rangeland Site Ecological Site Description Database
#> 91  NRCS Rangeland Site Ecological Site Description Database
#> 92  NRCS Rangeland Site Ecological Site Description Database
#> 93  NRCS Rangeland Site Ecological Site Description Database
#> 94  NRCS Rangeland Site Ecological Site Description Database
#> 95  NRCS Rangeland Site Ecological Site Description Database
#> 96  NRCS Rangeland Site Ecological Site Description Database
#> 97  NRCS Rangeland Site Ecological Site Description Database
#> 98  NRCS Rangeland Site Ecological Site Description Database
#> 99  NRCS Rangeland Site Ecological Site Description Database
#> 100 NRCS Rangeland Site Ecological Site Description Database
#> 101 NRCS Rangeland Site Ecological Site Description Database
#> 102 NRCS Rangeland Site Ecological Site Description Database
#> 103 NRCS Rangeland Site Ecological Site Description Database
#> 104 NRCS Rangeland Site Ecological Site Description Database
#> 105 NRCS Rangeland Site Ecological Site Description Database
#> 106 NRCS Rangeland Site Ecological Site Description Database
#> 107 NRCS Rangeland Site Ecological Site Description Database
#> 108 NRCS Rangeland Site Ecological Site Description Database
#> 109 NRCS Rangeland Site Ecological Site Description Database
#> 110 NRCS Rangeland Site Ecological Site Description Database
#> 111 NRCS Rangeland Site Ecological Site Description Database
#> 112 NRCS Rangeland Site Ecological Site Description Database
#> 113 NRCS Rangeland Site Ecological Site Description Database
#> 114 NRCS Rangeland Site Ecological Site Description Database
#> 115 NRCS Rangeland Site Ecological Site Description Database
#> 116 NRCS Rangeland Site Ecological Site Description Database
#> 117 NRCS Rangeland Site Ecological Site Description Database
#> 118 NRCS Rangeland Site Ecological Site Description Database
#> 119 NRCS Rangeland Site Ecological Site Description Database
#> 120 NRCS Rangeland Site Ecological Site Description Database
#> 121 NRCS Rangeland Site Ecological Site Description Database
#> 122 NRCS Rangeland Site Ecological Site Description Database
#> 123 NRCS Rangeland Site Ecological Site Description Database
#> 124 NRCS Rangeland Site Ecological Site Description Database
#> 125 NRCS Rangeland Site Ecological Site Description Database
#> 126 NRCS Rangeland Site Ecological Site Description Database
#> 127 NRCS Rangeland Site Ecological Site Description Database
#> 128 NRCS Rangeland Site Ecological Site Description Database
#> 129 NRCS Rangeland Site Ecological Site Description Database
#> 130 NRCS Rangeland Site Ecological Site Description Database
#> 131 NRCS Rangeland Site Ecological Site Description Database
#> 132 NRCS Rangeland Site Ecological Site Description Database
#> 133 NRCS Rangeland Site Ecological Site Description Database
#> 134 NRCS Rangeland Site Ecological Site Description Database
#> 135 NRCS Rangeland Site Ecological Site Description Database
#> 136 NRCS Rangeland Site Ecological Site Description Database
#> 137 NRCS Rangeland Site Ecological Site Description Database
#> 138 NRCS Rangeland Site Ecological Site Description Database
#> 139 NRCS Rangeland Site Ecological Site Description Database
#> 140 NRCS Rangeland Site Ecological Site Description Database
#> 141 NRCS Rangeland Site Ecological Site Description Database
#> 142 NRCS Rangeland Site Ecological Site Description Database
#> 143 NRCS Rangeland Site Ecological Site Description Database
#> 144 NRCS Rangeland Site Ecological Site Description Database
#> 145 NRCS Rangeland Site Ecological Site Description Database
#> 146 NRCS Rangeland Site Ecological Site Description Database
#> 147 NRCS Rangeland Site Ecological Site Description Database
#> 148 NRCS Rangeland Site Ecological Site Description Database
#> 149 NRCS Rangeland Site Ecological Site Description Database
#> 150 NRCS Rangeland Site Ecological Site Description Database
#> 151 NRCS Rangeland Site Ecological Site Description Database
#> 152 NRCS Rangeland Site Ecological Site Description Database
#> 153 NRCS Rangeland Site Ecological Site Description Database
#> 154 NRCS Rangeland Site Ecological Site Description Database
#> 155 NRCS Rangeland Site Ecological Site Description Database
#> 156 NRCS Rangeland Site Ecological Site Description Database
#> 157 NRCS Rangeland Site Ecological Site Description Database
#> 158 NRCS Rangeland Site Ecological Site Description Database
#> 159 NRCS Rangeland Site Ecological Site Description Database
#> 160 NRCS Rangeland Site Ecological Site Description Database
#> 161 NRCS Rangeland Site Ecological Site Description Database
#> 162 NRCS Rangeland Site Ecological Site Description Database
#> 163 NRCS Rangeland Site Ecological Site Description Database
#> 164 NRCS Rangeland Site Ecological Site Description Database
#> 165 NRCS Rangeland Site Ecological Site Description Database
#> 166 NRCS Rangeland Site Ecological Site Description Database
#> 167 NRCS Rangeland Site Ecological Site Description Database
#> 168 NRCS Rangeland Site Ecological Site Description Database
#> 169 NRCS Rangeland Site Ecological Site Description Database
#> 170 NRCS Rangeland Site Ecological Site Description Database
#> 171 NRCS Rangeland Site Ecological Site Description Database
#> 172 NRCS Rangeland Site Ecological Site Description Database
#> 173 NRCS Rangeland Site Ecological Site Description Database
#> 174 NRCS Rangeland Site Ecological Site Description Database
#> 175 NRCS Rangeland Site Ecological Site Description Database
#> 176 NRCS Rangeland Site Ecological Site Description Database
#> 177 NRCS Rangeland Site Ecological Site Description Database
#> 178 NRCS Rangeland Site Ecological Site Description Database
#> 179 NRCS Rangeland Site Ecological Site Description Database
#> 180 NRCS Rangeland Site Ecological Site Description Database
#> 181 NRCS Rangeland Site Ecological Site Description Database
#> 182 NRCS Rangeland Site Ecological Site Description Database
#> 183 NRCS Rangeland Site Ecological Site Description Database
#> 184 NRCS Rangeland Site Ecological Site Description Database
#> 185 NRCS Rangeland Site Ecological Site Description Database
#> 186 NRCS Rangeland Site Ecological Site Description Database
#> 187 NRCS Rangeland Site Ecological Site Description Database
#> 188 NRCS Rangeland Site Ecological Site Description Database
#> 189 NRCS Rangeland Site Ecological Site Description Database
#> 190 NRCS Rangeland Site Ecological Site Description Database
#> 191 NRCS Rangeland Site Ecological Site Description Database
#> 192 NRCS Rangeland Site Ecological Site Description Database
#> 193 NRCS Rangeland Site Ecological Site Description Database
#> 194 NRCS Rangeland Site Ecological Site Description Database
#> 195 NRCS Rangeland Site Ecological Site Description Database
#> 196 NRCS Rangeland Site Ecological Site Description Database
#> 197 NRCS Rangeland Site Ecological Site Description Database
#> 198 NRCS Rangeland Site Ecological Site Description Database
#> 199 NRCS Rangeland Site Ecological Site Description Database
#> 200 NRCS Rangeland Site Ecological Site Description Database
#> 201 NRCS Rangeland Site Ecological Site Description Database
#> 202 NRCS Rangeland Site Ecological Site Description Database
#> 203 NRCS Rangeland Site Ecological Site Description Database
#> 204 NRCS Rangeland Site Ecological Site Description Database
#> 205 NRCS Rangeland Site Ecological Site Description Database
#> 206 NRCS Rangeland Site Ecological Site Description Database
#> 207 NRCS Rangeland Site Ecological Site Description Database
#> 208 NRCS Rangeland Site Ecological Site Description Database
#> 209 NRCS Rangeland Site Ecological Site Description Database
#> 210 NRCS Rangeland Site Ecological Site Description Database
#> 211 NRCS Rangeland Site Ecological Site Description Database
#> 212 NRCS Rangeland Site Ecological Site Description Database
#> 213 NRCS Rangeland Site Ecological Site Description Database
#> 214 NRCS Rangeland Site Ecological Site Description Database
#> 215 NRCS Rangeland Site Ecological Site Description Database
#> 216 NRCS Rangeland Site Ecological Site Description Database
#> 217 NRCS Rangeland Site Ecological Site Description Database
#> 218 NRCS Rangeland Site Ecological Site Description Database
#> 219 NRCS Rangeland Site Ecological Site Description Database
#> 220 NRCS Rangeland Site Ecological Site Description Database
#> 221 NRCS Rangeland Site Ecological Site Description Database
#> 222 NRCS Rangeland Site Ecological Site Description Database
#> 223 NRCS Rangeland Site Ecological Site Description Database
#> 224 NRCS Rangeland Site Ecological Site Description Database
#> 225 NRCS Rangeland Site Ecological Site Description Database
#> 226 NRCS Rangeland Site Ecological Site Description Database
#> 227 NRCS Rangeland Site Ecological Site Description Database
#> 228 NRCS Rangeland Site Ecological Site Description Database
#> 229 NRCS Rangeland Site Ecological Site Description Database
#> 230 NRCS Rangeland Site Ecological Site Description Database
#> 231 NRCS Rangeland Site Ecological Site Description Database
#> 232 NRCS Rangeland Site Ecological Site Description Database
#> 233 NRCS Rangeland Site Ecological Site Description Database
#> 234 NRCS Rangeland Site Ecological Site Description Database
#> 235 NRCS Rangeland Site Ecological Site Description Database
#> 236 NRCS Rangeland Site Ecological Site Description Database
#> 237 NRCS Rangeland Site Ecological Site Description Database
#> 238 NRCS Rangeland Site Ecological Site Description Database
#> 239 NRCS Rangeland Site Ecological Site Description Database
#> 240 NRCS Rangeland Site Ecological Site Description Database
#> 241 NRCS Rangeland Site Ecological Site Description Database
#> 242 NRCS Rangeland Site Ecological Site Description Database
#> 243 NRCS Rangeland Site Ecological Site Description Database
#> 244 NRCS Rangeland Site Ecological Site Description Database
#> 245 NRCS Rangeland Site Ecological Site Description Database
#> 246 NRCS Rangeland Site Ecological Site Description Database
#> 247 NRCS Rangeland Site Ecological Site Description Database
#> 248 NRCS Rangeland Site Ecological Site Description Database
#> 249 NRCS Rangeland Site Ecological Site Description Database
#> 250 NRCS Rangeland Site Ecological Site Description Database
#> 251 NRCS Rangeland Site Ecological Site Description Database
#> 252 NRCS Rangeland Site Ecological Site Description Database
#> 253 NRCS Rangeland Site Ecological Site Description Database
#> 254 NRCS Rangeland Site Ecological Site Description Database
#> 255 NRCS Rangeland Site Ecological Site Description Database
#> 256 NRCS Rangeland Site Ecological Site Description Database
#> 257 NRCS Rangeland Site Ecological Site Description Database
#> 258 NRCS Rangeland Site Ecological Site Description Database
#> 259 NRCS Rangeland Site Ecological Site Description Database
#> 260 NRCS Rangeland Site Ecological Site Description Database
#> 261 NRCS Rangeland Site Ecological Site Description Database
#> 262 NRCS Rangeland Site Ecological Site Description Database
#> 263 NRCS Rangeland Site Ecological Site Description Database
#> 264 NRCS Rangeland Site Ecological Site Description Database
#> 265 NRCS Rangeland Site Ecological Site Description Database
#> 266 NRCS Rangeland Site Ecological Site Description Database
#> 267 NRCS Rangeland Site Ecological Site Description Database
#> 268 NRCS Rangeland Site Ecological Site Description Database
#> 269 NRCS Rangeland Site Ecological Site Description Database
#> 270 NRCS Rangeland Site Ecological Site Description Database
#> 271 NRCS Rangeland Site Ecological Site Description Database
#> 272 NRCS Rangeland Site Ecological Site Description Database
#> 273 NRCS Rangeland Site Ecological Site Description Database
#> 274 NRCS Rangeland Site Ecological Site Description Database
#> 275 NRCS Rangeland Site Ecological Site Description Database
#> 276 NRCS Rangeland Site Ecological Site Description Database
#> 277 NRCS Rangeland Site Ecological Site Description Database
#> 278 NRCS Rangeland Site Ecological Site Description Database
#> 279 NRCS Rangeland Site Ecological Site Description Database
#> 280 NRCS Rangeland Site Ecological Site Description Database
#> 281 NRCS Rangeland Site Ecological Site Description Database
#> 282 NRCS Rangeland Site Ecological Site Description Database
#> 283 NRCS Rangeland Site Ecological Site Description Database
#> 284 NRCS Rangeland Site Ecological Site Description Database
#> 285 NRCS Rangeland Site Ecological Site Description Database
#> 286 NRCS Rangeland Site Ecological Site Description Database
#> 287 NRCS Rangeland Site Ecological Site Description Database
#> 288 NRCS Rangeland Site Ecological Site Description Database
#> 289 NRCS Rangeland Site Ecological Site Description Database
#> 290 NRCS Rangeland Site Ecological Site Description Database
#> 291 NRCS Rangeland Site Ecological Site Description Database
#> 292 NRCS Rangeland Site Ecological Site Description Database
#> 293 NRCS Rangeland Site Ecological Site Description Database
#> 294 NRCS Rangeland Site Ecological Site Description Database
#> 295 NRCS Rangeland Site Ecological Site Description Database
#> 296 NRCS Rangeland Site Ecological Site Description Database
#> 297 NRCS Rangeland Site Ecological Site Description Database
#> 298 NRCS Rangeland Site Ecological Site Description Database
#> 299 NRCS Rangeland Site Ecological Site Description Database
#> 300 NRCS Rangeland Site Ecological Site Description Database
#> 301 NRCS Rangeland Site Ecological Site Description Database
#> 302 NRCS Rangeland Site Ecological Site Description Database
#> 303 NRCS Rangeland Site Ecological Site Description Database
#> 304 NRCS Rangeland Site Ecological Site Description Database
#> 305 NRCS Rangeland Site Ecological Site Description Database
#> 306 NRCS Rangeland Site Ecological Site Description Database
#> 307 NRCS Rangeland Site Ecological Site Description Database
#> 308 NRCS Rangeland Site Ecological Site Description Database
#> 309 NRCS Rangeland Site Ecological Site Description Database
#> 310 NRCS Rangeland Site Ecological Site Description Database
#> 311 NRCS Rangeland Site Ecological Site Description Database
#> 312 NRCS Rangeland Site Ecological Site Description Database
#> 313 NRCS Rangeland Site Ecological Site Description Database
#> 314 NRCS Rangeland Site Ecological Site Description Database
#> 315 NRCS Rangeland Site Ecological Site Description Database
#> 316 NRCS Rangeland Site Ecological Site Description Database
#> 317 NRCS Rangeland Site Ecological Site Description Database
#> 318 NRCS Rangeland Site Ecological Site Description Database
#> 319 NRCS Rangeland Site Ecological Site Description Database
#> 320 NRCS Rangeland Site Ecological Site Description Database
#> 321 NRCS Rangeland Site Ecological Site Description Database
#> 322 NRCS Rangeland Site Ecological Site Description Database
#> 323 NRCS Rangeland Site Ecological Site Description Database
#> 324 NRCS Rangeland Site Ecological Site Description Database
#> 325 NRCS Rangeland Site Ecological Site Description Database
#> 326 NRCS Rangeland Site Ecological Site Description Database
#> 327 NRCS Rangeland Site Ecological Site Description Database
#> 328 NRCS Rangeland Site Ecological Site Description Database
#> 329 NRCS Rangeland Site Ecological Site Description Database
#> 330 NRCS Rangeland Site Ecological Site Description Database
#> 331 NRCS Rangeland Site Ecological Site Description Database
#> 332 NRCS Rangeland Site Ecological Site Description Database
#> 333 NRCS Rangeland Site Ecological Site Description Database
#> 334 NRCS Rangeland Site Ecological Site Description Database
#> 335 NRCS Rangeland Site Ecological Site Description Database
#> 336 NRCS Rangeland Site Ecological Site Description Database
#> 337 NRCS Rangeland Site Ecological Site Description Database
#> 338 NRCS Rangeland Site Ecological Site Description Database
#> 339 NRCS Rangeland Site Ecological Site Description Database
#> 340 NRCS Rangeland Site Ecological Site Description Database
#> 341 NRCS Rangeland Site Ecological Site Description Database
#> 342 NRCS Rangeland Site Ecological Site Description Database
#> 343 NRCS Rangeland Site Ecological Site Description Database
#> 344 NRCS Rangeland Site Ecological Site Description Database
#> 345 NRCS Rangeland Site Ecological Site Description Database
#> 346 NRCS Rangeland Site Ecological Site Description Database
#> 347 NRCS Rangeland Site Ecological Site Description Database
#> 348 NRCS Rangeland Site Ecological Site Description Database
#> 349 NRCS Rangeland Site Ecological Site Description Database
#> 350 NRCS Rangeland Site Ecological Site Description Database
#> 351 NRCS Rangeland Site Ecological Site Description Database
#> 352 NRCS Rangeland Site Ecological Site Description Database
#> 353 NRCS Rangeland Site Ecological Site Description Database
#> 354 NRCS Rangeland Site Ecological Site Description Database
#> 355 NRCS Rangeland Site Ecological Site Description Database
#> 356 NRCS Rangeland Site Ecological Site Description Database
#> 357 NRCS Rangeland Site Ecological Site Description Database
#> 358 NRCS Rangeland Site Ecological Site Description Database
#> 359 NRCS Rangeland Site Ecological Site Description Database
#> 360 NRCS Rangeland Site Ecological Site Description Database
#> 361 NRCS Rangeland Site Ecological Site Description Database
#> 362 NRCS Rangeland Site Ecological Site Description Database
#> 363 NRCS Rangeland Site Ecological Site Description Database
#> 364 NRCS Rangeland Site Ecological Site Description Database
#> 365 NRCS Rangeland Site Ecological Site Description Database
#> 366 NRCS Rangeland Site Ecological Site Description Database
#> 367 NRCS Rangeland Site Ecological Site Description Database
#> 368 NRCS Rangeland Site Ecological Site Description Database
#> 369 NRCS Rangeland Site Ecological Site Description Database
#> 370 NRCS Rangeland Site Ecological Site Description Database
#> 371 NRCS Rangeland Site Ecological Site Description Database
#> 372 NRCS Rangeland Site Ecological Site Description Database
#> 373 NRCS Rangeland Site Ecological Site Description Database
#> 374 NRCS Rangeland Site Ecological Site Description Database
#> 375 NRCS Rangeland Site Ecological Site Description Database
#> 376 NRCS Rangeland Site Ecological Site Description Database
#> 377 NRCS Rangeland Site Ecological Site Description Database
#> 378 NRCS Rangeland Site Ecological Site Description Database
#> 379 NRCS Rangeland Site Ecological Site Description Database
#> 380 NRCS Rangeland Site Ecological Site Description Database
#> 381 NRCS Rangeland Site Ecological Site Description Database
#> 382 NRCS Rangeland Site Ecological Site Description Database
#> 383 NRCS Rangeland Site Ecological Site Description Database
#> 384 NRCS Rangeland Site Ecological Site Description Database
#> 385 NRCS Rangeland Site Ecological Site Description Database
#> 386 NRCS Rangeland Site Ecological Site Description Database
#> 387 NRCS Rangeland Site Ecological Site Description Database
#> 388 NRCS Rangeland Site Ecological Site Description Database
#> 389 NRCS Rangeland Site Ecological Site Description Database
#> 390 NRCS Rangeland Site Ecological Site Description Database
#> 391 NRCS Rangeland Site Ecological Site Description Database
#> 392 NRCS Rangeland Site Ecological Site Description Database
#> 393 NRCS Rangeland Site Ecological Site Description Database
#> 394 NRCS Rangeland Site Ecological Site Description Database
#> 395 NRCS Rangeland Site Ecological Site Description Database
#> 396 NRCS Rangeland Site Ecological Site Description Database
#> 397 NRCS Rangeland Site Ecological Site Description Database
#> 398 NRCS Rangeland Site Ecological Site Description Database
#> 399 NRCS Rangeland Site Ecological Site Description Database
#> 400 NRCS Rangeland Site Ecological Site Description Database
#> 401 NRCS Rangeland Site Ecological Site Description Database
#> 402 NRCS Rangeland Site Ecological Site Description Database
#> 403 NRCS Rangeland Site Ecological Site Description Database
#> 404 NRCS Rangeland Site Ecological Site Description Database
#> 405 NRCS Rangeland Site Ecological Site Description Database
#> 406 NRCS Rangeland Site Ecological Site Description Database
#> 407 NRCS Rangeland Site Ecological Site Description Database
#> 408 NRCS Rangeland Site Ecological Site Description Database
#> 409 NRCS Rangeland Site Ecological Site Description Database
#> 410 NRCS Rangeland Site Ecological Site Description Database
#> 411 NRCS Rangeland Site Ecological Site Description Database
#> 412 NRCS Rangeland Site Ecological Site Description Database
#> 413 NRCS Rangeland Site Ecological Site Description Database
#> 414 NRCS Rangeland Site Ecological Site Description Database
#> 415 NRCS Rangeland Site Ecological Site Description Database
#> 416 NRCS Rangeland Site Ecological Site Description Database
#> 417 NRCS Rangeland Site Ecological Site Description Database
#> 418 NRCS Rangeland Site Ecological Site Description Database
#> 419 NRCS Rangeland Site Ecological Site Description Database
#> 420 NRCS Rangeland Site Ecological Site Description Database
#> 421 NRCS Rangeland Site Ecological Site Description Database
#> 422 NRCS Rangeland Site Ecological Site Description Database
#> 423 NRCS Rangeland Site Ecological Site Description Database
#> 424 NRCS Rangeland Site Ecological Site Description Database
#> 425 NRCS Rangeland Site Ecological Site Description Database
#> 426 NRCS Rangeland Site Ecological Site Description Database
#> 427 NRCS Rangeland Site Ecological Site Description Database
#> 428 NRCS Rangeland Site Ecological Site Description Database
#> 429 NRCS Rangeland Site Ecological Site Description Database
#> 430 NRCS Rangeland Site Ecological Site Description Database
#> 431 NRCS Rangeland Site Ecological Site Description Database
#> 432 NRCS Rangeland Site Ecological Site Description Database
#> 433 NRCS Rangeland Site Ecological Site Description Database
#> 434 NRCS Rangeland Site Ecological Site Description Database
#> 435 NRCS Rangeland Site Ecological Site Description Database
#> 436 NRCS Rangeland Site Ecological Site Description Database
#> 437 NRCS Rangeland Site Ecological Site Description Database
#> 438 NRCS Rangeland Site Ecological Site Description Database
#> 439 NRCS Rangeland Site Ecological Site Description Database
#> 440 NRCS Rangeland Site Ecological Site Description Database
#> 441 NRCS Rangeland Site Ecological Site Description Database
#> 442 NRCS Rangeland Site Ecological Site Description Database
#> 443 NRCS Rangeland Site Ecological Site Description Database
#> 444 NRCS Rangeland Site Ecological Site Description Database
#> 445 NRCS Rangeland Site Ecological Site Description Database
#> 446 NRCS Rangeland Site Ecological Site Description Database
#> 447 NRCS Rangeland Site Ecological Site Description Database
#> 448 NRCS Rangeland Site Ecological Site Description Database
#> 449 NRCS Rangeland Site Ecological Site Description Database
#> 450 NRCS Rangeland Site Ecological Site Description Database
#> 451 NRCS Rangeland Site Ecological Site Description Database
#> 452 NRCS Rangeland Site Ecological Site Description Database
#> 453 NRCS Rangeland Site Ecological Site Description Database
#> 454 NRCS Rangeland Site Ecological Site Description Database
#> 455 NRCS Rangeland Site Ecological Site Description Database
#> 456 NRCS Rangeland Site Ecological Site Description Database
#> 457 NRCS Rangeland Site Ecological Site Description Database
#> 458 NRCS Rangeland Site Ecological Site Description Database
#> 459 NRCS Rangeland Site Ecological Site Description Database
#> 460 NRCS Rangeland Site Ecological Site Description Database
#> 461 NRCS Rangeland Site Ecological Site Description Database
#> 462 NRCS Rangeland Site Ecological Site Description Database
#> 463 NRCS Rangeland Site Ecological Site Description Database
#> 464 NRCS Rangeland Site Ecological Site Description Database
#> 465 NRCS Rangeland Site Ecological Site Description Database
#> 466 NRCS Rangeland Site Ecological Site Description Database
#> 467 NRCS Rangeland Site Ecological Site Description Database
#> 468 NRCS Rangeland Site Ecological Site Description Database
#> 469 NRCS Rangeland Site Ecological Site Description Database
#> 470 NRCS Rangeland Site Ecological Site Description Database
#> 471 NRCS Rangeland Site Ecological Site Description Database
#> 472 NRCS Rangeland Site Ecological Site Description Database
#> 473 NRCS Rangeland Site Ecological Site Description Database
#> 474 NRCS Rangeland Site Ecological Site Description Database
#> 475 NRCS Rangeland Site Ecological Site Description Database
#> 476 NRCS Rangeland Site Ecological Site Description Database
#> 477 NRCS Rangeland Site Ecological Site Description Database
#> 478 NRCS Rangeland Site Ecological Site Description Database
#> 479 NRCS Rangeland Site Ecological Site Description Database
#> 480 NRCS Rangeland Site Ecological Site Description Database
#> 481 NRCS Rangeland Site Ecological Site Description Database
#> 482 NRCS Rangeland Site Ecological Site Description Database
#> 483 NRCS Rangeland Site Ecological Site Description Database
#> 484 NRCS Rangeland Site Ecological Site Description Database
#> 485 NRCS Rangeland Site Ecological Site Description Database
#> 486 NRCS Rangeland Site Ecological Site Description Database
#> 487 NRCS Rangeland Site Ecological Site Description Database
#> 488 NRCS Rangeland Site Ecological Site Description Database
#> 489 NRCS Rangeland Site Ecological Site Description Database
#> 490 NRCS Rangeland Site Ecological Site Description Database
#> 491 NRCS Rangeland Site Ecological Site Description Database
#> 492 NRCS Rangeland Site Ecological Site Description Database
#> 493 NRCS Rangeland Site Ecological Site Description Database
#> 494 NRCS Rangeland Site Ecological Site Description Database
#> 495 NRCS Rangeland Site Ecological Site Description Database
#> 496 NRCS Rangeland Site Ecological Site Description Database
#> 497 NRCS Rangeland Site Ecological Site Description Database
#> 498 NRCS Rangeland Site Ecological Site Description Database
#> 499 NRCS Rangeland Site Ecological Site Description Database
#> 500 NRCS Rangeland Site Ecological Site Description Database
#> 501 NRCS Rangeland Site Ecological Site Description Database
#> 502 NRCS Rangeland Site Ecological Site Description Database
#> 503 NRCS Rangeland Site Ecological Site Description Database
#> 504 NRCS Rangeland Site Ecological Site Description Database
#> 505 NRCS Rangeland Site Ecological Site Description Database
#> 506 NRCS Rangeland Site Ecological Site Description Database
#> 507 NRCS Rangeland Site Ecological Site Description Database
#> 508 NRCS Rangeland Site Ecological Site Description Database
#> 509 NRCS Rangeland Site Ecological Site Description Database
#> 510 NRCS Rangeland Site Ecological Site Description Database
#> 511 NRCS Rangeland Site Ecological Site Description Database
#> 512 NRCS Rangeland Site Ecological Site Description Database
#> 513 NRCS Rangeland Site Ecological Site Description Database
#> 514 NRCS Rangeland Site Ecological Site Description Database
#> 515 NRCS Rangeland Site Ecological Site Description Database
#> 516 NRCS Rangeland Site Ecological Site Description Database
#> 517 NRCS Rangeland Site Ecological Site Description Database
#> 518 NRCS Rangeland Site Ecological Site Description Database
#> 519 NRCS Rangeland Site Ecological Site Description Database
#> 520 NRCS Rangeland Site Ecological Site Description Database
#> 521 NRCS Rangeland Site Ecological Site Description Database
#> 522 NRCS Rangeland Site Ecological Site Description Database
#> 523 NRCS Rangeland Site Ecological Site Description Database
#> 524 NRCS Rangeland Site Ecological Site Description Database
#> 525 NRCS Rangeland Site Ecological Site Description Database
#> 526 NRCS Rangeland Site Ecological Site Description Database
#> 527 NRCS Rangeland Site Ecological Site Description Database
#> 528 NRCS Rangeland Site Ecological Site Description Database
#> 529 NRCS Rangeland Site Ecological Site Description Database
#> 530 NRCS Rangeland Site Ecological Site Description Database
#> 531 NRCS Rangeland Site Ecological Site Description Database
#> 532 NRCS Rangeland Site Ecological Site Description Database
#> 533 NRCS Rangeland Site Ecological Site Description Database
#> 534 NRCS Rangeland Site Ecological Site Description Database
#> 535 NRCS Rangeland Site Ecological Site Description Database
#> 536 NRCS Rangeland Site Ecological Site Description Database
#> 537 NRCS Rangeland Site Ecological Site Description Database
#> 538 NRCS Rangeland Site Ecological Site Description Database
#> 539 NRCS Rangeland Site Ecological Site Description Database
#> 540 NRCS Rangeland Site Ecological Site Description Database
#> 541 NRCS Rangeland Site Ecological Site Description Database
#> 542 NRCS Rangeland Site Ecological Site Description Database
#> 543 NRCS Rangeland Site Ecological Site Description Database
#> 544 NRCS Rangeland Site Ecological Site Description Database
#> 545 NRCS Rangeland Site Ecological Site Description Database
#> 546 NRCS Rangeland Site Ecological Site Description Database
#> 547 NRCS Rangeland Site Ecological Site Description Database
#> 548 NRCS Rangeland Site Ecological Site Description Database
#> 549 NRCS Rangeland Site Ecological Site Description Database
#> 550 NRCS Rangeland Site Ecological Site Description Database
#> 551 NRCS Rangeland Site Ecological Site Description Database
#> 552 NRCS Rangeland Site Ecological Site Description Database
#> 553 NRCS Rangeland Site Ecological Site Description Database
#> 554 NRCS Rangeland Site Ecological Site Description Database
#> 555 NRCS Rangeland Site Ecological Site Description Database
#> 556 NRCS Rangeland Site Ecological Site Description Database
#> 557 NRCS Rangeland Site Ecological Site Description Database
#> 558 NRCS Rangeland Site Ecological Site Description Database
#> 559 NRCS Rangeland Site Ecological Site Description Database
#> 560 NRCS Rangeland Site Ecological Site Description Database
#> 561 NRCS Rangeland Site Ecological Site Description Database
#> 562 NRCS Rangeland Site Ecological Site Description Database
#> 563 NRCS Rangeland Site Ecological Site Description Database
#> 564 NRCS Rangeland Site Ecological Site Description Database
#> 565 NRCS Rangeland Site Ecological Site Description Database
#> 566 NRCS Rangeland Site Ecological Site Description Database
#> 567 NRCS Rangeland Site Ecological Site Description Database
#> 568 NRCS Rangeland Site Ecological Site Description Database
#> 569 NRCS Rangeland Site Ecological Site Description Database
#> 570 NRCS Rangeland Site Ecological Site Description Database
#> 571 NRCS Rangeland Site Ecological Site Description Database
#> 572 NRCS Rangeland Site Ecological Site Description Database
#> 573 NRCS Rangeland Site Ecological Site Description Database
#> 574 NRCS Rangeland Site Ecological Site Description Database
#> 575 NRCS Rangeland Site Ecological Site Description Database
#> 576 NRCS Rangeland Site Ecological Site Description Database
#> 577 NRCS Rangeland Site Ecological Site Description Database
#> 578 NRCS Rangeland Site Ecological Site Description Database
#> 579 NRCS Rangeland Site Ecological Site Description Database
#> 580 NRCS Rangeland Site Ecological Site Description Database
#> 581 NRCS Rangeland Site Ecological Site Description Database
#> 582 NRCS Rangeland Site Ecological Site Description Database
#> 583 NRCS Rangeland Site Ecological Site Description Database
#> 584 NRCS Rangeland Site Ecological Site Description Database
#> 585 NRCS Rangeland Site Ecological Site Description Database
#> 586 NRCS Rangeland Site Ecological Site Description Database
#> 587 NRCS Rangeland Site Ecological Site Description Database
#> 588 NRCS Rangeland Site Ecological Site Description Database
#> 589 NRCS Rangeland Site Ecological Site Description Database
#> 590 NRCS Rangeland Site Ecological Site Description Database
#> 591 NRCS Rangeland Site Ecological Site Description Database
#> 592 NRCS Rangeland Site Ecological Site Description Database
#> 593 NRCS Rangeland Site Ecological Site Description Database
#> 594 NRCS Rangeland Site Ecological Site Description Database
#> 595 NRCS Rangeland Site Ecological Site Description Database
#> 596 NRCS Rangeland Site Ecological Site Description Database
#> 597 NRCS Rangeland Site Ecological Site Description Database
#> 598 NRCS Rangeland Site Ecological Site Description Database
#> 599 NRCS Rangeland Site Ecological Site Description Database
#> 600 NRCS Rangeland Site Ecological Site Description Database
#> 601 NRCS Rangeland Site Ecological Site Description Database
#> 602 NRCS Rangeland Site Ecological Site Description Database
#> 603 NRCS Rangeland Site Ecological Site Description Database
#> 604 NRCS Rangeland Site Ecological Site Description Database
#> 605 NRCS Rangeland Site Ecological Site Description Database
#> 606 NRCS Rangeland Site Ecological Site Description Database
#> 607 NRCS Rangeland Site Ecological Site Description Database
#> 608 NRCS Rangeland Site Ecological Site Description Database
#> 609 NRCS Rangeland Site Ecological Site Description Database
#> 610 NRCS Rangeland Site Ecological Site Description Database
#> 611 NRCS Rangeland Site Ecological Site Description Database
#> 612 NRCS Rangeland Site Ecological Site Description Database
#> 613 NRCS Rangeland Site Ecological Site Description Database
#> 614 NRCS Rangeland Site Ecological Site Description Database
#> 615 NRCS Rangeland Site Ecological Site Description Database
#> 616 NRCS Rangeland Site Ecological Site Description Database
#> 617 NRCS Rangeland Site Ecological Site Description Database
#> 618 NRCS Rangeland Site Ecological Site Description Database
#> 619 NRCS Rangeland Site Ecological Site Description Database
#> 620 NRCS Rangeland Site Ecological Site Description Database
#> 621 NRCS Rangeland Site Ecological Site Description Database
#> 622 NRCS Rangeland Site Ecological Site Description Database
#> 623 NRCS Rangeland Site Ecological Site Description Database
#> 624 NRCS Rangeland Site Ecological Site Description Database
#> 625 NRCS Rangeland Site Ecological Site Description Database
#> 626 NRCS Rangeland Site Ecological Site Description Database
#> 627 NRCS Rangeland Site Ecological Site Description Database
#> 628 NRCS Rangeland Site Ecological Site Description Database
#> 629 NRCS Rangeland Site Ecological Site Description Database
#> 630 NRCS Rangeland Site Ecological Site Description Database
#> 631 NRCS Rangeland Site Ecological Site Description Database
#> 632 NRCS Rangeland Site Ecological Site Description Database
#> 633 NRCS Rangeland Site Ecological Site Description Database
#> 634 NRCS Rangeland Site Ecological Site Description Database
#> 635 NRCS Rangeland Site Ecological Site Description Database
#> 636 NRCS Rangeland Site Ecological Site Description Database
#> 637 NRCS Rangeland Site Ecological Site Description Database
#> 638 NRCS Rangeland Site Ecological Site Description Database
#> 639 NRCS Rangeland Site Ecological Site Description Database
#> 640 NRCS Rangeland Site Ecological Site Description Database
#> 641 NRCS Rangeland Site Ecological Site Description Database
#> 642 NRCS Rangeland Site Ecological Site Description Database
#> 643 NRCS Rangeland Site Ecological Site Description Database
#> 644 NRCS Rangeland Site Ecological Site Description Database
#> 645 NRCS Rangeland Site Ecological Site Description Database
#> 646 NRCS Rangeland Site Ecological Site Description Database
#> 647 NRCS Rangeland Site Ecological Site Description Database
#> 648 NRCS Rangeland Site Ecological Site Description Database
#> 649 NRCS Rangeland Site Ecological Site Description Database
#> 650 NRCS Rangeland Site Ecological Site Description Database
#> 651 NRCS Rangeland Site Ecological Site Description Database
#> 652 NRCS Rangeland Site Ecological Site Description Database
#> 653 NRCS Rangeland Site Ecological Site Description Database
#> 654 NRCS Rangeland Site Ecological Site Description Database
#> 655 NRCS Rangeland Site Ecological Site Description Database
#> 656 NRCS Rangeland Site Ecological Site Description Database
#> 657 NRCS Rangeland Site Ecological Site Description Database
#> 658 NRCS Rangeland Site Ecological Site Description Database
#> 659 NRCS Rangeland Site Ecological Site Description Database
#> 660 NRCS Rangeland Site Ecological Site Description Database
#> 661 NRCS Rangeland Site Ecological Site Description Database
#> 662 NRCS Rangeland Site Ecological Site Description Database
#> 663 NRCS Rangeland Site Ecological Site Description Database
#> 664 NRCS Rangeland Site Ecological Site Description Database
#> 665 NRCS Rangeland Site Ecological Site Description Database
#> 666 NRCS Rangeland Site Ecological Site Description Database
#> 667 NRCS Rangeland Site Ecological Site Description Database
#> 668 NRCS Rangeland Site Ecological Site Description Database
#> 669 NRCS Rangeland Site Ecological Site Description Database
#> 670 NRCS Rangeland Site Ecological Site Description Database
#> 671 NRCS Rangeland Site Ecological Site Description Database
#> 672 NRCS Rangeland Site Ecological Site Description Database
#> 673 NRCS Rangeland Site Ecological Site Description Database
#> 674 NRCS Rangeland Site Ecological Site Description Database
#> 675 NRCS Rangeland Site Ecological Site Description Database
#> 676 NRCS Rangeland Site Ecological Site Description Database
#> 677 NRCS Rangeland Site Ecological Site Description Database
#> 678 NRCS Rangeland Site Ecological Site Description Database
#> 679 NRCS Rangeland Site Ecological Site Description Database
#> 680 NRCS Rangeland Site Ecological Site Description Database
#> 681 NRCS Rangeland Site Ecological Site Description Database
#> 682 NRCS Rangeland Site Ecological Site Description Database
#> 683 NRCS Rangeland Site Ecological Site Description Database
#> 684 NRCS Rangeland Site Ecological Site Description Database
#> 685 NRCS Rangeland Site Ecological Site Description Database
#> 686 NRCS Rangeland Site Ecological Site Description Database
#> 687 NRCS Rangeland Site Ecological Site Description Database
#> 688 NRCS Rangeland Site Ecological Site Description Database
#> 689 NRCS Rangeland Site Ecological Site Description Database
#> 690 NRCS Rangeland Site Ecological Site Description Database
#> 691 NRCS Rangeland Site Ecological Site Description Database
#> 692 NRCS Rangeland Site Ecological Site Description Database
#> 693 NRCS Rangeland Site Ecological Site Description Database
#> 694 NRCS Rangeland Site Ecological Site Description Database
#> 695 NRCS Rangeland Site Ecological Site Description Database
#> 696 NRCS Rangeland Site Ecological Site Description Database
#> 697 NRCS Rangeland Site Ecological Site Description Database
#> 698 NRCS Rangeland Site Ecological Site Description Database
#> 699 NRCS Rangeland Site Ecological Site Description Database
#> 700 NRCS Rangeland Site Ecological Site Description Database
#> 701 NRCS Rangeland Site Ecological Site Description Database
#> 702 NRCS Rangeland Site Ecological Site Description Database
#> 703 NRCS Rangeland Site Ecological Site Description Database
#> 704 NRCS Rangeland Site Ecological Site Description Database
#> 705 NRCS Rangeland Site Ecological Site Description Database
#> 706 NRCS Rangeland Site Ecological Site Description Database
#> 707 NRCS Rangeland Site Ecological Site Description Database
#> 708 NRCS Rangeland Site Ecological Site Description Database
#> 709 NRCS Rangeland Site Ecological Site Description Database
#> 710 NRCS Rangeland Site Ecological Site Description Database
#> 711 NRCS Rangeland Site Ecological Site Description Database
#> 712 NRCS Rangeland Site Ecological Site Description Database
#> 713 NRCS Rangeland Site Ecological Site Description Database
#> 714 NRCS Rangeland Site Ecological Site Description Database
#> 715 NRCS Rangeland Site Ecological Site Description Database
#> 716 NRCS Rangeland Site Ecological Site Description Database
#> 717 NRCS Rangeland Site Ecological Site Description Database
#> 718 NRCS Rangeland Site Ecological Site Description Database
#> 719 NRCS Rangeland Site Ecological Site Description Database
#> 720 NRCS Rangeland Site Ecological Site Description Database
#> 721 NRCS Rangeland Site Ecological Site Description Database
#> 722 NRCS Rangeland Site Ecological Site Description Database
#> 723 NRCS Rangeland Site Ecological Site Description Database
#> 724 NRCS Rangeland Site Ecological Site Description Database
#> 725 NRCS Rangeland Site Ecological Site Description Database
#> 726 NRCS Rangeland Site Ecological Site Description Database
#> 727 NRCS Rangeland Site Ecological Site Description Database
#> 728 NRCS Rangeland Site Ecological Site Description Database
#> 729 NRCS Rangeland Site Ecological Site Description Database
#> 730 NRCS Rangeland Site Ecological Site Description Database
#> 731 NRCS Rangeland Site Ecological Site Description Database
#> 732 NRCS Rangeland Site Ecological Site Description Database
#> 733 NRCS Rangeland Site Ecological Site Description Database
#> 734 NRCS Rangeland Site Ecological Site Description Database
#> 735 NRCS Rangeland Site Ecological Site Description Database
#> 736 NRCS Rangeland Site Ecological Site Description Database
#> 737 NRCS Rangeland Site Ecological Site Description Database
#> 738 NRCS Rangeland Site Ecological Site Description Database
#> 739 NRCS Rangeland Site Ecological Site Description Database
#> 740 NRCS Rangeland Site Ecological Site Description Database
#> 741 NRCS Rangeland Site Ecological Site Description Database
#> 742 NRCS Rangeland Site Ecological Site Description Database
#> 743 NRCS Rangeland Site Ecological Site Description Database
#> 744 NRCS Rangeland Site Ecological Site Description Database
#> 745 NRCS Rangeland Site Ecological Site Description Database
#> 746 NRCS Rangeland Site Ecological Site Description Database
```

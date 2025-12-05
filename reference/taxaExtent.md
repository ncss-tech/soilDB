# Soil Taxonomy Grids via SoilWeb

This function downloads gridded, generalized representations of the
geographic extent for several types of taxonomic information. Options
include: a single taxon from the top four levels of Soil Taxonomy,
formative elements used in great group or subgroup taxa, or family
mineralogy class. Data are provided by SoilWeb, ultimately sourced from
the current SSURGO snapshot. Data are returned as `SpatRaster` objects
representing soil map unit area proportion falling within 800m cells.
Area proportions are based on major component percentages only. Data are
available in CONUS and returned using an Albers Equal Area / NAD83(2011)
coordinate reference system (EPSG: 5070).

## Usage

``` r
taxaExtent(
  x,
  type = c("taxon", "formative element", "mineralogy class"),
  level = c("order", "suborder", "greatgroup", "subgroup"),
  formativeElement = NULL,
  timeout = 60,
  as_Spatial = getOption("soilDB.return_Spatial", default = FALSE)
)
```

## Arguments

- x:

  single taxon label (e.g. `haploxeralfs`), formative element (e.g.
  `pale`), or family mineralogy class (see Details)

- type:

  query type: `taxon`, `formative element`, `mineralogy class`

- level:

  the taxonomic level within the top 4 tiers of Soil Taxonomy, one of
  `'order'`, `'suborder'`, `'greatgroup'`, `'subgroup'`

- formativeElement:

  deprecated, see `type` argument

- timeout:

  time that we are willing to wait for a response, in seconds

- as_Spatial:

  Return raster (`RasterLayer`) classes? Default: `FALSE`.

## Value

a `SpatRaster` object (or `RasterLayer` when `as_Spatial = TRUE`)

## Details

See the [Geographic Extent of Soil
Taxa](https://ncss-tech.github.io/AQP/soilDB/taxa-extent.html) tutorial
for more detailed examples. The [Keys to Soil
Taxonomy](https://www.nrcs.usda.gov/sites/default/files/2022-09/Keys-to-Soil-Taxonomy.pdf)
is the authoritative resource for definitions of taxa, formative
elements, and family mineralogy classes.

### Taxon Queries

Taxon labels can be conveniently extracted from the `"ST_unique_list"`
sample data, provided by the [SoilTaxonomy
package](https://github.com/ncss-tech/SoilTaxonomy).

### Formative Element Queries

#### Greatgroup:

The following labels are used to access taxa containing the following
formative elements (in parentheses)

- acr: (acro/acr) extreme weathering

- alb: (alb) presence of an albic horizon

- anhy: (anhy) very dry

- anthra: (anthra) presence of an anthropic epipedon

- aqu: (aqui/aqu) wetness

- argi: (argi) presence of an argillic horizon

- calci: (calci) presence of a calcic horizon

- cry: (cryo/cry) cryic STR

- dur: (duri/dur) presence of a duripan

- dystr: (dystro/dystr) low base saturation

- endo: (endo) ground water table

- epi: (epi) perched water table

- eutr: (eutro/eutr) high base saturation

- ferr: (ferr) presence of Fe

- fibr: (fibr) least decomposed stage

- fluv: (fluv) flood plain

- fol: (fol) mass of leaves

- fragi: (fragi) presence of a fragipan

- fragloss: (fragloss) presence of a fragipan and glossic horizon

- frasi: (frasi) not salty

- fulv: (fulvi/fulv) dark brown with organic carbon

- glac: (glac) presence of ice lenses

- gloss: (glosso/gloss) presence of a glossic horizon

- gypsi: (gypsi) presence of a gypsic horizon

- hal: (hal) salty

- hemi: (hemi) intermediate decomposition

- hist: (histo/hist) organic soil material

- hum: (humi/hum) presence of organic carbon

- hydr: (hydro/hydr) presence of water

- kandi: (kandi) presence of a kandic horizon

- kanhap: (kanhaplo/kanhap) thin kandic horizon

- luvi: (luvi) illuvial organic material

- melan: (melano/melan) presence of a melanic epipedon

- moll: (molli/moll) presence of a mollic epipedon

- natr: (natri/natr) presence of a natric horizon

- pale: (pale) excessive development

- petr: (petro/petr) petrocalcic horizon

- plac: (plac) presence of a thin pan

- plagg: (plagg) presence of a plaggen epipedon

- plinth: (plinth) presence of plinthite

- psamm: (psammo/psamm) sandy texture

- quartzi: (quartzi) high quartz content

- rhod: (rhodo/rhod) dark red colors

- sal: (sali/sal) presence of a salic horizon

- sapr: (sapr) most decomposed stage

- sombri: (sombri) presence of a sombric horizon

- sphagno: (sphagno) presence of sphagnum moss

- sulf: (sulfo/sulfi/sulf) presence of sulfides or their oxidation
  products

- torri: (torri) torric/aridic SMR

- ud: (udi/ud) udic SMR

- umbr: (umbri/umbr) presence of an umbric epipedon

- ust: (usti/ust) ustic SMR

- verm: (verm) wormy, or mixed by animals

- vitr: (vitri/vitr) presence of glass

- xer: (xero/xer) xeric SMR

#### Subgroup:

The following labels are used to access taxa containing the following
formative elements (in parenthesis).

- abruptic: (abruptic) abrupt textural change

- acric: (acric) low apparent CEC

- aeric: (aeric) more aeration than typic subgroup

- albaquic: (albaquic) presence of albic minerals, wetter than typic
  subgroup

- albic: (albic) presence of albic minerals

- alfic: (alfic) presence of an argillic or kandic horizon

- alic: (alic) high extractable Al content

- anionic: (anionic) low CEC or positively charged

- anthraquic: (anthraquic) human controlled flooding as in paddy rice
  culture

- anthropic: (anthropic) an anthropic epipedon

- aquic: (aquic) wetter than typic subgroup

- arenic: (arenic) 50-100cm sandy textured surface

- argic: (argic) argillic horizon

- aridic: (aridic) more aridic than typic subgroup

- calcic: (calcic) presence of a calcic horizon

- chromic: (chromic) high chroma colors

- cumulic: (cumulic) thickened epipedon

- duric: (duric) presence of a duripan

- durinodic: (durinodic) presence of durinodes

- dystric: (dystric) lower base saturation percentage

- entic: (entic) minimal surface/subsurface development

- eutric: (eutric) higher base saturation percentage

- fibric: (fibric) \>25cm of fibric material

- fluvaquentic: (fluvaquentic) wetter than typic subgroup, evidence of
  stratification

- fragiaquic: (fragiaquic) presence of fragic properties, wetter than
  typic subgroup

- fragic: (fragic) presence of fragic properties

- glacic: (glacic) presence of ice lenses or wedges

- glossaquic: (glossaquic) interfingered horizon boundaries, wetter than
  typic subgroup

- glossic: (glossic) interfingered horizon boundaries

- grossarenic: (grossarenic) \>100cm sandy textured surface

- gypsic: (gypsic) presence of gypsic horizon

- halic: (halic) salty

- haplic: (haplic) central theme of subgroup concept

- hemic: (hemic) \>25cm of hemic organic material

- humic: (humic) higher organic matter content

- hydric: (hydric) presence of water

- kandic: (kandic) low activity clay present

- lamellic: (lamellic) presence of lamellae

- leptic: (leptic) thinner than typic subgroup

- limnic: (limnic) presence of a limnic layer

- lithic: (lithic) shallow lithic contact present

- natric: (natric) presence of sodium

- nitric: (nitric) presence of nitrate salts

- ombroaquic: (ombroaquic) surface wetness

- oxyaquic: (oxyaquic) water saturated but not reduced

- pachic: (pachic) epipedon thicker than typic subgroup

- petrocalcic: (petrocalcic) presence of a petrocalcic horizon

- petroferric: (petroferric) presence of petroferric contact

- petrogypsic: (petrogypsic) presence of a petrogypsic horizon

- petronodic: (petronodic) presence of concretions and/or nodules

- placic: (placic) presence of a placic horizon

- plinthic: (plinthic) presence of plinthite

- rhodic: (rhodic) darker red colors than typic subgroup

- ruptic: (ruptic) intermittent horizon

- salic: (salic) presence of a salic horizon

- sapric: (sapric) \>25cm of sapric organic material

- sodic: (sodic) high exchangeable Na content

- sombric: (sombric) presence of a sombric horizon

- sphagnic: (sphagnic) sphagnum organic material

- sulfic: (sulfic) presence of sulfides

- terric: (terric) mineral substratum within 1 meter

- thapto: (thaptic/thapto) presence of a buried soil horizon

- turbic: (turbic) evidence of cryoturbation

- udic: (udic) more humid than typic subgroup

- umbric: (umbric) presence of an umbric epipedon

- ustic: (ustic) more ustic than typic subgroup

- vermic: (vermic) animal mixed material

- vitric: (vitric) presence of glassy material

- xanthic: (xanthic) more yellow than typic subgroup

- xeric: (xeric) more xeric than typic subgroup

### Family Mineralogy Class Queries

The family mineralogy class grids hosted by SoilWeb represent a
simplification; major components with highly contrasting particle size
classes (and thus multiple mineralogy classes) will match each class
present in the family classification. For example, major components with
'glassy over mixed' and 'glassy' family mineralogy classes are included
in the same extent grid. The 'mixed' mineralogy class is not currently
included.

The following simplified mineralogy classes are available. Descriptions
have been condensed for brevity. See chapter 17 from Keys to Soil
Taxonomy for full descriptions.

- allitic: 18 to 40 percent (by weight) gibbsite in the fine-earth
  fraction

- amorphic: a. A sum of 8 times the Si (percent by weight extracted by
  ammonium oxalate from the fine-earth fraction) plus 2 times the Fe
  (percent by weight extracted by ammonium oxalate from the fine-earth
  fraction) of 5 or more; and, The product of 8 times the Si is more
  than the product of 2 times the Fe.

- anhydritic: Any particle-size class and 15 percent or more (by weight)
  anhydrite, either in the fine-earth fraction or in the fraction less
  than 20 mm in diameter, whichever has a higher percentage of
  anhydrite.

- carbonatic: Any particle-size class and more than 40 percent (by
  weight) carbonates (expressed as CaCO3 ) plus gypsum, either in the
  fine-earth fraction or in the fraction less than 20 mm in diameter,
  whichever has a higher percentage of carbonates plus gypsum

- diatomaceous (used for organic soils with limnic materials)

- ferrihydritic: A sum of 8 times the Si (percent by weight extracted by
  ammonium oxalate from the fine-earth fraction) plus 2 times the Fe
  (percent by weight extracted by ammonium oxalate from the fine-earth
  fraction) of 5 or more.

- ferritic: More than 40 percent (by weight) iron oxide as Fe2O3 (more
  than 28 percent Fe), extractable by dithionite-citrate, in the
  fine-earth fraction.

- ferruginous: 18 to 40 percent (by weight) iron oxide as Fe2O3 (12.6 to
  28 percent Fe), extractable by dithionite-citrate, in the fineearth
  fraction.

- gibbsitic: More than 40 percent (by weight) gibbsite in the fine earth
  fraction.

- glassy: 30 percent or more (by grain count) volcanic glass in the 0.02
  to 2.0 mm fraction.

- glauconitic: 30 percent or more (by grain count) volcanic glass in the
  0.02 to 2.0 mm fraction.

- gypsic: Any particle-size class and 15 percent or more (by weight)
  gypsum, either in the fine-earth fraction or in the fraction less than
  20 mm in diameter, whichever has a higher percentage of gypsum.

- halloysitic: Have more than 50 percent (by weight) halloysite plus
  kaolinite and allophane and more halloysite than any other single kind
  of clay mineral.

- hypergypsic: 40 percent or more (by weight) gypsum either in the
  fine-earth fraction or in the fraction less than 20 mm in diameter,
  whichever has a higher percentage of gypsum.

- illitic: Have more than 50 percent (by weight) illite (hydrous mica)
  and commonly more than 4 percent K2O.

- isotic: In more than one-half of the thickness, all of the
  following: a. No free carbonates; and, b. A sodium fluoride pH (NaF
  pH) of 8.4 or more; and, c. A ratio of 1500 kPa water to measured clay
  of 0.6 or more.

- kaolinitic: More than 50 percent (by weight) kaolinite plus
  halloysite, dickite, nacrite, and other 1:1 or nonexpanding 2:1 layer
  minerals and gibbsite and less than 10 percent (by weight) smectite
  minerals (montmorillonite, beidellite, and nontronite) in the fraction
  less than 0.002 mm in diameter, and more kaolinite than halloysite.

- magnesic: Any particle-size class, except for fragmental, and more
  than 40 percent (by weight) magnesium-silicate minerals, such as the
  serpentine minerals (antigorite, chrysotile, and lizardite) plus talc,
  olivines, Mg-rich pyroxenes, and Mg-rich amphiboles, in the fine-earth
  fraction.

- micaceous: More than 45 percent (by grain count) mica and stable mica
  pseudomorphs in the 0.02 to 0.25 mm fraction.

- opaline: 30 percent or more (by weight) diatoms, plant opal, and
  sponge spicules in the fine-earth fraction.

- parasesquic: A total percent (by weight) iron oxide as Fe2O3 (percent
  Fe extractable by dithionite-citrate times 1.43) plus the percent (by
  weight) gibbsite of more than 10 in the fine-earth fraction.

- sesquic: 18 to 40 percent (by weight) iron oxide as Fe2O3 (12.6 to 28
  percent Fe), extractable by dithionite-citrate, in the fine-earth
  fraction; and, 18 to 40 percent (by weight) gibbsite in the fine-earth
  fraction.

- siliceous: More than 90 percent (by weight or grain count) silica
  minerals (quartz, chalcedony, or opal) and other resistant minerals in
  the 0.02 to 2.0 mm fraction.

- smectitic: Have more smectite minerals (montmorillonite, beidellite,
  and nontronite), by weight, than any other single kind of clay
  mineral.

- vermiculitic: Have more vermiculite than any other single kind of clay
  mineral.

## Author

D.E. Beaudette and A.G. Brown

## Examples

``` r
if (FALSE) { # \dontrun{
  library(terra)
  
  # soil order
  taxa <- 'vertisols'
  x <- taxaExtent(taxa, level = 'order')
  
  # suborder
  taxa <- 'ustalfs'
  x <- taxaExtent(taxa, level = 'suborder')
  
  # greatgroup
  taxa <- 'haplohumults'
  x <- taxaExtent(taxa, level = 'greatgroup')
  
  # subgroup
  taxa <- 'Typic Haploxerepts'
  x <- taxaExtent(taxa, level = 'subgroup')
  
  # greatgroup formative element
  taxa <- 'psamm'
  x <- taxaExtent(taxa, level = 'greatgroup', type = 'formative element')
  
  # subgroup formative element
  taxa <- 'abruptic'
  x <- taxaExtent(taxa, level = 'subgroup', type = 'formative element')
  
  # 'glassy' family mineralogy class 
  x <- taxaExtent('glassy', type = 'mineralogy class')
  
  # coarsen for faster plotting
  a <- terra::aggregate(x, fact = 5, na.rm = TRUE)
  
  # quick evaluation of the result
  terra::plot(a, axes = FALSE)
} # }
```

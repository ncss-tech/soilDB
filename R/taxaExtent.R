
#' @title Retrieve Soil Taxonomy Membership Grids
#' 
#' @description This function downloads a generalized representation of the geographic extent of any single taxon from the top 4 tiers of Soil Taxonomy, or those taxa matching a given formative element used in greatgroup or subgroup taxa. Data are provided by SoilWeb, ultimately sourced from from the current SSURGO snapshot. Data are returned as \code{raster} objects representing area proportion falling within 800m cells. Currently area proportions are based on major components only. Data are only available in CONUS and returned using an Albers Equal Area / NAD83 coordinate reference system (EPSG 6350).
#' 
#' @param x single taxon label (e.g. `haploxeralfs`) or formative element (e.g. `pale`), case-insensitive
#' 
#' @param level the taxonomic level within the top 4 tiers of Soil Taxonomy, one of \code{c('order', 'suborder', 'greatgroup', 'subgroup')}
#' 
#' @param formativeElement logical, search using formative elements instead of taxon label
#' 
#' @param timeout time that we are willing to wait for a response, in seconds
#' 
#' @return a \code{raster} object
#' 
#' @author D.E. Beaudette and Andrew Brown
#' 
#' @details 
#' 
#' ## Taxon Queries
#' 
#' Taxon labels can be conveniently extracted from the "ST_unique_list" sample data, provided by the [SoilTaxonomy package](https://github.com/ncss-tech/SoilTaxonomy).
#' 
#' 
#' 
#' 
#' ## Formative Element Queries
#' 
#' ### Greatgroup:
#' * acro: extreme weathering
#' * alb: presence of an albic horizon
#' * anhy: very dry
#' * anthra: presence of an anthropic epipedon
#' * aqui: wetness
#' * argi: presence of an argillic horizon
#' * calci: presence of a calcic horizon
#' * cryo: cryic STR
#' * duri: presence of a duripan
#' * dystro: low base saturation
#' * endo: ground water table
#' * epi: perched water table
#' * eutro: high base saturation
#' * ferr: presence of Fe
#' * fibr: least decomposed stage
#' * fluv: flood plain
#' * fol: mass of leaves
#' * fragi: presence of a fragipan
#' * fragloss: presence of a fragipan and glossic horizon
#' * frasi: not salty
#' * fulvi: dark brown with organic carbon
#' * glac: presence of ice lenses
#' * glosso: presence of a glossic horizon
#' * gypsi: presence of a gypsic horizon
#' * hal: salty
#' * hapl: minimum horizon development
#' * hemi: intermediate decomposition
#' * histo: organic soil material
#' * humi: presence of organic carbon
#' * hydro: presence of water
#' * kandi: presence of a kandic horizon
#' * kanhaplo: thin kandic horizon
#' * luvi: illuvial organic material
#' * melano: presence of a melanic epipedon
#' * molli: presence of a mollic epipedon
#' * natri: presence of a natric horizon
#' * pale: excessive development
#' * petro: petrocalcic horizon
#' * plac: presence of a thin pan
#' * plagg: presence of a plaggen epipedon
#' * plinth: presence of plinthite
#' * psammo: sandy texture
#' * quartzi: high quartz content
#' * rhodo: dark red colors
#' * sali: presence of a salic horizon
#' * sapr: most decomposed stage
#' * sombri: presence of a sombric horizon
#' * sphagno: presence of sphagnum moss
#' * sulfo: presence of sulfides or their oxidation products
#' * torri: torric/aridic SMR
#' * udi: udic SMR
#' * umbri: presence of an umbric epipedon
#' * usti: ustic SMR
#' * verm: wormy, or mixed by animals
#' * vitri: presence of glass
#' * xero: xeric SMR
#' 
#' 
#' ### Subgroup:
#' * abruptic: abrupt textural change
#' * acric: low apparent CEC
#' * aeric: more aeration than typic subgroup
#' * albaquic: presence of albic minerals, wetter than typic subgroup
#' * albic: presence of albic minerals
#' * alfic: presence of an argillic or kandic horizon
#' * alic: high extractable Al content
#' * anionic: low CEC or positively charged
#' * anthraquic: human controlled flooding as in paddy rice culture
#' * anthropic: an anthropic epipedon
#' * aquic: wetter than typic subgroup
#' * arenic: 50-100cm sandy textured surface
#' * argic: argillic horizon
#' * aridic: more aridic than typic subgroup
#' * calcic: presence of a calcic horizon
#' * chromic: high chroma colors
#' * cumulic: thickened epipedon
#' * duric: presence of a duripan
#' * durinodic: presence of durinodes
#' * dystric: lower base saturation percentage
#' * entic: minimal surface/subsurface development
#' * eutric: higher base saturation percentage
#' * fibric: >25cm of fibric material
#' * fluvaquentic: wetter than typic subgroup, evidence of stratification
#' * fragiaquic: presence of fragic properties, wetter than typic subgroup
#' * fragic: presence of fragic properties
#' * glacic: presence of ice lenses or wedges
#' * glossaquic: interfingered horizon boundaries, wetter than typic subgroup
#' * glossic: interfingered horizon boundaries
#' * grossarenic: >100cm sandy textured surface
#' * gypsic: presence of gypsic horizon
#' * halic: salty
#' * haplic: central theme of subgroup concept
#' * hemic: >25cm of hemic organic material
#' * humic: higher organic matter content
#' * hydric: presence of water
#' * kandic: low activity clay present
#' * lamellic: presence of lamellae
#' * leptic: thinner than typic subgroup
#' * limnic: presence of a limnic layer
#' * lithic: shallow lithic contact present
#' * natric: presence of sodium
#' * nitric: presence of nitrate salts
#' * ombroaquic: surface wetness
#' * oxyaquic: water saturated but not reduced
#' * pachic: epipedon thicker than typic subgroup
#' * petrocalcic: presence of a petrocalcic horizon
#' * petroferric: presence of petroferric contact
#' * petrogypsic: presence of a petrogypsic horizon
#' * petronodic: presence of concretions and/or nodules
#' * placic: presence of a placic horizon
#' * plinthic: presence of plinthite
#' * rhodic: darker red colors than typic subgroup
#' * ruptic: intermittent horizon
#' * salic: presence of a salic horizon
#' * sapric: >25cm of sapric organic material
#' * sodic: high exchangeable Na content
#' * sombric: presence of a sombric horizon
#' * sphagnic: sphagnum organic material
#' * sulfic: presence of sulfides
#' * terric: mineral substratum within 1 meter
#' * thaptic: presence of a buried soil horizon
#' * turbic: evidence of cryoturbation
#' * udic: more humid than typic subgroup
#' * umbric: presence of an umbric epipedon
#' * ustic: more ustic than typic subgroup
#' * vermic: animal mixed material
#' * vitric: presence of glassy material
#' * xanthic: more yellow than typic subgroup
#' * xeric: more xeric than typic subgroup
#' 
#' @examples 
#' \donttest{
#' 
#' if(requireNamespace("curl") &
#'    curl::has_internet()) {
#'   
#'   library(raster)
#'   
#'   # try a couple of different examples
#'   
#'   # soil order
#'   taxa <- 'vertisols'
#'   x <- taxaExtent(taxa, level = 'order')
#'   a <- raster::aggregate(x, fact = 5)
#'   
#'   # suborder
#'   taxa <- 'ustalfs'
#'   x <- taxaExtent(taxa, level = 'suborder')
#'   a <- raster::aggregate(x, fact = 5)
#'   
#'   # greatgroup
#'   taxa <- 'haplohumults'
#'   x <- taxaExtent(taxa, level = 'greatgroup')
#'   a <- raster::aggregate(x, fact = 5)
#'   
#'   # subgroup
#'   taxa <- 'Typic Haploxerepts'
#'   x <- taxaExtent(taxa, level = 'subgroup')
#'   a <- raster::aggregate(x, fact = 5)
#'   
#'   # quick evaluation of the result
#'   if(requireNamespace("rasterVis") & requireNamespace('viridisLite')) {
#'     rasterVis::levelplot(a, 
#'       margin = FALSE, scales = list(draw = FALSE), 
#'       col.regions = viridisLite::viridis, 
#'       main = names(a)
#'     )
#'   }
#'   
#'   # slippy map
#'   if(requireNamespace("mapview")) {
#'     mapview::mapview(a, col.regions = viridisLite::viridis, na.color = NA, use.layer.names = TRUE)
#'   }
#'   
#'   
#'   
#' }
#' 
#' }
#' 
taxaExtent <- function(x, level = c('order', 'suborder', 'greatgroup', 'subgroup'), formativeElement = FALSE, timeout = 60) {
 
  ## sanity checks
  
  # legal levels
  level <- match.arg(level)
  
  # base URL
  base.url <- 'https://casoilresource.lawr.ucdavis.edu/taxa-grid-cache'
  
  # main branch
  # formative element query
  if(formativeElement) {
    
    # formative elements are only available at the greatgroup / subgroup for now
    if(level %in% c('order', 'suborder')) {
      stop('formative element queries are only supported for greatgroup and subgroup taxa', call. = FALSE)
    }
    
    # convert formative element level to path
    subdir <- switch(
      level,
      greatgroup = 'gg',
      subgroup = 'sg'
    )
    
    # full URL
    u <- URLencode(
      sprintf(
        '%s/fm/%s/%s.tif', 
        base.url,
        subdir, 
        x
      )
    )
    
  } else {
    # taxon query
    
    # encode taxa name: spaces -> underscores
    x <- gsub(pattern=' ', replacement='_', x = tolower(x), fixed = TRUE)
    
    # convert taxa level to path
    subdir <- switch(
      level,
      order = 'taxorder',
      suborder = 'taxsuborder',
      greatgroup = 'taxgrtgroup',
      subgroup = 'taxsubgrp'
    )
    
    # full URL
    u <- URLencode(
      sprintf(
        '%s/%s/%s.tif', 
        base.url,
        subdir, 
        x
      )
    )
    
  }
  
  
  
  # init temp files
  tf <- tempfile(fileext = '.tif')
  
  # safely download GeoTiff file
  # Mac / Linux: file automatically downloaded via binary transfer
  # Windows: must manually specify binary transfer
  res <- tryCatch(
    suppressWarnings(
      download.file(url = u, destfile = tf, extra = c(timeout = timeout), quiet = TRUE, mode = 'wb')
    ),
    error = function(e) {e}
  )
  
  # trap errors
  if(inherits(res, 'error')){
    message('no data returned')
    return(NULL)
  }
 
  ## TODO: suppressing CRS-related warnings (not a problem) until we have a better solution
  # https://github.com/ncss-tech/soilDB/issues/144
  # load raster object into memory
  r <- suppressWarnings(raster(tf, verbose=FALSE))
  r <- readAll(r)
  
  # transfer layer name
  # conversion of '_' -> ' ' only meaningful in taxon query
  names(r) <- gsub(pattern='_', replacement=' ', x = x, fixed = TRUE)
  
  # cleanup
  unlink(tf)
  
  # EPSG:6350
  return(r)
  
  return(res)
}


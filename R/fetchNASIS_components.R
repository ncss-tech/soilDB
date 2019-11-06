## TODO: better documentation for "fill" argument
# https://github.com/ncss-tech/soilDB/issues/50
## TODO: this will not ID horizons with no depths
## TODO: better error checking / reporting is needed: coiid, dmu id, component name
fetchNASIS_components <- function(SS=TRUE, rmHzErrors=TRUE, fill = FALSE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  # ensure that any old hz errors are cleared
  if(exists('component.hz.problems', envir=soilDB.env))
    assign('component.hz.problems', value=character(0), envir=soilDB.env)
  
  # load data in pieces
  f.comp       <- get_component_data_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
  f.chorizon   <- get_component_horizon_data_from_NASIS_db(SS=SS, fill=fill)
  f.copm       <- get_component_copm_data_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
  f.cogeomorph <- get_component_cogeomorph_data_from_NASIS_db(SS=SS)
  f.otherveg   <- get_component_otherveg_data_from_NASIS_db(SS=SS)
  f.ecosite    <- get_component_esd_data_from_NASIS_db(SS=SS, stringsAsFactors = stringsAsFactors)
  f.diaghz     <- get_component_diaghz_from_NASIS_db(SS=SS)

  # optionally test for bad horizonation... flag, and remove
  if(rmHzErrors & nrow(f.chorizon)) {
    f.chorizon.test <- plyr::ddply(f.chorizon, 'coiid', test_hz_logic, topcol='hzdept_r', bottomcol='hzdepb_r', strict=TRUE)

    # which are the good (valid) ones?
    good.ids <- as.character(f.chorizon.test$coiid[which(f.chorizon.test$hz_logic_pass)])
    bad.ids <- as.character(f.chorizon.test$coiid[which(! f.chorizon.test$hz_logic_pass)])

    # keep the good ones
    f.chorizon <- f.chorizon[which(f.chorizon$coiid %in% good.ids), ]

    # keep track of those components with horizonation errors
    #if(length(bad.ids) > 0) # AGB removed this line of code b/c it prevents update of 'component.hz.problems' on subsequent error-free calls
    assign('component.hz.problems', value=bad.ids, envir=soilDB.env)
  } 
  
  if(nrow(f.chorizon)) {
    # upgrade to SoilProfilecollection
    depths(f.chorizon) <- coiid ~ hzdept_r + hzdepb_r
  } else {
    stop("No horizon data in NASIS component query result.")
  }
  
  # add site data to object
  site(f.chorizon) <- f.comp # left-join via coiid

  # join-in copm strings
  ## 2017-3-13: short-circuts need testing, consider pre-marking mistakes before parsing
  pm <- plyr::ddply(f.copm, 'coiid', .formatcoParentMaterialString, name.sep=' & ')
  if(nrow(pm) > 0)
    site(f.chorizon) <- pm

  # join-in cogeomorph strings
  ## 2017-3-13: short-circuts need testing, consider pre-marking mistakes before parsing
  lf <- plyr::ddply(f.cogeomorph, 'coiid', .formatcoLandformString, name.sep=' & ')
  if(nrow(lf) > 0)
    site(f.chorizon) <- lf

  # join-in ecosite string
  ## 2017-3-06: short-circuts need testing, consider pre-marking mistakes before parsing
  es <- plyr::ddply(f.ecosite, 'coiid', .formatEcositeString, name.sep=' & ')
  if(nrow(es) > 0)
    site(f.chorizon) <- es

  # join-in othervegclass string
  ## 2017-3-06: short-circuts need testing, consider pre-marking mistakes before parsing
  ov <- plyr::ddply(f.otherveg, 'coiid', .formatOtherVegString, name.sep=' & ')
  if(nrow(ov) > 0)
    site(f.chorizon) <- ov

  # add diagnostic features to SPC
  diagnostic_hz(f.chorizon) <- f.diaghz

  # print any messages on possible data quality problems:
  if(exists('component.hz.problems', envir=soilDB.env))
    if(length(get("component.hz.problems", envir = soilDB.env)) > 0)
      message("-> QC: horizon errors detected, use `get('component.hz.problems', envir=soilDB.env)` for related coiid values")
  
  # set NASIS component specific horizon identifier
  hzidname(f.chorizon) <- 'chiid'
  
  # set metadata
  m <- metadata(f.chorizon)
  m$origin <- 'NASIS components'
  metadata(f.chorizon) <- m

  # done, return SPC
  return(f.chorizon)

}

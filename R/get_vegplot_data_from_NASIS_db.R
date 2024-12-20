## lower level functions for fetchVegdata()

#' @export
#' @rdname fetchVegdata
get_vegplot_from_NASIS_db <- function(SS = TRUE,
                                      stringsAsFactors = NULL,
                                      dsn = NULL) {
  
  .soilDB_warn_deprecated_aliases(c("usiteid" = "site_id", "assocuserpedonid" = "pedon_id", "vegplotid" = "vegplot_id"))
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }

  q.vegplot <- "SELECT siteiid, so.siteobsiid, usiteid AS site_id, usiteid, assocuserpedonid as pedon_id, assocuserpedonid, 
    v.vegplotid AS vegplot_id, v.vegplotid, vegplotiid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose,
    vegdataorigin, vegplotsize, soilprofileindicator, soil232idlegacy, ahorizondepth, alkalinesalineindicator,
    alkalineaffected, salinityclass, restrictivelayerdepthlegacy, legacysoilcompname, legacysoilphase, 
    legacylocalsoilphase, legacysoilsurftext, legacysurftextmod, legacyterminlieu, erosionclasslegacy, 
    landformgrouplegacy, cryptogamcovcllegacy, rangelandusehistory, cancovpctplotave, cancovtotalpct, 
    cancovtotalclass, overstorycancontotalpct, overstorycancovtotalclass, dblsampannualprodave, 
    compyieldproductionave, abovegroundbiomasstotave, understoryreprodabundance, woodyunderstoryabundance,
    herbundertoryabundance, lichensunderstoryabundance, crowncanclosurepct, crowncancloseassessmethod,
    crowncompfactorlpp, crowncomplppavedbh, basalcoverpctave, basalareaplottotal, basalareaassessmethod,
    constreeshrubgrp, windbreakrowonedirection, windbreaktrappedsoildepth, windbreaktrappedsoiltexture,
    understorydescindicator, mensurationdataindicator, vigorclasslegacy, siteconditionlegacy, 
    overstoryspecieslegacy, plantmoiststate, currenttreedensity, currenttreespacing, currentdxspacing,
    currentplotavedbh, plotbasalareafactor, currentbasalarea, foreststandtype, foreststratainventoried,
    foreststandregen, foreststandquality, desiredtreedensity, desireddxspacing, desiredbasalarea, 
    excessbasalarea, excesstreedensity, stockingchangepct, treepctgoodcondition, treepctfaircondition, 
    treepctpoorcondition, treecounttotal, treesnagdensityhard, treesnagdensitysoft, pastureforagetype, 
    pasturestanddensityave, pastureplanthtave, pastureprodave, pcidesirableplants, pciplantcover, 
    pciplantdiversity, pcigroundcovresidue, pcistandingdeadforage, pciplantresiduecompscore, pciplantvigor,
    pcilegumepctclass, pciuseuniformity, pcilivestockconcareas, pcisoilcompaction, pcisheetrillerosion, 
    pciwinderosion, pcistreamshoreerosion, pcigullyerosion, pcierosioncompscore, pcipastureconditionscore,
    refplantcommunity, repannualprod, totestannualprod, totallowableannualprod, totpalatableannualprod,
    similarityindex, annualuseableprod, harvesteffpct, takehalfleavehalf, acresperaum, aumperacre, 
    audperacre, desirableplantvigor, desirableseedlingabundance, decadentplantabundance, 
    plantresidueadequacy, undesirableinvadingspecies, majorinvadingspecies, invadingspeciescancovpct,
    soilsurferosion, soilcrusting, soilcompaction, baregroundpct, gullyrillpresence, soildegradationrating,
    rangetrendcurrent, rangetrendplanned, qcreviewperson, qcreviewdate, qareviewperson, 
    qareviewdate, swcdlegacy, fieldofficelegacy, nrcsarealegacy, aktotallichencoverpct, 
    aktotallitter1coverpct, aktotallitter2coverpct, aktotalmosscoverpct, aktotalrockcoverpct,
    aktotalsoilcoverpct, aktotalwatercoverpct, akecologicalsitestatus, aktotalbedrockcoverpct, akfieldecositeid
  FROM
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  ORDER BY s.siteiid;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.vegplot <- gsub(pattern = '_View_1', replacement = '', x = q.vegplot, fixed = TRUE)
  }

  # exec query
  d.vegplot <- dbQueryNASIS(channel, q.vegplot)

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.vegplot <- gsub(pattern = '_View_1', replacement = '', x = q.vegplot, fixed = TRUE)
  }

  # test for no data
  if (nrow(d.vegplot) == 0) {
    ds <- ifelse(SS, "NASIS selected set", "NASIS local database")
    stop('No NASIS site/vegetation plot records in ', ds, call. = FALSE)
  }
  # uncode metadata domains
  d <- uncode(d.vegplot, dsn = dsn)

  # done
  return(d)
}


# get location data from the corresponding record in the site table
#' @export
#' @rdname fetchVegdata
get_vegplot_location_from_NASIS_db <- function(SS = TRUE,
                                               stringsAsFactors = NULL,
                                               dsn = NULL) {
  
  .soilDB_warn_deprecated_aliases(c("usiteid" = "site_id", "vegplotid" = "vegplot_id"))
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }

  # query the coordinate, plss description, and site characteristics data for these records from the site table
  q.plotlocation <- "SELECT s.siteiid, so.siteobsiid, s.usiteid AS site_id, s.usiteid, v.vegplotid AS vegplot_id, v.vegplotid, vegplotiid, so.obsdate, v.datacollectionpurpose, latdegrees, latminutes, latseconds, latdir, longdegrees, longminutes, longseconds, longdir, horizdatnm, plsssection, plsstownship, plssrange, plssmeridian, utmzone, utmnorthing, utmeasting, latstddecimaldegrees, longstddecimaldegrees, geocoordsource, elev, slope, aspect, CAST(plsssdetails as text) AS plsssdetails, CAST(locdesc as text) AS locdesc
  FROM
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  ORDER BY s.siteiid;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.plotlocation <- gsub(pattern = '_View_1', replacement = '', x = q.plotlocation, fixed = TRUE)
  }

  # exec query
  d.plotlocation <- dbQueryNASIS(channel, q.plotlocation)

  # uncode metadata domains
  d <- uncode(d.plotlocation, dsn = dsn)

  # # test for no data
  # if (nrow(d) == 0)
  #   stop('there are no NASIS vegplots in your selected set!')

  # hack for CRAN check
  state_FIPS_codes <- NULL

  # load FIPS codes from local package data
  load(system.file("data/state_FIPS_codes.rda", package = "soilDB"))

  # add ESIS_id
  fips <- substr(d$usiteid, 3, 5)
  fips_state <- substr(d$usiteid, 1, 2)
  idx <- match(fips_state, state_FIPS_codes$state_alpha)
  fips_state_num <- state_FIPS_codes$state_fips[idx]
  year <- substr(d$usiteid, 8, 9)
  sitenum <- substr(d$usiteid, 10, 12)
  d$ESIS_id <- paste(sitenum, year, fips_state_num, fips, sep = '')

  # clean PLSS TRS data
  d$plsstownship <- gsub(d$plsstownship, pattern = '\\.', replacement = '', fixed = TRUE)
  d$plsstownship <- toupper(trimws(d$plsstownship))
  d$plssrange <- gsub(d$plssrange, pattern = '\\.', replacement = '', fixed = TRUE)
  d$plssrange <- toupper(trimws(d$plssrange))

  # done
  return(d)
}



# get Rangeland Health Indicator(RHI) associated fields in the vegplot table
#' @export
#' @rdname fetchVegdata
get_vegplot_trhi_from_NASIS_db <- function(SS = TRUE,
                                           stringsAsFactors = NULL,
                                           dsn = NULL) {
  
  .soilDB_warn_deprecated_aliases(c("usiteid" = "site_id", "assocuserpedonid" = "pedon_id", "vegplotid" = "vegplot_id"))
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }

  q.vegplotrhi <- "SELECT siteiid, so.siteobsiid, p.peiid, usiteid AS site_id, usiteid, assocuserpedonid AS pedon_id, assocuserpedonid, v.vegplotid AS vegplot_id, v.vegplotid, vegplotiid, vegplotname, obsdate, rhiannualprod, rhibareground, rhicompactionlayer, rhifuncstructgroups, rhierosionresistance, rhigullies, rhirills, rhipedastalsterracettes, rhiinfilrunoff, rhilitteramount, rhilittermovement, rhiplantmortality, rhireprodcapability, rhiinvasiveplants, rhisoilsurfdegradation, rhiwaterflowpatterns, rhiwindscourareas, rhisoilsitestabsumm, rhibioticintegritysumm, rhihydrofunctionsumm
  FROM
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  LEFT OUTER JOIN pedon_View_1 AS p ON p.siteobsiidref=so.siteobsiid
  ORDER BY s.siteiid;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.vegplotrhi <- gsub(pattern = '_View_1', replacement = '', x = q.vegplotrhi, fixed = TRUE)
  }

  # exec query
  d.vegplotrhi <- dbQueryNASIS(channel, q.vegplotrhi)

  # uncode metadata domains
  d <- uncode(d.vegplotrhi, dsn = dsn)

  # # test for no data
  # if (nrow(d) == 0) {
  #   stop('there are no NASIS vegplots in your selected set!')
  # }

  # done
  return(d)
}


# get vegplot species - this is a reconstruction of a site existing species list
#' @export
#' @rdname fetchVegdata
get_vegplot_species_from_NASIS_db <-  function(SS = TRUE,
                                               stringsAsFactors = NULL,
                                               dsn = NULL) {
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }

  q.vegplotspecies <- "SELECT siteiid, siteobsiid, vegplotiid, vegplotid, vegplotname, obsdate, primarydatacollector,
    datacollectionpurpose, assocuserpedonid, ppi.seqnum, plantsym, plantsciname, plantnatvernm,
    planttypegroup, plantheightcllowerlimit, plantheightclupperlimit,
    plantnativity, sociabilityclass, livecanopyhtbottom, livecanopyhttop, overstorydbhmin,
    overstorydbhmax, speciescancovpct, speciescancovclass, speciescomppct, speciesdbhaverage,
    speciescompbywtpct, speciestreecount, speciestraceamtflag, speciesbasalarea, understorygrcovpct,
    understorygrcovclass, seedlingdensityclass, maturedensityclass, vegetationstratalevel, orderofdominance,
    outsideplotindicator, estannualprod, esdannualprod, allowableannualprod, palatableannualprod,
    akstratumcoverclass, akfunctionalgroup, akstratumcoverclasspct
  FROM
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref = s.siteiid
  LEFT JOIN vegplot_View_1 AS v ON v.siteobsiidref = so.siteobsiid
  LEFT JOIN plotplantinventory_View_1 AS ppi ON ppi.vegplotiidref = v.vegplotiid
  LEFT JOIN plant ON plant.plantiid = ppi.plantiidref
  ORDER BY s.siteiid, ppi.orderofdominance, ppi.seqnum;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.vegplotspecies <- gsub(pattern = '_View_1', replacement = '', x = q.vegplotspecies, fixed = TRUE)
  }

  # exec query
  d.vegplotspecies <- dbQueryNASIS(channel, q.vegplotspecies)

  # uncode metadata domains
  d <- uncode(d.vegplotspecies, dsn = dsn)

  # # test for no data
  # if (nrow(d) == 0) {
  #   stop('there are no NASIS vegplots in your selected set!', call. = FALSE)
  # }

  # done
  return(d)
}


# get vegplot transect data
#' @export
#' @rdname fetchVegdata
get_vegplot_transect_from_NASIS_db <-  function(SS = TRUE,
                                                stringsAsFactors = NULL,
                                                dsn = NULL) {
  
  .soilDB_warn_deprecated_aliases(c("usiteid" = "site_id", "assocuserpedonid" = "pedon_id", "vegplotid" = "vegplot_id", "vegtransectid" = "vegtransect_id"))
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }

  # veg transect data - many transects to one vegplot
  q.vegtransect <- "SELECT siteiid, siteobsiid, vegplotiid, p.peiid, vegplotiidref, vegtransectiid,
    usiteid AS site_id, usiteid, assocuserpedonid AS pedon_id, assocuserpedonid, 
    vegplotid AS vegplot_id, vegplotid, vegplotname, vegtransectid as vegtransect_id, vegtransectid, 
    obsdate, primarydatacollector, datacollectionpurpose, transectstartlatitude, 
    transectstartlongitude, transectendlatitude, transectendlongitude, transectazimuth, transectlength,
    transectstartelevation, transectendelevation, dblsampquadratssampled, dblsampquadratsclipped, 
    nestedfreqquadratssampled, freqquadratssampled, dwrquadratssampled, daubenmirequadratssampled,
    quadratsizedomlegacy, quadratsizeseclegacy, quadratshapedomlegacy, quadratshapeseclegacy, 
    beltwidth, dblsampannualprod, totharvestannualprod, wtunitannualprod, dwrannualprod, comparativeyieldprod,
    comparativeyieldranktotal, comparativeyieldrankave, comparativerefclipwtave, abovegroundbiomasstotal,
    standingherbbiomass, transectbasalcovpct, basalcovpcttotal, basalgapsizemin, canopygapsizemin, 
    gapsmeasuredbetween, canopygaplengthtotal, canopygappcttotal, basalgaplengthtotal, basalgappcttotal, 
    vt.understoryreprodabundance, vt.woodyunderstoryabundance, vt.herbundertoryabundance, 
    vt.lichensunderstoryabundance, cancovpcttotaltrans, cancovtotalclasstrans, cancovassessmethod,
    vt.crowncanclosurepct, vt.crowncancloseassessmethod, vt.crowncompfactorlpp, vt.crowncomplppavedbh, 
    overstorycancovpcttrans, overstorycancovclasstrans, groundcovassessmethod, groundcovquadratssampled, 
    groundcovpointssampled, groundsurfcovassessmethod, groundsurfcovquadratsamp, groundsurfcovpointssamp, 
    lpiobsinterval, totalpointssampledcount, topcanopyhtave, topcanopyhtstddev, totalnumplantsbelt,
    totalnumspeciesbelt, totalplantdensitybelt
  FROM
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  LEFT OUTER JOIN pedon_View_1 AS p ON so.siteobsiid = p.siteobsiidref
  LEFT JOIN vegtransect_View_1 AS vt ON vt.vegplotiidref=v.vegplotiid
  ORDER BY s.siteiid;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.vegtransect <- gsub(pattern = '_View_1', replacement = '', x = q.vegtransect, fixed = TRUE)
  }

  # exec query
  d.vegtransect <- dbQueryNASIS(channel, q.vegtransect)

  # # test for no data
  # if (nrow(d.vegtransect) == 0) {
  #   stop('there are no NASIS vegplots transects in your selected set!', call. = FALSE)
  # }

  # uncode metadata domains
  d <- uncode(d.vegtransect, dsn = dsn)

  # done
  return(d)
}


# get vegplot transect species data
#' @export
#' @rdname fetchVegdata
get_vegplot_transpecies_from_NASIS_db <-  function(SS = TRUE,
                                                   stringsAsFactors = NULL,
                                                   dsn = NULL) {
  
  .soilDB_warn_deprecated_aliases(c("vegtransplantsummiid" = "vstpiid"))
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }

  # veg transect species data - many species to one veg transect
  q.vtps <- "SELECT siteiid, siteobsiid, vegplotiid, vegtransectiidref as vegtransectiid, vegplotid, vegplotname,
    obsdate, vegtransplantsummiid AS vstpiid, vegtransplantsummiid, vtps.seqnum, plantsym, plantsciname,
    plantnatvernm, plantnativity, planttypegroup,
    plantheightcllowerlimit, plantheightclupperlimit, sociabilityclass,
    specieslivecanhtbotave, specieslivecanhttopave, overstorydbhmin,
    overstorydbhmax, speciesovercancovpct, speciesovercancovclass,
    plantprodquadratsize, plantprodquadratshape, nestedfreqquadratsize,
    nestedfreqquadratshape, frequencyquadratsize, frequencyquadratshape,
    dwrquadratsize, dwrquadratshape, densityquadratsize, densityquadratshape,
    speciestotwtclippedest, speciestotwtclippedfresh, speciestotwtclippedairdry,
    speciestotwtairdry, speciestotwtest, speciestotwtexisting, speciesdrywtpct,
    speciestotwt, speciesaveyielddblsamp, speciescomppctdblsamp, speciescomppctdaubenmire,
    speciescomppctlineintercept, speciestraceamtflag, weightconvfactor,
    dblsampcorrectionfactor, airdrywtadjustment, utilizationadjustment, growthadjustment,
    weatheradjustment, numberofquadratsin, speciesfreqdaubenmire, dwronetally,
    dwrtwotally, dwrthreetally, dwrweightedtally, speciescomppctdwr, speciesaveyielddwr,
    wtunitweight, wtunitcounttotal, speciesaveyieldwtunit, wtunitwtclippedtotal,
    speciescancovhitcount, speciescancovpct, speciescancovpctavedaub, speciescancovaveclass,
    speciesfoliarcovhitcount, speciesfoliarcovpctlineint, speciestotfoliarcovlineint,
    speciesbasalcovhitcount, speciesbasalcovpctlineint, speciestotbasalcovlineint,
    maturecounttotal, maturedensityave, maturedensityaveclass, seedlingcounttotal,
    seedlingdensityave, seedlingdensityaveclass, speciesgroundcovabundclass,
    speciescancovportion, speciesbasalarea, vtps.basalareaassessmethod,
    vegtransplantsummiid
  FROM
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  LEFT JOIN vegtransect_View_1 AS vt ON vt.vegplotiidref=v.vegplotiid
  LEFT JOIN vegtransectplantsummary_View_1 AS vtps ON vtps.vegtransectiidref=vt.vegtransectiid
  LEFT JOIN plant ON plant.plantiid=vtps.plantiidref
  ORDER BY s.siteiid;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.vtps <- gsub(pattern = '_View_1', replacement = '', x = q.vtps, fixed = TRUE)
  }

  # exec query
  d.vegtransplantsum <- dbQueryNASIS(channel, q.vtps)

  # # test for no data
  # if (nrow(d.vegtransplantsum) == 0)
  #   stop('there are no NASIS vegplots transect species in your selected set!')

  # uncode metadata domains
  d <- uncode(d.vegtransplantsum, dsn = dsn)

  # done
  return(d)
}

# get point plant cover details for vegtransect plant summary
#' @export
#' @rdname fetchVegdata
get_vegplot_transpoints_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {
  q <- "SELECT siteiid, siteobsiid, vegplotiid, vegtransectiid,
               plantsym, plantsciname, plantnatvernm,
               transectpointlocation,
               livecanopyhtbottom, livecanopyhttop,
               canopycoverpresent, foliarcoverpresent, basalcoverpresent,
               pointplantcovdetailsiid, vegtransplantsummiidref
              FROM site_View_1 AS s
              INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
              INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
              LEFT JOIN vegtransect_View_1 AS vt
                     ON vt.vegplotiidref = v.vegplotiid
              INNER JOIN vegtransectplantsummary_View_1 AS vtps
                     ON vtps.vegtransectiidref = vt.vegtransectiid
              INNER JOIN pointplantcoverdetails_View_1 AS ppcd
                     ON ppcd.vegtransplantsummiidref = vtps.vegtransplantsummiid
              INNER JOIN plant ON plant.plantiid = vtps.plantiidref"
  if (!SS) {
    q <- gsub("_View_1", "", q)
  }

  res <- dbQueryNASIS(NASIS(dsn = dsn), q)
  uncode(res)
}


# get vegplot transect production quadrats
#' @export
#' @rdname fetchVegdata
get_vegplot_prodquadrats_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {
  q <- "SELECT siteiid, siteobsiid, vegplotiid, vegtransectiid,
               plantsym, plantsciname, plantnatvernm,
               quadratnumber, transectpointlocation, quadratclippedindicator,
               specieswtairdry, specieswtclipped, specieswtestimated,
               ppqd.speciestraceamtflag, weightunitcount, ppqd.speciescancovpct,
               speciescancovclass,
               plantprodquaddetailsiid, vegtransplantsummiidref
              FROM site_View_1 AS s
              INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
              INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
              LEFT JOIN vegtransect_View_1 AS vt
                     ON vt.vegplotiidref = v.vegplotiid
              INNER JOIN vegtransectplantsummary_View_1 AS vtps
                     ON vtps.vegtransectiidref = vt.vegtransectiid
              INNER JOIN plantprodquadratdetails_View_1 AS ppqd
                     ON ppqd.vegtransplantsummiidref = vtps.vegtransplantsummiid
              INNER JOIN plant ON plant.plantiid = vtps.plantiidref"
  if (!SS) {
    q <- gsub("_View_1", "", q)
  }

  res <- dbQueryNASIS(NASIS(dsn = dsn), q)
  uncode(res)
}

# get vegplot tree site index summary data
#' @export
#' @rdname fetchVegdata
get_vegplot_tree_si_summary_from_NASIS_db <-  function(SS = TRUE,
                                                       stringsAsFactors = NULL,
                                                       dsn = NULL) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }

  # plot tree site index summary data
  q.pltsis <- "SELECT siteiid, siteobsiid, vegplotiid, pltsis.seqnum, plantiidref, plantsym, plantsciname, plantnatvernm, plantnativity, siteindexbase, speciestreecount, siteindexplotave, speciesdbhaverage, treeageave, treecanopyhttopave, plottreesiteindsumiid

  FROM
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=so.siteobsiid
  LEFT JOIN plottreesiteindexsummary_View_1 AS pltsis ON pltsis.vegplotiidref=v.vegplotiid
  LEFT JOIN plant ON plant.plantiid=pltsis.plantiidref
  ORDER BY s.siteiid;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.pltsis <- gsub(pattern = '_View_1', replacement = '', x = q.pltsis, fixed = TRUE)
  }

  # exec query
  d.vegsiteindexsum <- dbQueryNASIS(channel, q.pltsis)

  # test for no data
  # if (nrow(d.vegsiteindexsum) == 0)
  #   stop('there are no NASIS vegplots tree site index data in your selected set!', call. = FALSE)

  # uncode metadata domains
  d <- uncode(d.vegsiteindexsum, dsn = dsn)

  # done
  return(d)
}

# get vegplot species basal area
#' @export
#' @rdname fetchVegdata
get_vegplot_speciesbasalarea_from_NASIS <- function(SS = TRUE, dsn = NULL) {
  q <- "SELECT siteiid, siteobsiid, vegplotiid, vegplotid, vegplotname, obsdate,
  primarydatacollector, plantiidref AS plantiid,
  plotspeciebasalareaiid, basalareatreescountediid
            plantsym, plantsciname, plantnatvernm,
            basalareafactor, speciesnumbertreesin, speciesbasalarea,
            treenumber, treeheight, treediameterbreastheight
FROM site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref = s.siteiid
  LEFT JOIN vegplot_View_1 AS v ON v.siteobsiidref = so.siteobsiid
  LEFT JOIN plotspeciesbasalarea_View_1 AS vb ON vb.vegplotiidref = v.vegplotiid
    LEFT JOIN basalareatreescounted_View_1 AS ba ON ba.plotspeciebasalareaiidref = vb.plotspeciebasalareaiid
    INNER JOIN plant ON plant.plantiid = vb.plantiidref"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  if (!SS) {
    q <- gsub("_View_1", "", q)
  }

  uncode(dbQueryNASIS(channel, q), dsn = dsn)
}

# get vegplot tree site index details data
#' @export
#' @rdname fetchVegdata
get_vegplot_tree_si_details_from_NASIS_db <- function(SS = TRUE,
                                                      stringsAsFactors = NULL,
                                                      dsn = NULL) {

  # plot tree site index detail data
  q.pltsid <- "SELECT  siteiid, siteobsiid, vegplotiid, plottreesiteindsumiidref, pltsid.seqnum, plantsym, plantsciname, plantnatvernm, treenumber, crownclass, reproductionsource, treediameterbreastheight, tenyeargrowthradius, growthringcount, growthringcountheight, growthringcountage, treeage, treecanopyhtbottom, treecanopyhttop, plottreesiteinddetailsiid

  FROM
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=so.siteobsiid
  LEFT JOIN vegtransect_View_1 AS vt ON vt.vegplotiidref=v.vegplotiid
  LEFT JOIN plottreesiteindexsummary_View_1 AS pltsis ON pltsis.vegplotiidref=v.vegplotiid
  LEFT JOIN plottreesiteindexdetails_View_1 AS pltsid ON pltsid.plottreesiteindsumiidref=pltsis.plottreesiteindsumiid
  LEFT JOIN plant ON plant.plantiid=pltsis.plantiidref
  ORDER BY s.siteiid;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.pltsid <- gsub(pattern = '_View_1', replacement = '', x = q.pltsid, fixed = TRUE)
  }

  # exec query
  d.vegsiteindexdet <- dbQueryNASIS(channel, q.pltsid)

  # test for no data
  # if (nrow(d.vegsiteindexdet) == 0) {
  #   stop('there are no NASIS vegplots tree site index data in your selected set!', call. = FALSE)
  # }

  # uncode metadata domains
  d <- uncode(d.vegsiteindexdet, dsn = dsn)

  # done
  return(d)
}


# get vegplot textnotes
#' @param fixLineEndings Replace `'\r\n'` with `'\n'`; Default: `TRUE`
#' @export
#' @rdname fetchVegdata
get_vegplot_textnote_from_NASIS_db <- function(SS = TRUE,
                                               fixLineEndings = TRUE,
                                               stringsAsFactors = NULL,
                                               dsn = NULL) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }

  # vegplot textnotes
  q.vegplottext <- "SELECT vegplotiidref AS vegplotiid, seqnum, recdate, recauthor, vegplottextkind,
textcat, textsubcat, vegplottextiid, CAST(textentry as text) AS textentry
FROM vegplottext_View_1;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.vegplottext <- gsub(pattern = '_View_1', replacement = '', x = q.vegplottext, fixed = TRUE)
  }

  # exec query
  d.vegplottext <- dbQueryNASIS(channel, q.vegplottext)

  # # test for no data
  # if (nrow(d.vegplottext) == 0)
  #  stop('there are no NASIS vegplots textnotes in your selected set!', call. = FALSE)

  # uncode metadata domains
  d <- uncode(d.vegplottext, dsn = dsn)

  # optionally convert \r\n -> \n
  if (fixLineEndings) {
    d$textentry <- gsub(d$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
  }

  # done
  return(d)
}



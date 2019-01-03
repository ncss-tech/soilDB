## lower level functions for fetchVegdata()

get_vegplot_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  # must have stringr installed
  if(!requireNamespace('stringr'))
    stop('please install the `stringr` package', call.=FALSE)

  q.vegplot <- "SELECT siteiid, p.peiid, usiteid as site_id, assocuserpedonid as pedon_id, v.vegplotid as vegplot_id, vegplotiid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, vegdataorigin, vegplotsize, soilprofileindicator, soil232idlegacy, ahorizondepth, alkalinesalineindicator, alkalineaffected, salinityclass, restrictivelayerdepthlegacy, legacysoilcompname, legacysoilphase, legacylocalsoilphase, legacysoilsurftext, legacysurftextmod, legacyterminlieu, erosionclasslegacy, landformgrouplegacy, cryptogamcovcllegacy, rangelandusehistory, cancovpctplotave, cancovtotalpct, cancovtotalclass, overstorycancontotalpct, overstorycancovtotalclass, dblsampannualprodave, compyieldproductionave, abovegroundbiomasstotave, understoryreprodabundance, woodyunderstoryabundance, herbundertoryabundance, lichensunderstoryabundance, crowncanclosurepct, crowncancloseassessmethod, crowncompfactorlpp, crowncomplppavedbh, basalcoverpctave, basalareaplottotal, basalareaassessmethod, constreeshrubgrp, windbreakrowonedirection, windbreaktrappedsoildepth, windbreaktrappedsoiltexture, understorydescindicator, mensurationdataindicator, vigorclasslegacy, siteconditionlegacy, overstoryspecieslegacy, plantmoiststate, currenttreedensity, currenttreespacing, currentdxspacing, currentplotavedbh, plotbasalareafactor, currentbasalarea, foreststandtype, foreststratainventoried, foreststandregen, foreststandquality, desiredtreedensity, desireddxspacing, desiredbasalarea, excessbasalarea, excesstreedensity, stockingchangepct, treepctgoodcondition, treepctfaircondition, treepctpoorcondition, treecounttotal, treesnagdensityhard, treesnagdensitysoft, pastureforagetype, pasturestanddensityave, pastureplanthtave, pastureprodave, pcidesirableplants, pciplantcover, pciplantdiversity, pcigroundcovresidue, pcistandingdeadforage, pciplantresiduecompscore, pciplantvigor, pcilegumepctclass, pciuseuniformity, pcilivestockconcareas, pcisoilcompaction, pcisheetrillerosion, pciwinderosion, pcistreamshoreerosion, pcigullyerosion, pcierosioncompscore, pcipastureconditionscore, refplantcommunity, repannualprod, totestannualprod, totallowableannualprod, totpalatableannualprod, similarityindex, annualuseableprod, harvesteffpct, takehalfleavehalf, acresperaum, aumperacre, audperacre, desirableplantvigor, desirableseedlingabundance, decadentplantabundance, plantresidueadequacy, undesirableinvadingspecies, majorinvadingspecies, invadingspeciescancovpct, soilsurferosion, soilcrusting, soilcompaction, baregroundpct, gullyrillpresence, soildegradationrating, rangetrendcurrent, rangetrendplanned, qcreviewperson, qcreviewdate, qareviewperson, qareviewdate, swcdlegacy, fieldofficelegacy, nrcsarealegacy
  FROM 
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  LEFT OUTER JOIN pedon_View_1 AS p ON p.siteobsiidref=so.siteobsiid
  ORDER BY s.siteiid;"

  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))

  # exec query
  d.vegplot <- RODBC::sqlQuery(channel, q.vegplot, stringsAsFactors=FALSE)

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # close connection
  RODBC::odbcClose(channel)

  d <- uncode(d.vegplot)

  # test for no data
  if(nrow(d) == 0)
  stop('there are no NASIS vegplots in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}


  # get location data from the corresponding record in the site table
  get_vegplot_location_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  # query the coordinate, plss description, and site characteristics data for these records from the site table
  q.plotlocation <- "SELECT s.siteiid, s.usiteid as site_id, v.vegplotid as vegplot_id, vegplotiid, so.obsdate, v.datacollectionpurpose, latdegrees, latminutes, latseconds, latdir, longdegrees, longminutes, longseconds, longdir, horizdatnm, locdesc, plsssdetails, plsssection, plsstownship, plssrange, plssmeridian, utmzone, utmnorthing, utmeasting, latstddecimaldegrees, longstddecimaldegrees, geocoordsource, elev, slope, aspect 
  FROM 
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  ORDER BY s.siteiid;"

  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials')) 

  # exec query
  d.plotlocation <- RODBC::sqlQuery(channel, q.plotlocation, stringsAsFactors=FALSE)

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # close connection
  RODBC::odbcClose(channel)

  d <- uncode(d.plotlocation)	

  # test for no data
  if(nrow(d) == 0)
  stop('there are no NASIS vegplots in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)

  
  # hack for CRAN check
  state_FIPS_codes <- NULL
  # load FIPS codes from local package data
  load(system.file("data/state_FIPS_codes.rda", package="soilDB"))
  
  # add ESIS_id
  fips <- stringr::str_sub(d$site_id, 3, 5)
  fips_state <- stringr::str_sub(d$site_id, 1, 2)
  idx <- match(fips_state, state_FIPS_codes$state_alpha)
  fips_state_num <- state_FIPS_codes$state_fips[idx]
  year <- stringr::str_sub(d$site_id, 8, 9)
  sitenum <- stringr::str_sub(d$site_id, 10, 12)
  d$ESIS_id <- paste(sitenum, year, fips_state_num, fips, sep='')

  # clean PLSS TRS data 
  d$plsstownship <- gsub(d$plsstownship, pattern = '\\.', replacement = '', fixed = TRUE)
  d$plsstownship <- toupper(trimws(d$plsstownship))
  d$plssrange <- gsub(d$plssrange, pattern = '\\.', replacement = '', fixed = TRUE)
  d$plssrange <- toupper(trimws(d$plssrange))	
  
  # done
  return(d)
}


  
  # get Rangeland Health Indicator(RHI) associated fields in the vegplot table
  get_vegplot_trhi_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q.vegplotrhi <- "SELECT siteiid, p.peiid, usiteid as site_id, assocuserpedonid as pedon_id, v.vegplotid as vegplot_id, vegplotiid, vegplotname, obsdate, rhiannualprod, rhibareground, rhicompactionlayer, rhifuncstructgroups, rhierosionresistance, rhigullies, rhirills, rhipedastalsterracettes, rhiinfilrunoff, rhilitteramount, rhilittermovement, rhiplantmortality, rhireprodcapability, rhiinvasiveplants, rhisoilsurfdegradation, rhiwaterflowpatterns, rhiwindscourareas, rhisoilsitestabsumm, rhibioticintegritysumm, rhihydrofunctionsumm 
  FROM 
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  LEFT OUTER JOIN pedon_View_1 AS p ON p.siteobsiidref=so.siteobsiid
  ORDER BY s.siteiid;"

  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials')) 

  # exec query
  d.vegplotrhi <- RODBC::sqlQuery(channel, q.vegplotrhi, stringsAsFactors=FALSE)

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # close connection
  RODBC::odbcClose(channel)

  d <- uncode(d.vegplotrhi)	

  # test for no data
  if(nrow(d) == 0)
  stop('there are no NASIS vegplots in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}

  
  # get vegplot species - this is a reconstruction of a site existing species list
  get_vegplot_species_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q.vegplotspecies <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, ppi.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, speciescancovclass
  FROM 
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  LEFT JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  LEFT JOIN plotplantinventory_View_1 AS ppi ON ppi.vegplotiidref=v.vegplotiid
  INNER JOIN plant ON plant.plantiid=ppi.plantiidref
  ORDER BY s.siteiid, ppi.orderofdominance, ppi.seqnum;"

  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))

  # exec query
  d.vegplotspecies <- RODBC::sqlQuery(channel, q.vegplotspecies, stringsAsFactors=FALSE)

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }	
  
  # close connection
  RODBC::odbcClose(channel)

  d <- uncode(d.vegplotspecies)

  # test for no data
  if(nrow(d) == 0)
  stop('there are no NASIS vegplots in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}

  
  # get vegplot transect data
  get_vegplot_transect_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  # veg transect data - many transects to one vegplot
  q.vegtransect<- "SELECT siteiid, p.peiid, vegplotiidref, vegtransectiid, usiteid as site_id, assocuserpedonid as pedon_id, vegplotid as vegplot_id, vegplotname, vegtransectid as vegtransect_id, obsdate, primarydatacollector, datacollectionpurpose, transectstartlatitude, transectstartlongitude, transectendlatitude, transectendlongitude, transectazimuth, transectlength, transectstartelevation, transectendelevation, dblsampquadratssampled, dblsampquadratsclipped, nestedfreqquadratssampled, freqquadratssampled, dwrquadratssampled, daubenmirequadratssampled, quadratsizedomlegacy, quadratsizeseclegacy, quadratshapedomlegacy, quadratshapeseclegacy, beltwidth, dblsampannualprod, totharvestannualprod, wtunitannualprod, dwrannualprod, comparativeyieldprod, comparativeyieldranktotal, comparativeyieldrankave, comparativerefclipwtave, abovegroundbiomasstotal, standingherbbiomass, transectbasalcovpct, basalcovpcttotal, basalgapsizemin, canopygapsizemin, gapsmeasuredbetween, canopygaplengthtotal, canopygappcttotal, basalgaplengthtotal, basalgappcttotal, vt.understoryreprodabundance, vt.woodyunderstoryabundance, vt.herbundertoryabundance, vt.lichensunderstoryabundance, cancovpcttotaltrans, cancovtotalclasstrans, cancovassessmethod, vt.crowncanclosurepct, vt.crowncancloseassessmethod, vt.crowncompfactorlpp, vt.crowncomplppavedbh, overstorycancovpcttrans, overstorycancovclasstrans, groundcovassessmethod, groundcovquadratssampled, groundcovpointssampled, groundsurfcovassessmethod, groundsurfcovquadratsamp, groundsurfcovpointssamp, lpiobsinterval, totalpointssampledcount, topcanopyhtave, topcanopyhtstddev, totalnumplantsbelt, totalnumspeciesbelt, totalplantdensitybelt
  FROM
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  LEFT OUTER JOIN pedon_View_1 AS p ON so.siteobsiid = p.siteobsiidref
  LEFT JOIN vegtransect_View_1 AS vt ON vt.vegplotiidref=v.vegplotiid
  ORDER BY s.siteiid;"

  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))

  # exec query
  d.vegtransect <- RODBC::sqlQuery(channel, q.vegtransect, stringsAsFactors=FALSE)

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }	

  # close connection
  RODBC::odbcClose(channel)

  d <- uncode(d.vegtransect)

  # test for no data
  if(nrow(d) == 0)
  stop('there are no NASIS vegplots transects in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}
	

  # get vegplot transect species data
  get_vegplot_transpecies_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  # veg transect species data - many species to one veg transect
  q.vtps<- "SELECT siteiid, vegtransectiidref as vegtransect_id, vegplotid, vegplotname, obsdate, vegtransplantsummiid as vtpsiid, vtps.seqnum, plantsym, plantsciname, plantnatvernm, plantnativity, planttypegroup, plantheightcllowerlimit, plantheightclupperlimit, sociabilityclass, specieslivecanhtbotave, specieslivecanhttopave, overstorydbhmin, overstorydbhmax, speciesovercancovpct, speciesovercancovclass, plantprodquadratsize, plantprodquadratshape, nestedfreqquadratsize, nestedfreqquadratshape, frequencyquadratsize, frequencyquadratshape, dwrquadratsize, dwrquadratshape, densityquadratsize, densityquadratshape, speciestotwtclippedest, speciestotwtclippedfresh, speciestotwtclippedairdry, speciestotwtairdry, speciestotwtest, speciestotwtexisting, speciesdrywtpct, speciestotwt, speciesaveyielddblsamp, speciescomppctdblsamp, speciescomppctdaubenmire, speciescomppctlineintercept, speciestraceamtflag, weightconvfactor, dblsampcorrectionfactor, airdrywtadjustment, utilizationadjustment, growthadjustment, weatheradjustment, numberofquadratsin, speciesfreqdaubenmire, dwronetally, dwrtwotally, dwrthreetally, dwrweightedtally, speciescomppctdwr, speciesaveyielddwr, wtunitweight, wtunitcounttotal, speciesaveyieldwtunit, wtunitwtclippedtotal, speciescancovhitcount, speciescancovpct, speciescancovpctavedaub, speciescancovaveclass, speciesfoliarcovhitcount, speciesfoliarcovpctlineint, speciestotfoliarcovlineint, speciesbasalcovhitcount, speciesbasalcovpctlineint, speciestotbasalcovlineint, maturecounttotal, maturedensityave, maturedensityaveclass, seedlingcounttotal, seedlingdensityave, seedlingdensityaveclass, speciesgroundcovabundclass, speciescancovportion, speciesbasalarea, vtps.basalareaassessmethod
  FROM 
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v ON v.siteobsiidref=so.siteobsiid
  LEFT JOIN vegtransect_View_1 AS vt ON vt.vegplotiidref=v.vegplotiid
  LEFT JOIN vegtransectplantsummary_View_1 AS vtps ON vtps.vegtransectiidref=vt.vegtransectiid
  INNER JOIN plant ON plant.plantiid=vtps.plantiidref
  ORDER BY s.siteiid;"
  
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))
  
  # exec query
  d.vegtransplantsum <- RODBC::sqlQuery(channel, q.vtps, stringsAsFactors=FALSE)

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }	
  
  # close connection
  RODBC::odbcClose(channel)

  d <- uncode(d.vegtransplantsum)

  # test for no data
  if(nrow(d) == 0)
  stop('there are no NASIS vegplots transect species in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}


  # get vegplot tree site index summary data
  get_vegplot_tree_si_summary_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  # plot tree site index summary data
  q.pltsis<- "SELECT vegplotiidref AS vegplotiid, pltsis.seqnum, plantiidref, plantsym, plantsciname, plantnatvernm, plantnativity, siteindexbase, speciestreecount, siteindexplotave, speciesdbhaverage, treeageave, treecanopyhttopave, plottreesiteindsumiid

  FROM 
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=so.siteobsiid
  LEFT JOIN plottreesiteindexsummary_View_1 AS pltsis ON pltsis.vegplotiidref=v.vegplotiid
  INNER JOIN plant ON plant.plantiid=pltsis.plantiidref
  ORDER BY s.siteiid;"

  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))
  
  # exec query
  d.vegsiteindexsum <- RODBC::sqlQuery(channel, q.pltsis, stringsAsFactors=FALSE)

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }	
  
  # close connection
  RODBC::odbcClose(channel)

  d <- uncode(d.vegsiteindexsum)

  # test for no data
  if(nrow(d) == 0)
  stop('there are no NASIS vegplots tree site index data in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}


  # get vegplot tree site index details data
  get_vegplot_tree_si_details_from_NASIS_db <- function(SS=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  # plot tree site index detail data
  q.pltsid<- "SELECT plottreesiteindsumiidref, pltsid.seqnum, plantsym, plantsciname, plantnatvernm, treenumber, crownclass, reproductionsource, treediameterbreastheight, tenyeargrowthradius, growthringcount, growthringcountheight, growthringcountage, treeage, treecanopyhtbottom, treecanopyhttop, plottreesiteinddetailsiid

  FROM 
  site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=so.siteobsiid
  LEFT JOIN vegtransect_View_1 AS vt ON vt.vegplotiidref=v.vegplotiid
  LEFT JOIN plottreesiteindexsummary_View_1 AS pltsis ON pltsis.vegplotiidref=v.vegplotiid
  LEFT JOIN plottreesiteindexdetails_View_1 AS pltsid ON pltsid.plottreesiteindsumiidref=pltsis.plottreesiteindsumiid
  INNER JOIN plant ON plant.plantiid=pltsis.plantiidref
  ORDER BY s.siteiid;"


  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))
  
  # exec query
  d.vegsiteindexdet <- RODBC::sqlQuery(channel, q.pltsid, stringsAsFactors=FALSE)

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }	
  
  # close connection
  RODBC::odbcClose(channel)

  d <- uncode(d.vegsiteindexdet)

  # test for no data
  if(nrow(d) == 0)
  stop('there are no NASIS vegplots tree site index data in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)
  
  # done
  return(d)
}


  # get vegplot textnotes
  get_vegplot_textnote_from_NASIS_db <- function(SS=TRUE, fixLineEndings=TRUE, stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)


  # vegplot textnotes
  q.vegplottext <- "SELECT vegplotiidref as vegplotiid, seqnum, recdate, recauthor, vegplottextkind, 
textcat, textsubcat, textentry, vegplottextiid 
FROM vegplottext_View_1;"

# setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))
  
  # exec query
  d.vegplottext <- RODBC::sqlQuery(channel, q.vegplottext, stringsAsFactors=FALSE)

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }	
  
  # close connection
  RODBC::odbcClose(channel)

  d <- uncode(d.vegplottext)

  # test for no data
  if(nrow(d) == 0)
  stop('there are no NASIS vegplots textnotes in your selected set!')
  
  # uncode metadata domains
  d <- uncode(d, stringsAsFactors = stringsAsFactors)

  # optionally convert \r\n -> \n
  if(fixLineEndings){
    d$textentry <- gsub(d$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
  }
  
  # done
  return(d)
}



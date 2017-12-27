## TODO: merge with other vegplot functions

get_vegplot_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q.vegplot <- "SELECT siteiid, p.peiid, usiteid as site_id, assocuserpedonid as pedon_id, v.vegplotid as vegplot_id, vegplotiid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, vegdataorigin, vegplotsize, soilprofileindicator, soil232idlegacy, ahorizondepth, alkalinesalineindicator, alkalineaffected, salinityclass, restrictivelayerdepthlegacy, legacysoilcompname, legacysoilphase, legacylocalsoilphase, legacysoilsurftext, legacysurftextmod, legacyterminlieu, erosionclasslegacy, landformgrouplegacy, cryptogamcovcllegacy, rangelandusehistory, cancovpctplotave, cancovtotalpct, cancovtotalclass, overstorycancontotalpct, overstorycancovtotalclass, dblsampannualprodave, compyieldproductionave, abovegroundbiomasstotave, understoryreprodabundance, woodyunderstoryabundance, herbundertoryabundance, lichensunderstoryabundance, crowncanclosurepct, crowncancloseassessmethod, crowncompfactorlpp, crowncomplppavedbh, basalcoverpctave, basalareaplottotal, basalareaassessmethod, constreeshrubgrp, windbreakrowonedirection, windbreaktrappedsoildepth, windbreaktrappedsoiltexture, understorydescindicator, mensurationdataindicator, vigorclasslegacy, siteconditionlegacy, overstoryspecieslegacy, plantmoiststate, currenttreedensity, currenttreespacing, currentdxspacing, currentplotavedbh, plotbasalareafactor, currentbasalarea, foreststandtype, foreststratainventoried, foreststandregen, foreststandquality, desiredtreedensity, desireddxspacing, desiredbasalarea, excessbasalarea, excesstreedensity, stockingchangepct, treepctgoodcondition, treepctfaircondition, treepctpoorcondition, treecounttotal, treesnagdensityhard, treesnagdensitysoft, pastureforagetype, pasturestanddensityave, pastureplanthtave, pastureprodave, pcidesirableplants, pciplantcover, pciplantdiversity, pcigroundcovresidue, pcistandingdeadforage, pciplantresiduecompscore, pciplantvigor, pcilegumepctclass, pciuseuniformity, pcilivestockconcareas, pcisoilcompaction, pcisheetrillerosion, pciwinderosion, pcistreamshoreerosion, pcigullyerosion, pcierosioncompscore, pcipastureconditionscore, refplantcommunity, repannualprod, totestannualprod, totallowableannualprod, totpalatableannualprod, similarityindex, annualuseableprod, harvesteffpct, takehalfleavehalf, acresperaum, aumperacre, audperacre, desirableplantvigor, desirableseedlingabundance, decadentplantabundance, plantresidueadequacy, undesirableinvadingspecies, majorinvadingspecies, invadingspeciescancovpct, soilsurferosion, soilcrusting, soilcompaction, baregroundpct, gullyrillpresence, soildegradationrating, rangetrendcurrent, rangetrendplanned, qcreviewperson, qcreviewdate, qareviewperson, qareviewdate, swcdlegacy, fieldofficelegacy, nrcsarealegacy
  FROM 
  site AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT OUTER JOIN pedon AS p ON p.siteobsiidref=siteobs.siteobsiid
  ORDER BY s.siteiid;" 
  
  
  
  q.vegplotrhi <- "SELECT siteiid, p.peiid, usiteid as site_id, assocuserpedonid as pedon_id, v.vegplotid as vegplot_id, vegplotiid, vegplotname, obsdate, rhiannualprod, rhibareground, rhicompactionlayer, rhifuncstructgroups, rhierosionresistance, rhigullies, rhirills, rhipedastalsterracettes, rhiinfilrunoff, rhilitteramount, rhilittermovement, rhiplantmortality, rhireprodcapability, rhiinvasiveplants, rhisoilsurfdegradation, rhiwaterflowpatterns, rhiwindscourareas, rhisoilsitestabsumm, rhibioticintegritysumm, rhihydrofunctionsumm 
  FROM 
  site AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT OUTER JOIN pedon AS p ON p.siteobsiidref=siteobs.siteobsiid
  ORDER BY s.siteiid;" 
  
  
  # plot-level species data from plotplantinventory table - this records species within plot presence/absence - this catches the migrated data from the old siteexistingveg table
  q.vegplotspecies <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, plotplantinventory.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, speciescancovclass
  FROM 
  site AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  LEFT JOIN vegplot AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT JOIN plotplantinventory ON plotplantinventory.vegplotiidref=v.vegplotiid
  INNER JOIN plant ON plant.plantiid=plotplantinventory.plantiidref
  ORDER BY s.siteiid, plotplantinventory.orderofdominance, plotplantinventory.seqnum;"
  
  
  # veg transect data - many transects to one vegplot
  q.vegtransect<- "SELECT siteiid, p.peiid, vegplotiidref, vegtransectiid, usiteid as site_id, assocuserpedonid as pedon_id, vegplotid as vegplot_id, vegplotname, vegtransectid as vegtransect_id, obsdate, primarydatacollector, datacollectionpurpose, transectstartlatitude, transectstartlongitude, transectendlatitude, transectendlongitude, transectazimuth, transectlength, transectstartelevation, transectendelevation, dblsampquadratssampled, dblsampquadratsclipped, nestedfreqquadratssampled, freqquadratssampled, dwrquadratssampled, daubenmirequadratssampled, quadratsizedomlegacy, quadratsizeseclegacy, quadratshapedomlegacy, quadratshapeseclegacy, beltwidth, dblsampannualprod, totharvestannualprod, wtunitannualprod, dwrannualprod, comparativeyieldprod, comparativeyieldranktotal, comparativeyieldrankave, comparativerefclipwtave, abovegroundbiomasstotal, standingherbbiomass, transectbasalcovpct, basalcovpcttotal, basalgapsizemin, canopygapsizemin, gapsmeasuredbetween, canopygaplengthtotal, canopygappcttotal, basalgaplengthtotal, basalgappcttotal, vt.understoryreprodabundance, vt.woodyunderstoryabundance, vt.herbundertoryabundance, vt.lichensunderstoryabundance, cancovpcttotaltrans, cancovtotalclasstrans, cancovassessmethod, vt.crowncanclosurepct, vt.crowncancloseassessmethod, vt.crowncompfactorlpp, vt.crowncomplppavedbh, overstorycancovpcttrans, overstorycancovclasstrans, groundcovassessmethod, groundcovquadratssampled, groundcovpointssampled, groundsurfcovassessmethod, groundsurfcovquadratsamp, groundsurfcovpointssamp, lpiobsinterval, totalpointssampledcount, topcanopyhtave, topcanopyhtstddev, totalnumplantsbelt, totalnumspeciesbelt, totalplantdensitybelt
  FROM
  site AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT OUTER JOIN pedon AS p ON siteobs.siteobsiid = p.siteobsiidref
  LEFT JOIN vegtransect AS vt ON vt.vegplotiidref=v.vegplotiid
  ORDER BY s.siteiid;"
  
  # veg transect species data - many species to one veg transect
  q.vtps<- "SELECT siteiid, vegtransectiidref as vegtransect_id, vegplotid, vegplotname, obsdate, vegtransplantsummiid as vtpsiid, vtps.seqnum, plantsym, plantsciname, plantnatvernm, plantnativity, planttypegroup, plantheightcllowerlimit, plantheightclupperlimit, sociabilityclass, specieslivecanhtbotave, specieslivecanhttopave, overstorydbhmin, overstorydbhmax, speciesovercancovpct, speciesovercancovclass, plantprodquadratsize, plantprodquadratshape, nestedfreqquadratsize, nestedfreqquadratshape, frequencyquadratsize, frequencyquadratshape, dwrquadratsize, dwrquadratshape, densityquadratsize, densityquadratshape, speciestotwtclippedest, speciestotwtclippedfresh, speciestotwtclippedairdry, speciestotwtairdry, speciestotwtest, speciestotwtexisting, speciesdrywtpct, speciestotwt, speciesaveyielddblsamp, speciescomppctdblsamp, speciescomppctdaubenmire, speciescomppctlineintercept, speciestraceamtflag, weightconvfactor, dblsampcorrectionfactor, airdrywtadjustment, utilizationadjustment, growthadjustment, weatheradjustment, numberofquadratsin, speciesfreqdaubenmire, dwronetally, dwrtwotally, dwrthreetally, dwrweightedtally, speciescomppctdwr, speciesaveyielddwr, wtunitweight, wtunitcounttotal, speciesaveyieldwtunit, wtunitwtclippedtotal, speciescancovhitcount, speciescancovpct, speciescancovpctavedaub, speciescancovaveclass, speciesfoliarcovhitcount, speciesfoliarcovpctlineint, speciestotfoliarcovlineint, speciesbasalcovhitcount, speciesbasalcovpctlineint, speciestotbasalcovlineint, maturecounttotal, maturedensityave, maturedensityaveclass, seedlingcounttotal, seedlingdensityave, seedlingdensityaveclass, speciesgroundcovabundclass, speciescancovportion, speciesbasalarea, vtps.basalareaassessmethod
  FROM 
  site AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT JOIN vegtransect AS vt ON vt.vegplotiidref=v.vegplotiid
  LEFT JOIN vegtransectplantsummary AS vtps ON vtps.vegtransectiidref=vt.vegtransectiid
  INNER JOIN plant ON plant.plantiid=vtps.plantiidref
  ORDER BY s.siteiid;"
  
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  # exec queries
  d.vegplot <- RODBC::sqlQuery(channel, q.vegplot, stringsAsFactors=FALSE)
  d.vegplotrhi <- RODBC::sqlQuery(channel, q.vegplotrhi, stringsAsFactors=FALSE)
  d.vegplotspecies <- RODBC::sqlQuery(channel, q.vegplotspecies, stringsAsFactors=FALSE)
  d.vegtransect <- RODBC::sqlQuery(channel, q.vegtransect, stringsAsFactors=FALSE)
  d.vegtransplantsum <- RODBC::sqlQuery(channel, q.vtps, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)

  # uncode domained columns
#  d.vegplot <- uncode(q.vegplot)
#  d.vegplotrhi <- uncode(q.vegplotrhi)
#  d.vegplotspecies <- uncode(q.vegplotspecies)
#  d.vegtransect <- uncode(q.vegtransect)
#  d.vegtransplantsum <- uncode(q.vtps)
  
  
  # return a list of results
  return(list(vegplot=d.vegplot, 
              vegplotrhi=d.vegplotrhi,	
              vegplotspecies=d.vegplotspecies,	
              vegtransect=d.vegtransect,
              vegtransplantsum=d.vegtransplantsum))
}

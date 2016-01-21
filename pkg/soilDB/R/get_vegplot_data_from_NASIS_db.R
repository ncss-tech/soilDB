## TODO: merge with other vegplot functions

get_vegplot_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q.vegplot <- "SELECT siteiid, p.peiid, usiteid as site_id, assocuserpedonid as pedon_id, v.vegplotid as vegplot_id, vegplotiid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, vdo.ChoiceLabel as vegdataorigin, vegplotsize, soilprofileindicator, soil232idlegacy, ahorizondepth, asi.ChoiceLabel as alkalinesalineindicator, alkalineaffected, sc.ChoiceLabel as salinityclass, restrictivelayerdepthlegacy, legacysoilcompname, legacysoilphase, legacylocalsoilphase, lsst.ChoiceLabel as legacysoilsurftext, lstm.ChoiceLabel as legacysurftextmod, ltil.ChoiceLabel as legacyterminlieu, lec.ChoiceLabel as erosionclasslegacy, llg.ChoiceLabel as landformgrouplegacy, lccc.ChoiceLabel as cryptogamcovcllegacy, ruh.ChoiceLabel as rangelandusehistory, cancovpctplotave, cancovtotalpct, ccc.ChoiceLabel as cancovtotalclass, overstorycancontotalpct, ccc.ChoiceLabel as overstorycancovtotalclass, dblsampannualprodave, compyieldproductionave, abovegroundbiomasstotave, ac.ChoiceLabel as understoryreprodabundance, ac.ChoiceLabel as woodyunderstoryabundance, ac.ChoiceLabel as herbundertoryabundance, ac.ChoiceLabel as lichensunderstoryabundance, crowncanclosurepct, am.ChoiceLabel as crowncancloseassessmethod, crowncompfactorlpp, crowncomplppavedbh, basalcoverpctave, basalareaplottotal, am.ChoiceLabel as basalareaassessmethod, constreeshrubgrp, wbd.ChoiceLabel as windbreakrowonedirection, windbreaktrappedsoildepth, wbst.ChoiceLabel as windbreaktrappedsoiltexture, understorydescindicator, mensurationdataindicator, vcl.ChoiceLabel as vigorclasslegacy, siteconditionlegacy, overstoryspecieslegacy, pms.ChoiceLabel as plantmoiststate, currenttreedensity, currenttreespacing, currentdxspacing, currentplotavedbh, plotbasalareafactor, currentbasalarea, fst.ChoiceLabel as foreststandtype, fsi.ChoiceLabel as foreststratainventoried, fsr.ChoiceLabel as foreststandregen, fsq.ChoiceLabel as foreststandquality, desiredtreedensity, desireddxspacing, desiredbasalarea, excessbasalarea, excesstreedensity, stockingchangepct, treepctgoodcondition, treepctfaircondition, treepctpoorcondition, treecounttotal, treesnagdensityhard, treesnagdensitysoft, pastureforagetype, pasturestanddensityave, pastureplanthtave, pastureprodave, pcidp.ChoiceLabel as pcidesirableplants, pcipc.ChoiceLabel as pciplantcover, pcipd.ChoiceLabel as pciplantdiversity, pcigcr.ChoiceLabel as pcigroundcovresidue, pcisdf.ChoiceLabel as pcistandingdeadforage, pciplantresiduecompscore, pcipv.ChoiceLabel as pciplantvigor, pcilc.ChoiceLabel as pcilegumepctclass, pciuu.ChoiceLabel as pciuseuniformity, pcilca.ChoiceLabel as pcilivestockconcareas, pcisc.ChoiceLabel as pcisoilcompaction, pcisre.ChoiceLabel as pcisheetrillerosion, pciwe.ChoiceLabel as pciwinderosion, pcisse.ChoiceLabel as pcistreamshoreerosion, pcige.ChoiceLabel as pcigullyerosion, pcierosioncompscore, pcipastureconditionscore, refplantcommunity, repannualprod, totestannualprod, totallowableannualprod, totpalatableannualprod, similarityindex, annualuseableprod, harvesteffpct, takehalfleavehalf, acresperaum, aumperacre, audperacre, dpv.ChoiceLabel as desirableplantvigor, sac.ChoiceLabel as desirableseedlingabundance, dpa.ChoiceLabel as decadentplantabundance, pra.ChoiceLabel as plantresidueadequacy, undesirableinvadingspecies, majorinvadingspecies, invadingspeciescancovpct, sse.ChoiceLabel as soilsurferosion, scg.ChoiceLabel as soilcrusting, scpt.ChoiceLabel as soilcompaction, baregroundpct, gullyrillpresence, sdr.ChoiceLabel as soildegradationrating, rtc.ChoiceLabel as rangetrendcurrent, rtc.ChoiceLabel as rangetrendplanned, qcreviewperson, qcreviewdate, qareviewperson, qareviewdate, swcdlegacy, fieldofficelegacy, nrcsarealegacy
  FROM ((((((((((((((((((((((((((((((((((((((((((((
  site_View_1 AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT OUTER JOIN pedon_View_1 AS p ON p.siteobsiidref=siteobs.siteobsiid
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5256) AS vdo ON v.vegdataorigin = vdo.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5114) AS asi ON v.alkalinesalineindicator = asi.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5234) AS sc ON v.salinityclass = sc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 189) AS lsst ON v.legacysoilsurftext = lsst.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 190) AS lstm ON v.legacysurftextmod = lstm.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 192) AS ltil ON v.legacyterminlieu = ltil.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5154) AS lec ON v.erosionclasslegacy = lec.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5181) AS llg ON v.landformgrouplegacy = llg.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5129) AS lccc ON v.cryptogamcovcllegacy = lccc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5210) AS ruh ON v.rangelandusehistory = ruh.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5118) AS ccc ON v.cancovtotalclass = ccc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5113) AS ac ON v.understoryreprodabundance = ac.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5116) AS am ON v.crowncancloseassessmethod = am.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5212) AS sac ON v.desirableseedlingabundance = sac.ChoiceValue)  
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5260) AS wbd ON v.windbreakrowonedirection = wbd.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 189) AS wbst ON v.windbreaktrappedsoiltexture = wbst.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5258) AS vcl ON v.vigorclasslegacy = vcl.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5201) AS pms ON v.plantmoiststate = pms.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5158) AS fst ON v.foreststandtype = fst.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5159) AS fsi ON v.foreststratainventoried = fsi.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5157) AS fsr ON v.foreststandregen = fsr.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5156) AS fsq ON v.foreststandquality = fsq.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5186) AS pcidp ON v.pcidesirableplants = pcidp.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5190) AS pcipc ON v.pciplantcover = pcipc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5191) AS pcipd ON v.pciplantdiversity = pcipd.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5187) AS pcigcr ON v.pcigroundcovresidue = pcigcr.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5195) AS pcisdf ON v.pcistandingdeadforage = pcisdf.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5192) AS pcipv ON v.pciplantvigor = pcipv.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5189) AS pcilc ON v.pcilegumepctclass = pcilc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5197) AS pciuu ON v.pciuseuniformity = pciuu.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5185) AS pcilca ON v.pcilivestockconcareas = pcilca.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5194) AS pcisc ON v.pcisoilcompaction = pcisc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5193) AS pcisre ON v.pcisheetrillerosion = pcisre.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5198) AS pciwe ON v.pciwinderosion = pciwe.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5196) AS pcisse ON v.pcistreamshoreerosion = pcisse.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5188) AS pcige ON v.pcigullyerosion = pcige.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5258) AS dpv ON v.desirableplantvigor = dpv.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5203) AS pra ON v.plantresidueadequacy = pra.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5124) AS dpa ON v.decadentplantabundance = dpa.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5242) AS sse ON v.soilsurferosion = sse.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5240) AS scg ON v.soilcrusting = scg.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5239) AS scpt ON v.soilcompaction = scpt.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5241) AS sdr ON v.soildegradationrating = sdr.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5209) AS rtc ON v.rangetrendcurrent = rtc.ChoiceValue)
  ORDER BY s.siteiid;" 
  
  
  
  q.vegplotrhi <- "SELECT siteiid, p.peiid, usiteid as site_id, assocuserpedonid as pedon_id, v.vegplotid as vegplot_id, vegplotiid, vegplotname, obsdate, rhiap.ChoiceLabel as rhiannualprod, rhibg.ChoiceLabel as rhibareground, rhicl.ChoiceLabel as rhicompactionlayer, rhifsg.ChoiceLabel as rhifuncstructgroups, rhier.ChoiceLabel as rhierosionresistance, rhig.ChoiceLabel as rhigullies, rhir.ChoiceLabel as rhirills, rhipt.ChoiceLabel as rhipedastalsterracettes, rhiir.ChoiceLabel as rhiinfilrunoff, rhila.ChoiceLabel as rhilitteramount, rhilm.ChoiceLabel as rhilittermovement, rhipm.ChoiceLabel as rhiplantmortality, rhirc.ChoiceLabel as rhireprodcapability, rhiip.ChoiceLabel as rhiinvasiveplants, rhissd.ChoiceLabel as rhisoilsurfdegradation, rhiwf.ChoiceLabel as rhiwaterflowpatterns, rhiws.ChoiceLabel as rhiwindscourareas, rhiss.ChoiceLabel as rhisoilsitestabsumm, rhibi.ChoiceLabel as rhibioticintegritysumm, rhihf.ChoiceLabel as rhihydrofunctionsumm 
  FROM ((((((((((((((((((((
  site_View_1 AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT OUTER JOIN pedon_View_1 AS p ON p.siteobsiidref=siteobs.siteobsiid
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5214) AS rhiap ON v.rhiannualprod = rhiap.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5215) AS rhibg ON v.rhibareground = rhibg.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5216) AS rhicl ON v.rhicompactionlayer = rhicl.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5218) AS rhifsg ON v.rhifuncstructgroups = rhifsg.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5217) AS rhier ON v.rhierosionresistance = rhier.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5219) AS rhig ON v.rhigullies = rhig.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5227) AS rhir ON v.rhirills = rhir.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5224) AS rhipt ON v.rhipedastalsterracettes = rhipt.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5220) AS rhiir ON v.rhiinfilrunoff = rhiir.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5222) AS rhila ON v.rhilitteramount = rhila.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5223) AS rhilm ON v.rhilittermovement = rhilm.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5225) AS rhipm ON v.rhiplantmortality = rhipm.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5226) AS rhirc ON v.rhireprodcapability = rhirc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5221) AS rhiip ON v.rhiinvasiveplants = rhiip.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5228) AS rhissd ON v.rhisoilsurfdegradation = rhissd.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5230) AS rhiwf ON v.rhiwaterflowpatterns = rhiwf.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5231) AS rhiws ON v.rhiwindscourareas = rhiws.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5229) AS rhiss ON v.rhisoilsitestabsumm = rhiss.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5229) AS rhibi ON v.rhibioticintegritysumm = rhibi.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5229) AS rhihf ON v.rhihydrofunctionsumm = rhihf.ChoiceValue)
  ORDER BY s.siteiid;" 
  
  
  # plot-level species data from plotplantinventory table - this records species within plot presence/absence - this catches the migrated data from the old siteexistingveg table
  q.vegplotspecies <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, plotplantinventory.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, cc.ChoiceLabel as speciescancovclass
  FROM (
  site_View_1 AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  LEFT JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT JOIN plotplantinventory ON plotplantinventory.vegplotiidref=v.vegplotiid
  INNER JOIN plant ON plant.plantiid=plotplantinventory.plantiidref
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5118) AS cc ON speciescancovclass = cc.ChoiceValue)
  ORDER BY s.siteiid, plotplantinventory.orderofdominance, plotplantinventory.seqnum;"
  
  
  # veg transect data - many transects to one vegplot
  q.vegtransect<- "SELECT siteiid, p.peiid, vegplotiidref, vegtransectiid, usiteid as site_id, assocuserpedonid as pedon_id, vegplotid as vegplot_id, vegplotname, vegtransectid as vegtransect_id, obsdate, primarydatacollector, datacollectionpurpose, transectstartlatitude, transectstartlongitude, transectendlatitude, transectendlongitude, transectazimuth, transectlength, transectstartelevation, transectendelevation, dblsampquadratssampled, dblsampquadratsclipped, nestedfreqquadratssampled, freqquadratssampled, dwrquadratssampled, daubenmirequadratssampled, quadratsizedomlegacy, quadratsizeseclegacy, qsdl.ChoiceLabel as quadratshapedomlegacy, qsdl.ChoiceLabel as quadratshapeseclegacy, beltwidth, dblsampannualprod, totharvestannualprod, wtunitannualprod, dwrannualprod, comparativeyieldprod, comparativeyieldranktotal, comparativeyieldrankave, comparativerefclipwtave, abovegroundbiomasstotal, standingherbbiomass, transectbasalcovpct, basalcovpcttotal, basalgapsizemin, canopygapsizemin, gapsmeasuredbetween, canopygaplengthtotal, canopygappcttotal, basalgaplengthtotal, basalgappcttotal, ac.ChoiceLabel as understoryreprodabundance, ac.ChoiceLabel as woodyunderstoryabundance, ac.ChoiceLabel as herbundertoryabundance, ac.ChoiceLabel as lichensunderstoryabundance, cancovpcttotaltrans, cc.ChoiceLabel as cancovtotalclasstrans, am.ChoiceLabel as cancovassessmethod, vt.crowncanclosurepct, am.ChoiceLabel as crowncancloseassessmethod, vt.crowncompfactorlpp, vt.crowncomplppavedbh, overstorycancovpcttrans, cc.ChoiceLabel as overstorycancovclasstrans, am.ChoiceLabel as groundcovassessmethod, groundcovquadratssampled, groundcovpointssampled, am.ChoiceLabel as groundsurfcovassessmethod, groundsurfcovquadratsamp, groundsurfcovpointssamp, lpiobsinterval, totalpointssampledcount, topcanopyhtave, topcanopyhtstddev, totalnumplantsbelt, totalnumspeciesbelt, totalplantdensitybelt
  FROM ((((
  site_View_1 AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT OUTER JOIN pedon AS p ON siteobs.siteobsiid = p.siteobsiidref
  LEFT JOIN vegtransect AS vt ON vt.vegplotiidref=v.vegplotiid
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5208) AS qsdl ON vt.quadratshapedomlegacy = qsdl.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5113) AS ac ON vt.understoryreprodabundance = ac.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5116) AS am ON vt.crowncancloseassessmethod = am.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5118) AS cc ON vt.overstorycancovclasstrans = cc.ChoiceValue)
  ORDER BY s.siteiid;"
  
  # veg transect species data - many species to one veg transect
  q.vtps<- "SELECT siteiid, vegtransectiidref as vegtransect_id, vegplotid, vegplotname, obsdate, vegtransplantsummiid as vtpsiid, vtps.seqnum, plantsym, plantsciname, plantnatvernm, pn.ChoiceLabel as plantnativity, ptg.ChoiceLabel as planttypegroup, plantheightcllowerlimit, plantheightclupperlimit, sc.ChoiceLabel as sociabilityclass, specieslivecanhtbotave, specieslivecanhttopave, overstorydbhmin, overstorydbhmax, speciesovercancovpct, cc.ChoiceLabel as speciesovercancovclass, plantprodquadratsize, qs.ChoiceLabel as plantprodquadratshape, nestedfreqquadratsize, qs.ChoiceLabel as nestedfreqquadratshape, frequencyquadratsize, qs.ChoiceLabel as frequencyquadratshape, dwrquadratsize, qs.ChoiceLabel as dwrquadratshape, densityquadratsize, qs.ChoiceLabel as densityquadratshape, speciestotwtclippedest, speciestotwtclippedfresh, speciestotwtclippedairdry, speciestotwtairdry, speciestotwtest, speciestotwtexisting, speciesdrywtpct, speciestotwt, speciesaveyielddblsamp, speciescomppctdblsamp, speciescomppctdaubenmire, speciescomppctlineintercept, speciestraceamtflag, weightconvfactor, dblsampcorrectionfactor, airdrywtadjustment, utilizationadjustment, growthadjustment, weatheradjustment, numberofquadratsin, speciesfreqdaubenmire, dwronetally, dwrtwotally, dwrthreetally, dwrweightedtally, speciescomppctdwr, speciesaveyielddwr, wtunitweight, wtunitcounttotal, speciesaveyieldwtunit, wtunitwtclippedtotal, speciescancovhitcount, speciescancovpct, speciescancovpctavedaub, cc.ChoiceLabel as speciescancovaveclass, speciesfoliarcovhitcount, speciesfoliarcovpctlineint, speciestotfoliarcovlineint, speciesbasalcovhitcount, speciesbasalcovpctlineint, speciestotbasalcovlineint, maturecounttotal, maturedensityave, mdc.ChoiceLabel as maturedensityaveclass, seedlingcounttotal, seedlingdensityave, sdc.ChoiceLabel as seedlingdensityaveclass, speciesgroundcovabundclass, speciescancovportion, speciesbasalarea, am.ChoiceLabel as basalareaassessmethod
  FROM ((((((((
  site_View_1 AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT JOIN vegtransect AS vt ON vt.vegplotiidref=v.vegplotiid
  LEFT JOIN vegtransectplantsummary AS vtps ON vtps.vegtransectiidref=vt.vegtransectiid
  INNER JOIN plant ON plant.plantiid=vtps.plantiidref
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5202) AS pn ON plantnativity = pn.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5204) AS ptg ON planttypegroup = ptg.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5238) AS sc ON sociabilityclass = sc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5208) AS qs ON plantprodquadratshape = qs.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5118) AS cc ON speciesovercancovclass = cc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5208) AS mdc ON maturedensityaveclass = mdc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5113) AS sdc ON seedlingdensityaveclass = sdc.ChoiceValue)
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5116) AS am ON vtps.basalareaassessmethod = am.ChoiceValue)
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
  
  # return a list of results
  return(list(vegplot=d.vegplot, 
              vegplotrhi=d.vegplotrhi,	
              vegplotspecies=d.vegplotspecies,	
              vegtransect=d.vegtransect,
              vegtransplantsum=d.vegtransplantsum))
}

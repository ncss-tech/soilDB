## New get function for lab data


get_lab_data_from_NASIS_db <- function() {

q.phlabresults <- "SELECT phorizon_View_1.peiidref AS peiid, phorizon_View_1.phiid AS phiid, sampledepthtop, sampledepthbottom, sampleid, datacollector, claytotmeasured, claycarbmeasured, silttotmeasured, siltfinemeasured, siltcomeasured, sandtotmeasured, stm.ChoiceName AS sandtot_method, sandvfmeasured, svfm.ChoiceName AS sandvf_method, sandfinemeasured, sandmedmeasured, sandcomeasured, sandvcmeasured, tcfl.ChoiceName AS texcl_fieldlab, fiberrubbedpct, fiberunrubbedpct, ph1to1h2o, ph01mcacl2, phnaf, phoxidized, phdeltah2o2, liquidlimitmeasured, plasticlimitmeasured, pi, attcon.ChoiceName AS attberg_sampcond, cole, esttotpotacidityetpa, camgmeh2, potassiummeh2, camgsatpaste, extractaciditykcl, basesatmeh2, phosphatephos, nitratenitrogen, ecmeasured, ecm.ChoiceName AS ec_method, ec15, caco3equivmeasured, gypsumequiv, sodium, sar,gypsumreq, humiccolor, fulviccolor, humicfulviccolor, alummeasured, pyphh.ChoiceName AS pyrophos_hue, pyphv.ChoiceName AS pyrophos_value, pyphc.ChoiceName AS pyrophos_chroma, melanicindex, phlabresultiid 
FROM ((((((((
(phorizon_View_1 INNER JOIN phlabresults_View_1 ON phorizon_View_1.phiid = phlabresults_View_1.phiidref)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 4952) AS stm ON phlabresults_View_1.sandtotmethod = stm.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 4953) AS svfm ON phlabresults_View_1.sandvfmethod = svfm.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 189) AS tcfl ON phlabresults_View_1.textureclfieldlab = tcfl.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 4941) AS attcon ON phlabresults_View_1.atterbergsampcond = attcon.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1257) AS ecm ON phlabresults_View_1.ecdeterminemeth = ecm.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1242) AS pyphh ON phlabresults_View_1.pyrophoshue = pyphh.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1244) AS pyphv ON phlabresults_View_1.pyrophosvalue = pyphv.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1241) AS pyphc ON phlabresults_View_1.pyrophoschroma = pyphc.ChoiceValue);"


q.ncsslabpedon <- "SELECT peiidref AS peiid, pedlabsampnum, psctopdepth, pscbotdepth, noncarbclaywtavg, claytotwtavg, le0to100, wf0175wtavgpsc, volfractgt2wtavg, cec7clayratiowtavg, labdatasheeturl, ncsspedonlabdataiid AS labpeiid 
FROM ncsspedonlabdata_View_1;"

q.ncsslablayer <- "SELECT ncsspedonlabdataiidref AS labpeiid, labsampnum, layerseqnum, hzdept, hzdepb, layertype, hzname, hznameoriginal, stratextsflag, mps.ChoiceName AS moist_prepstate, txcl.ChoiceName AS lab_texcl, sandvcmeasured, sandcomeasured, sandmedmeasured, sandvfmeasured, siltcomeasured, siltfinemeasured, silttotmeasured, claycarbmeasured, clayfinemeasured, claytotmeasured, carbonorganicpctmeasured, carbontotalpctmeasured, ompctest, fiberrubbedpct, fiberunrubbedpct, fragwt25, fragwt520, fragwt2075, fragwt275, wtpct0175, wtpctgt2ws, ph1to1h2o, ph01mcacl2, phnaf, phoxidized, resistivity, ecmeasured, esp, sar, cecsumcations, cec7, ecec, sumbases, basesatsumcations, basesatnh4oac, caco3equivmeasured, caco3lt20measured, gypsumequivmeasured, feoxalatemeasured, feextractable, fetotal, sioxalatemeasured, extracid, extral, aloxalatemeasured, altotal, pmehlich3, ph2osolublemeasured, poxalatemeasured, polsenmeasured, ptotalmeasured, nzpretention, dbthirdbar, dbovendry, aggstabpct, wtenthbarclod, wtenthbarsieve, wthirdbarclod, wthirdbarsieve, wfifteenbarmeasured, wretentiondiffws, wfifteenbartoclay, adod, lep, cole, liquidlimitmeasured, pi, recwlupdated, ncsslayerlabdataiid 
FROM ((
ncsslayerlabdata_View_1
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5103) AS mps ON ncsslayerlabdata_View_1.moistprepstate = mps.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 189) AS txcl ON ncsslayerlabdata_View_1.texcl = txcl.ChoiceValue);"





# setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='nasisRe@d0n1y') 
	
	# exec queries
	d.hz.labresults <- sqlQuery(channel, q.phlabresults, stringsAsFactors=FALSE)
	d.labpedon <- sqlQuery(channel, q.ncsslabpedon, stringsAsFactors=FALSE)
	d.lablayer <- sqlQuery(channel, q.ncsslablayer, stringsAsFactors=FALSE)
		
	# close connection
	odbcClose(channel)
	
	# return a list of results
	return(list(hzlabresults=d.hz.labresults,
							labpedons=d.labpedon, 
							lablayers=d.lablayer))
}

## note: when multiple textures have been defined, only the first one is returned (alphabetical ?)

get_hz_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q <- "SELECT peiid, phiid, upedonid as pedon_id,
  hzname, dspcomplayerid as genhz, hzdept, hzdepb,
  claytotest as clay, CASE WHEN silttotest IS NULL THEN 100 - (claytotest + sandtotest) ELSE silttotest END as silt, sandtotest as sand, t.texture_class, texture, phfield, eff.ChoiceName AS effervescence, l.labsampnum, fragvoltot as total_frags_pct_cal, rrd.ChoiceName as rupresblkdry, stick.ChoiceName as stickiness, plast.ChoiceName as plasticity
  FROM
  pedon_View_1 
  INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref
	
	LEFT OUTER JOIN (
		SELECT phiidref, min(CASE WHEN tx.ChoiceName IS NULL THEN til.ChoiceName ELSE tx.ChoiceName END) as texture_class
		FROM
		phtexture_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 189) AS tx ON phtexture_View_1.texcl = tx.ChoiceValue
  	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 192) AS til ON phtexture_View_1.lieutex = til.ChoiceValue
		GROUP BY phiidref
	) as t ON phorizon_View_1.phiid = t.phiidref
  
	LEFT OUTER JOIN (SELECT phiidref, labsampnum FROM phsample_View_1) as l ON phorizon_View_1.phiid = l.phiidref
  
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE MetadataDomainDetail.DomainID = 1291) AS rrd ON phorizon_View_1.rupresblkdry = rrd.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1299) AS stick ON phorizon_View_1.stickiness = stick.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1280) AS plast ON phorizon_View_1.plasticity = plast.ChoiceValue
    
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1255) AS eff ON phorizon_View_1.effclass = eff.ChoiceValue
  ORDER BY pedon_View_1.upedonid, phorizon_View_1.hzdept ASC;"
	
	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# test for duplicate horizons:
	#  bugs in our queries
	#   multiple lab samples / genetic horizon
	hz.tab <- table(d$phiid)
	dupe.hz <- which(hz.tab > 1)
	dupe.hz.phiid <- names(hz.tab[dupe.hz])
	dupe.hz.pedon.ids <- d$pedon_id[d$phiid %in% dupe.hz.phiid]
	
	if(length(dupe.hz) > 0) {
		message(paste('NOTICE: multiple `labsampnum` values / horizons; see pedon IDs:\n', paste(unique(dupe.hz.pedon.ids), collapse=','), sep=''))
	}
	
	# close connection
	RODBC::odbcClose(channel)
	
	# done
	return(d)
}


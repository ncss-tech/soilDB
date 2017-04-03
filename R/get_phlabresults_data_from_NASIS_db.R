get_phlabresults_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if (!requireNamespace('RODBC')) stop('please install the `RODBC` package', call.=FALSE)

  q.phlabresults <- " SELECT phiidref, sampledepthtop, sampledepthbottom, claytotmeasured, sandtotmeasured, textureclfieldlab, ph1to1h2o, ph01mcacl2, caco3equivmeasured, sar 
  FROM phlabresults_View_1 
  ORDER BY phiidref, sampledepthtop
  ;"
  
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
  
  
  # exec query
  d.phlabresults <- RODBC::sqlQuery(channel, q.phlabresults, stringsAsFactors = FALSE)
  
  
  # recode metadata domains
  d.phlabresults <- .metadata_replace(d.phlabresults)
  
  
  # compute thickness
  d.phlabresults <- within(d.phlabresults, {
    hzthk = sampledepthbottom - sampledepthtop
    })
  
  
  # cache original column names
  orig_names <- names(d.phlabresults)
  
  
  # identify horizons with duplicate phiid
  idx <- which(duplicated(d.phlabresults$phiidref))
  
  if (length(idx) > 0) {
    message(paste("NOTICE: multiple phiid values exist in the `phlabresults` table, computing a weighted averages and dominant values based on horizon thickness"))
    
    # aggregate dup phiid
    dup <- d.phlabresults[idx, "phiidref"]
    dup_idx <- which(d.phlabresults$phiidref %in% dup)
    d.dups <- d.phlabresults[dup_idx, ]
    
    num_vars <- names(d.dups)[! grepl("ph1to1h2o|ph01mcacl2", names(d.dups)) &
                          sapply(d.dups, is.numeric)]
    d.dups_num <- d.dups[num_vars]
    d.dups_num <- plyr::ddply(d.dups_num, .(phiidref), function(x) {
      sapply(x[2:ncol(x)], function(x2) Hmisc::wtd.mean(x2, weights = x$hzthk, na.rm = TRUE))
      }
      )
    
    char_vars <- names(d.dups)[names(d.dups) %in% c("phiidref", "hzthk") |
                                 sapply(d.dups, function(x) is.character(x) | is.factor(x))]
    d.dups_char <- d.dups[char_vars]
    d.dups_char <- plyr::ddply(d.dups_char, .(phiidref), function(x) {
      sapply(x[2:ncol(x)], function(x2) x2[which.max(x$hzthk)])
      }
      )
    d.dups_char$hzthk <- NULL
    d.dups_char <- .metadata_replace(d.dups_char)
    
    num_ph <- names(d.dups)[names(d.dups) %in% c("phiidref", "hzthk") |
                          grepl("ph1to1h2o|ph01mcacl2", names(d.dups))]
    d.dups_ph <- d.dups[num_ph]
    d.dups_ph <- plyr::ddply(d.dups_ph, .(phiidref), function(x) {
      sapply(x[2:ncol(x)], function(x2) -log10(Hmisc::wtd.mean(1/10^x2, weights = x$hzthk, na.rm = TRUE)))
      }
      )
    d.dups_ph$hzthk <- NULL
    
    d.nodups <- join(d.dups_num, d.dups_char, by  = "phiidref", type = "left")
    d.nodups <- join(d.nodups, test_ph, by = "phiidref", type = "left")
    d.nodups <- d.nodups[orig_names]
    
    d.phlabresults <- rbind(d.phlabresults[-dup_idx, ], d.nodups)
    
    return(d.phlabresults)
    }

    
  # relabel names
  d.phlabresults[c("sampledepthtop", "sampledepthbottom", "hzthk")] <- NULL

  names(d.phlabresults) <- gsub("measured|lab$", "", names(d.phlabresults))
  
  vars <- c("phiidref")
  idx <- names(d.phlabresults) %in% vars
  names(d.phlabresults)[idx] <- c("phiid")
  
  names(d.phlabresults)[!idx] <- paste0(names(d.phlabresults)[!idx], "_lab")
  
  
  # close connection
  RODBC::odbcClose(channel)
  
  
  # done
  return(d.phlabresults)
  }
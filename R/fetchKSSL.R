
# create a valid URL filter for SoilWeb API
# arguments are NA by default
.buildFilter <- function(series, bbox, mlra, pedlabsampnum, pedon_id, pedon_key) {
  
  # sanity-check: user must supply some kind of criteria
  if(missing(series) & missing(bbox) & missing(mlra) & missing(pedlabsampnum) & missing(pedon_id) & missing(pedon_key))
    stop('you must provide some filtering criteria')
  
  # init empty filter
  f <- vector()
  
  # process filter components
  if(!is.na(series)) {
    f <- c(f, paste('&series=', series, sep=''))
  }
  
  # note: bbox has already been converted into text representation, suitable for URL
  if(!is.na(bbox)) {
    f <- c(f, paste('&bbox=', bbox, sep=''))
  }
  
  if(!is.na(mlra)) {
    f <- c(f, paste('&mlra=', mlra, sep=''))
  }
  
  if(!is.na(pedlabsampnum)) {
    f <- c(f, paste('&pedlabsampnum=', pedlabsampnum, sep=''))
  }
  
  if(!is.na(pedon_id)) {
    f <- c(f, paste('&pedon_id=', pedon_id, sep=''))
  }
  
  if(!is.na(pedon_key)) {
    f <- c(f, paste('&pedon_key=', pedon_key, sep=''))
  }
  
  # combine filters
  f <- paste(f, collapse='')
  
  return(f)
}


# get extended data as JSON -> list, from SoilWeb API
.getExtended_SoilWeb <- function(f) {
  
  # KSSL geochem, XRD, glass
  extended.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/kssl/query.php?gzip=1&format=json&what=extended', f))
  
  ## get data
  # note: missing data are returned as FALSE
  # list of dataframe objects
  ext <- jsonlite::fromJSON(extended.url)
  
  # done  
  return(ext)
  
}

# get simplified NASIS morphologic data as JSON -> list, from SoilWeb API
.getMorphologic_SoilWeb <- function(f) {
  
  # NASIS morphology
  morph.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/kssl/query.php?gzip=1&format=json&what=nasis_morphologic', f)) 
  
  ## get data
  # note: missing data are returned as FALSE
  # list of dataframe objects
  m <- jsonlite::fromJSON(morph.url)
  
  # done  
  return(m)
}


# get simplified KSSL site+hz data as JSON -> list, from SoilWeb API
.getKSSL_SoilWeb <- function(f) {
  
  # KSSL site + horizon
  site_hz.url <- URLencode(paste0('https://casoilresource.lawr.ucdavis.edu/soil_web/kssl/query.php?gzip=1&format=json&what=site_hz', f))
  
  ## get data
  # note: missing data are returned as FALSE
  # list of dataframe objects
  site_hz <- jsonlite::fromJSON(site_hz.url)
  
  # report missing data
  if(
    all(
      c(isFALSE(site_hz[['site']]), 
        isFALSE(site_hz[['horizon']])
      )
    )
  ) {
    # no data
    return(NULL)
  } 
  
  # done 
  return(site_hz)
  
}


# single record getter function, called by new vectorized interface
.fetchSingle_KSSL <- function(f, returnMorphologicData=FALSE, returnGeochemicalData=FALSE, simplifyColors=FALSE) {
  
  # basic site + hz data
  sh <- .getKSSL_SoilWeb(f)
  
  ## TODO: this may no longer be a valid approach, once vectorization is complete
  # account for missing data
  if(is.null(sh)) {
    return(NULL)
  }
  
  # local copies
  s <- sh[['site']]
  h <- sh[['horizon']]
  
  # upgrade to SoilProfileCollection
  suppressMessages(depths(h) <- pedon_key ~ hzn_top + hzn_bot)
  site(h) <- s
  
  # NASIS morphoogy
  if(returnMorphologicData) {
    m <- .getMorphologic_SoilWeb(f)
  }
  
  # get geochemical, optical and XRD data from extended query
  if(returnGeochemicalData) {
    ext <- .getExtended_SoilWeb(f)
    
    # local copies
    geochem <- ext$geochem
    optical <- ext$optical
    xrd_thermal <- ext$xrd_thermal
  }
  
  
  ## TODO: clean this up
  if(returnMorphologicData & returnGeochemicalData)
    return(list(SPC=h, morph=m, geochem=geochem, optical=optical, xrd_thermal=xrd_thermal))	
  else if(returnGeochemicalData)
    return(list(SPC=h, geochem=geochem, optical=optical, xrd_thermal=xrd_thermal))
  else if(returnMorphologicData)
    return(list(SPC=h, morph=m))
  else
    return(h)
}



## requires new tests to ensure fixes are reasonable, see https://github.com/ncss-tech/soilDB/issues/103

# fully vectorized
fetchKSSL <- function(series=NA, bbox=NA, mlra=NA, pedlabsampnum=NA, pedon_id=NA, pedon_key=NA, returnMorphologicData=FALSE, returnGeochemicalData=FALSE, simplifyColors=FALSE) {
  
  if(!requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `jsonlite` packages', call.=FALSE)
  
  ## TODO: this is not vectorized
  # convert BBOX into text representation
  bbox <- paste(bbox, collapse=',')
  
  # create argument matrix
  arg <- expand.grid(
    series=series, 
    bbox=bbox, 
    mlra=mlra, 
    pedlabsampnum=pedlabsampnum, 
    pedon_id=pedon_id, 
    pedon_key=pedon_key
  )
  
  n.args <- nrow(arg)
  
  # list to store results
  res <- vector(mode = 'list', length = n.args)
  
  # iterate over argument set
  for(i in 1:n.args) {
    # build single URL filter
    f <- with(
      arg[i, ], 
      .buildFilter(series, bbox, mlra, pedlabsampnum, pedon_id, pedon_key)
    )
    
    # process a single request
    res[[i]] <- .fetchSingle_KSSL(f, returnMorphologicData, returnGeochemicalData)
  }
  
  
  ## make composite SPC and optionally additional parts
  
  # simple request, result is a list of SPCs
  if(!returnMorphologicData & !returnGeochemicalData) {
    h <- aqp::union(res)
    
    # NO site/hz data, stop here
    if(is.null(h)) {
      message('query returned no data')
      return(NULL)
    }
    
  } else {
    # complex request, result is a list of lists
    # SPC
    h <- aqp::union(lapply(res, '[[', 'SPC'))
    
    # NO site/hz data, stop here
    if(is.null(h)) {
      message('query returned no data')
      return(NULL)
    }
    
    ## NOTE: simpler with purrr::transpose()
    # morph
    if(returnMorphologicData) {
      # add new tables here
      v <- c("phcolor", "phfrags", "phpores", "phstructure", "pediagfeatures")
      m <- vector(mode = 'list')
      # iterate over tables and unwind - rbind - store
      for(i in v) {
        m[[i]] <- do.call('rbind', lapply(res, function(j) j$morph[[i]]))
      }
    }
    
    # geochem
    if(returnGeochemicalData) {
      geochem <- do.call('rbind', lapply(res, '[[', 'geochem'))
      optical <- do.call('rbind', lapply(res, '[[', 'optical'))
      xrd_thermal <- do.call('rbind', lapply(res, '[[', 'xrd_thermal'))
    }
    
  }
  
  
  # set KSSL-specific horizon identifier
  ## WHOOPS -- turns out this is nonunique 0.1% of the time AGB 2019/11/14
  # hzidname(h) <- "labsampnum"
  
  # set KSSL-specific hzdesgn/hztexcl fields
  hzdesgnname(h) <- "hzn_desgn"
  hztexclname(h) <- "lab_texture_class"
  
  ## set metadata
  h.metadata <- metadata(h)
  h.metadata$origin <- 'KSSL via Soilweb / fetchKSSL'
  metadata(h) <- h.metadata
  
  
  # cleaning up the results
  if(returnMorphologicData & simplifyColors) {
    
    if(inherits(m$phcolor, 'data.frame')) {
      # extract horizon data from SPC
      hh <- horizons(h)
      
      # simplify color data: 1 row / horizon, from morphologic data tables
      x.colors <- simplifyColorData(m$phcolor, id.var = 'labsampnum', wt='colorpct')
      
      # replace horizon data in h
      hh <- merge(hh, x.colors, by='labsampnum', all.x=TRUE, sort=FALSE)
      slot(h, 'horizons') <- hh 
    }
  }
  
  # report object size
  res.size <- round(object.size(res) / 1024 / 1024, 2)
  # some feedback via message:
  message(paste(length(h), ' pedons loaded (', res.size, ' Mb transferred)', sep=''))
  
  
  ## TODO: clean this up
  if(returnMorphologicData & returnGeochemicalData)
    return(list(SPC=h, morph=m, geochem=geochem, optical=optical, xrd_thermal=xrd_thermal))	
  else if(returnGeochemicalData)
    return(list(SPC=h, geochem=geochem, optical=optical, xrd_thermal=xrd_thermal))
  else if(returnMorphologicData)
    return(list(SPC=h, morph=m))
  else
    return(h)
}

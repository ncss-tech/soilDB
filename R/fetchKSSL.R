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
  # note: missing data are returned as FALSE by the API
  # when data are available, list of data.frame objects
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
  
  # sometimes columns containing all NA are treated as logical, they are not
  logical.idx <- which(sapply(h, is.logical))
  for(i in logical.idx) { h[[i]] <- as.numeric(h[[i]]) }
  
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
    
    # local copies, these are FALSE if data are missing
    geochem <- ext$geochem
    optical <- ext$optical
    xrd_thermal <- ext$xrd_thermal
    
    # cleanly return missing data as 0-length data.frame
    # having a 'labsampnum' (character) column for joins
    ul <- 'dummy.labsampnum'
    if(isFALSE(geochem)) {
      geochem <- data.frame(labsampnum = ul)[0, , drop = FALSE]
    }
    if(isFALSE(optical)) {
      optical <- data.frame(labsampnum = ul)[0, , drop = FALSE]
    }
    if(isFALSE(xrd_thermal)) {
      xrd_thermal <- data.frame(labsampnum = ul)[0, , drop = FALSE]
    }
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



# fully vectorized in all arguments except BBOX


#' Get Kellogg Soil Survey Laboratory Data from SoilWeb snapshot
#' 
#' Download soil characterization and morphologic data via BBOX, MLRA, or soil
#' series name query, from the KSSL database.
#' 
#' This is an experimental interface to a subset for the most commonly used
#' data from a snapshot of KSSL (lab characterization) and NASIS (morphologic)
#' data.
#' 
#' Series-queries are case insensitive. Series name is based on the "correlated
#' as" field (from KSSL snapshot) when present.  The "sampled as"
#' classification was promoted to "correlated as" if the "correlated as"
#' classification was missing.
#' 
#' When \code{returnMorphologicData} is TRUE, the resulting object is a list.
#' The standard output from \code{fetchKSSL} (\code{SoilProfileCollection}
#' object) is stored in the named element "SPC". The additional elements are
#' basic morphologic data: soil color, rock fragment volume, pores, structure,
#' and redoximorphic features. There is a 1:many relationship between the
#' horizon data in "SPC" and the additional dataframes in \code{morph}. See
#' examples for ideas on how to "flatten" these tables.
#' 
#' When \code{returnGeochemicalData} is TRUE, the resulting object is a list.
#' The standard output from \code{fetchKSSL} (\code{SoilProfileCollection}
#' object) is stored in the named element "SPC". The additional elements are
#' geochemical and mineralogy analysis tables, specifically:
#' geochemical/elemental analyses "geochem", optical mineralogy "optical", and
#' X-ray diffraction / thermal "xrd_thermal". \code{returnGeochemicalData} will
#' include additional dataframes \code{geochem}, \code{optical}, and
#' \code{xrd_thermal} in list result.
#' 
#' Setting \code{simplifyColors=TRUE} will automatically flatten the soil color
#' data and join to horizon level attributes.
#' 
#' Function arguments (\code{series}, \code{mlra}, etc.) are fully vectorized
#' except for \code{bbox}.
#' 
#' @param series vector of soil series names, case insensitive
#' @param bbox a single bounding box in WGS84 geographic coordinates e.g.
#' \code{c(-120, 37, -122, 38)}
#' @param mlra vector of MLRA IDs, e.g. "18" or "22A"
#' @param pedlabsampnum vector of KSSL pedon lab sample number
#' @param pedon_id vector of user pedon ID
#' @param pedon_key vector of KSSL internal pedon ID
#' @param returnMorphologicData logical, optionally request basic morphologic
#' data, see details section
#' @param returnGeochemicalData logical, optionally request geochemical,
#' optical and XRD/thermal data, see details section
#' @param simplifyColors logical, simplify colors (from morphologic data) and
#' join with horizon data
#' @param progress logical, optionally give progress when iterating over
#' multiple requests
#' @return a \code{SoilProfileCollection} object when
#' \code{returnMorphologicData} is FALSE, otherwise a list.
#' @note SoilWeb maintains a snapshot of these KSSL and NASIS data. The SoilWeb
#' snapshot was developed using methods described here:
#' \url{https://github.com/dylanbeaudette/process-kssl-snapshot}. Please use
#' the link below for the live data.
#' @author D.E. Beaudette and A.G. Brown
#' @seealso \code{\link{fetchOSD}}
#' @references \url{http://ncsslabdatamart.sc.egov.usda.gov/}
#' @keywords utilities
#' @examples
#' 
#' \donttest{
#' if(requireNamespace("curl") &
#'     curl::has_internet()) {
#'     
#'     library(aqp)
#'     
#'     # search by series name
#'     s <- fetchKSSL(series='auburn')
#'     
#'     # search by bounding-box
#'     # s <- fetchKSSL(bbox=c(-120, 37, -122, 38))
#'     
#'     # how many pedons
#'     length(s)
#'     
#'     # plot 
#'     plotSPC(s, name='hzn_desgn', max.depth=150)
#'     
#'     ##
#'     ## morphologic data
#'     ##
#'     
#'     # get lab and morphologic data
#'     s <- fetchKSSL(series='auburn', returnMorphologicData = TRUE)
#'     
#'     # extract SPC
#'     pedons <- s$SPC
#'     
#'     ## automatically simplify color data
#'     s <- fetchKSSL(series='auburn', returnMorphologicData = TRUE, simplifyColors=TRUE)
#'     
#'     # check
#'     par(mar=c(0,0,0,0))
#'     plot(pedons, color='moist_soil_color', print.id=FALSE)
#'     
#' }
#' }
#' 
#' @export fetchKSSL
fetchKSSL <- function(series=NA, bbox=NA, mlra=NA, pedlabsampnum=NA, pedon_id=NA, pedon_key=NA, returnMorphologicData=FALSE, returnGeochemicalData=FALSE, simplifyColors=FALSE, progress=TRUE) {
  
  if(!requireNamespace('jsonlite', quietly=TRUE))
    stop('please install the `jsonlite` packages', call.=FALSE)
  
  # convert BBOX into text representation
  if(!missing(bbox)) {
    
    # invalid BBOX
    if( length(bbox) != 4) {
      stop('invalid BBOX')
    }
    
    # convert BBOX into text representation
    # not vectorized, would require a different kind of input
    bbox <- paste(bbox, collapse=',')
  }
  
  
  # create argument matrix
  arg <- expand.grid(
    series = series,
    bbox = bbox,
    mlra = mlra,
    pedlabsampnum = pedlabsampnum,
    pedon_id = pedon_id,
    pedon_key = pedon_key
  )
  
  if (all(is.na(arg))) {
    stop("must specify series, bbox, mlra, pedlabsampnum, pedon_id, or pedon_key argument",
         call. = FALSE)
  }
  
  # number of unique argument sets (rows in matrix)
  n.arg.sets <- nrow(arg)
  
  # list to store results
  res <- vector(mode = 'list', length = n.arg.sets)
  
  # disable progress bar when number of argument sets (calls to .fetchSingle_KSSL) is 1
  if(n.arg.sets == 1) {
    progress <- FALSE
  }
  
  # allow toggling of progress bar
  if(progress) {
    pb <- txtProgressBar(min = 0, max = n.arg.sets, style = 3)
  }
  
  # iterate over argument set
  for(i in 1:n.arg.sets) {
    # build single URL filter
    f <- with(
      arg[i, ], 
      .buildFilter(series, bbox, mlra, pedlabsampnum, pedon_id, pedon_key)
    )
    
    # process a single request
    req.i <- .fetchSingle_KSSL(f, returnMorphologicData, returnGeochemicalData)
    
    # setting a list element to NULL effectively removes it
    if(!is.null(req.i)) {
      res[[i]] <- req.i
    }
    
    if(progress) {
      setTxtProgressBar(pb, i)
    }
  }
  
  if(progress) {
    close(pb)
    rm(pb)
  }
  
  
  ## TODO: enforce unique-ness in results: unique.SPC and unique.data.frame on extended data
  
  ## make composite SPC and optionally additional parts
  
  # simple request, result is a list of SPCs
  if(!returnMorphologicData & !returnGeochemicalData) {
    h <- aqp::pbindlist(res)
    
    # NO site/hz data, stop here
    if(is.null(h)) {
      message('query returned no data')
      return(NULL)
    }
    
  } else {
    # complex request, result is a list of lists
    # SPC
    h <- aqp::pbindlist(lapply(res, '[[', 'SPC'))
    
    # NO site/hz data, stop here
    if(is.null(h)) {
      message('query returned no data')
      return(NULL)
    }
    
    ## TODO enforce unique-ness on SPC here
    
    ## NOTE: simpler with purrr::transpose()
    # morph
    if(returnMorphologicData) {
      # add new tables here
      v <- c("phcolor", "phfrags", "phpores", "phstructure", "pediagfeatures", "rmf")
      m <- vector(mode = 'list')
      # iterate over tables and unwind - rbind - store
      for(i in v) {
        m[[i]] <- do.call('rbind', lapply(res, function(j) j$morph[[i]]))
        
        ## TODO enforce unique-ness on data.frames here
      }
    }
    
    # geochem: may be missing for a lot of labsampnum
    if(returnGeochemicalData) {
      
      # safely combine + fill missing columns
      geochem <- do.call('rbindlist', args = list(l = lapply(res, '[[', 'geochem'), fill = TRUE))
      optical <- do.call('rbindlist', args = list(l = lapply(res, '[[', 'optical'), fill = TRUE))
      xrd_thermal <- do.call('rbindlist', args = list(l = lapply(res, '[[', 'xrd_thermal'), fill = TRUE))
      
      # convert back to data.frame
      geochem <- as.data.frame(geochem)
      optical <- as.data.frame(optical)
      xrd_thermal <- as.data.frame(xrd_thermal)
      
      ## TODO enforce unique-ness on data.frames here
    }
    
  }
  
  
  # do NOT set KSSL-specific horizon identifier
  # labsampnum is NOT unique 0.1% of the time AGB 2019/11/14
  # hzidname(h) <- "labsampnum"
  
  # set KSSL-specific hzdesgn/hztexcl fields
  hzdesgnname(h) <- "hzn_desgn"
  hztexclname(h) <- "lab_texture_class"
  
  ## set metadata
  # TODO: check before clobbering / consider standard var name
  metadata(h)$origin <- 'KSSL via Soilweb / fetchKSSL'
  
  
  # cleaning up the results
  if(returnMorphologicData & simplifyColors) {
    
    if(inherits(m$phcolor, 'data.frame')) {
      
      # simplify color data: 1 row / horizon, from morphologic data tables
      x.colors <- simplifyColorData(m$phcolor, id.var = 'labsampnum', wt='colorpct')
      
      # safely LEFT JOIN with @horizons   
      suppressMessages(horizons(h) <- x.colors)
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

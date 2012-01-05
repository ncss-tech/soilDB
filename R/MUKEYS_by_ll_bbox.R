
# http://sdmdataaccess.nrcs.usda.gov/Spatial/SDMNAD83Geographic.wfs?Service=WFS&Version=1.0.0&Request=GetFeature&OutputFormat=XmlMukeyList&Typename=MapunitPolyNoGeometry&BBOX=-120.950129388,37.7972571005,-120.677685495,37.9766971606

MUKEYS_by_ll_bbox <- function(bbox) {
  # check for required packages
  if(!require(RCurl) | !require(XML))
    stop('please install the `RCurl` and `XML` packages')
  
  bbox.text <- paste(bbox, collapse=',')
  
  u <- paste( 'http://sdmdataaccess.nrcs.usda.gov/Spatial/SDMNAD83Geographic.wfs?Service=WFS&Version=1.0.0&Request=GetFeature&OutputFormat=XmlMukeyList&Typename=MapunitPolyNoGeometry&BBOX=', bbox.text, sep='')
  
  html <- getURL(u) 
  html.tree <- htmlTreeParse(html, useInternalNodes=TRUE, error=function(...){}) 

  m <- getNodeSet(html.tree, '//mapunitkeylist')
  m.val <- gsub("'", '', xmlValue(m[[1]]))
  m.keys <- strsplit(m.val, ',')[[1]]
  
  return(m.keys)
  }


## Coercition methods: general
setAs("SoilProfileCollection", "data.frame", function(from) {
  
  # horizons + site + coordinates
  if(nrow(site(from)) > 0 & nrow(coordinates(from)) == length(from)) {
    site.coords <- data.frame(site(from), coordinates(from), stringsAsFactors=FALSE)
    return(join(horizons(from), site.coords, by=idname(from)))
  }
  
  # horizons + site
  if(nrow(site(from)) > 0 & ! nrow(coordinates(from)) == length(from))
    return(join(horizons(from), site(from), by=idname(from)))
    
  # horizons + coordinates
  if(! nrow(site(from)) > 0 & nrow(coordinates(from)) == length(from)) {
    ids.coords <- data.frame(profile_id(from), coordinates(from), stringsAsFactors=FALSE)
    return(data.frame(horizons(from), ids.coords, stringsAsFactors=FALSE))
  }
  
  # just horizons
  else {
    return(horizons(from))
  }
  
}
)

## TODO: why does the proj4string get mangled in the conversion?
## Coercition methods: and sp utilities
setAs("SoilProfileCollection", "SpatialPointsDataFrame", function(from) {
    message('only site data are extracted')
    s <- SpatialPointsDataFrame(coordinates(from), data = site(from), proj4string=CRS(proj4string(from)), match.ID = FALSE)
    return(s)
  }
)

## Coercition methods: and sp utilities
setAs("SoilProfileCollection", "SpatialPoints", function(from) {
    SpatialPoints(coordinates(from), proj4string=CRS(proj4string(from)))
  }
)

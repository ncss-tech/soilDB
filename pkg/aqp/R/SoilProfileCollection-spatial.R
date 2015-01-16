
##
## wrappers to spatial operations via sp, rgdal, raster
##

## Note: this should probably be done with proper S4 methods

# spTransform.SoilProfileCollection <- function(spc, ...) {
# 	spc@sp <- spTransform(spc@sp, ...)
# 	return(x)
# }
# 
# over.SoilProfileCollection <- function(spc, ...) {
# 	res <- over(spc@sp, ...)
# 	return(res)
# }
# 
# extract.SoilProfileCollection <- function(spc, x, ...) {
# 	res <- extract(spc@sp, ...)
# 	return(res)
# }



##
## proj4string setting
##

setMethod(f='proj4string', signature='SoilProfileCollection',
  function(obj){
    proj4string(obj@sp)
  }
)

setReplaceMethod("proj4string", "SoilProfileCollection",
  function(obj, value) {
  proj4string(obj@sp) <- value
  obj
  }
)

##
## initialize spatial data
##
setReplaceMethod("coordinates", "SoilProfileCollection",
  function(object, value) {

  # basic sanity check... needs work
  if(! inherits(value, "formula"))
    stop('invalid formula', call.=FALSE)

  # extract coordinates as matrix
  mf <- data.matrix(model.frame(value, site(object), na.action=na.pass))

  # test for missing coordinates
  mf.missing <- apply(mf, 2, is.na)

  if(any(mf.missing))
	  stop('cannot initialize a SpatialPoints object with missing coordinates', call.=FALSE)

  # assign to sp slot
  # note that this will clobber any existing spatial data
  object@sp <- SpatialPoints(coords=mf)
  
  # remove coordinates from source data
  # note that mf is a matrix, so we need to access the colnames differently
  coord_names <- dimnames(mf)[[2]]
  idx <- match(coord_names, names(site(object)))
  
  # remove the named site data from site_data
  # TODO we should use a proper setter!
  # bug fix c/o José Padarian: drop=FALSE
  object@site <- site(object)[, -idx, drop=FALSE]
  
  # done
  return(object)
  }
)





# ### TODO: consider removing this function
# 
# ##
# ## spatial_subset: spatial clipping of a SPC (requires GEOS)
# ##
# 
# if (!isGeneric("spatial_subset"))
#   setGeneric("spatial_subset", function(object, geom) standardGeneric("spatial_subset"))
# 
# setMethod(f='spatial_subset', signature='SoilProfileCollection',
#   function(object, geom){
# 
#     # This functionality require the GEOS bindings
#     # provided by rgeos
#     if(require(rgeos)) {
#       spc_intersection <- gIntersects(as(object, "SpatialPoints"), geom, byid = TRUE)
#       ids <- which(spc_intersection)
# 	
# 	# extract relevant info
# 	s <- site(object)
# 	h <- horizons(object)
# 	d <- diagnostic_hz(object)
#   
# 	# get indexes to valid site, hz, diagnostic data
#   valid_ids <- s[ids, idname(object)]
#   valid_horizons <- which(h[, idname(object)] %in% valid_ids)
#   valid_sites <- which(s[, idname(object)] %in% valid_ids)
#   valid_diagnostic <- which(d[, idname(object)] %in% valid_ids)
# 	
# 	# create a new SPC with subset data
#   ## TODO: copy over diagnostic horizon data
# 	## TODO: use integer profile index to simplify this process
# 	## TODO: @sp bbox may need to be re-computed
#   ## TODO: check diagnostic subset
#       SoilProfileCollection(idcol = object@idcol, depthcols = object@depthcols, metadata = metadata(object), horizons = h[valid_horizons, ], site = s[valid_sites, ], sp = object@sp[ids,], diagnostic = d[valid_diagnostic, ])
#     }
#     else { # no rgeos, return original
#       stop('Spatial subsetting not performed, please install the `rgeos` package.', call.=FALSE)
#     }
#   }
# )

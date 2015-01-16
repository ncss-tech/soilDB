# class def for main class within aqp
.SoilProfileCollectionValidity <- function(object) {
	
# 	# for now, test only for NA in horizon boundaries
# 	dc <- horizonDepths(object)
# 	h <- horizons(object)
# 	top <- dc[1]
#   bottom <- dc[2]
# 	
# 	if(any(c(is.na(h[[top]]), is.na(h[[bottom]])))) {
# 		msg <- 'horizon top and bottom values cannot contain NA'
# 		return(FALSE)
# 	}
	
	# otherwise, we are fine
	return(TRUE)
	
	
	## 2013-03-07: this should all be done outside of our class... too much testing slows things down
	
#     # 1. test for bad horizon logic
#     id <- idname(object)
#     h <- horizons(object)
#     dc <- horizonDepths(object)
#     top <- dc[1]
#     bottom <- dc[2]
#     
#     ## Note: this may be redundant, as checking is commonly done before init of object
#     # perform test: fails on missing horizon boundaries or overlapping horizons
#     # non-contiguous horizonation is allowed
#     obj.test <- ddply(h, id, test_hz_logic, topcol=top, bottomcol=bottom)
#     
#     # let the user know which profile IDs aren't going to work
#     if(any(obj.test$hz_logic_pass == FALSE)) {
#       bad.ids <- obj.test[[id]][obj.test$hz_logic_pass == FALSE]
#       msg <- paste('\n\nNOTICE: invalid horizon logic in:', paste(bad.ids, collapse=','), '\n')
#       return(msg)
#     }
      
  }

setClass(
  Class='SoilProfileCollection', 
  representation=representation(
    idcol='character', # column name containing IDs
    depthcols='character', # 2 element vector with column names for hz top, bottom
    metadata='data.frame', # single-row dataframe with key-value mapping
    horizons='data.frame', # all horizons sorted by ID, top
    site='data.frame', # data about the sampling sites
    sp='SpatialPoints', # (optional) spatial data stored here
    diagnostic='data.frame' # (optional) diagnostic horizons are stored here
  ),
  prototype=prototype(
    idcol='id',
    depthcols=c('top','bottom'),
    metadata=data.frame(stringsAsFactors=FALSE), # default units are unkown
    horizons=data.frame(stringsAsFactors=FALSE),
    site=data.frame(stringsAsFactors=FALSE),
    sp=new('SpatialPoints'),
    diagnostic=data.frame(stringsAsFactors=FALSE)
  ),
  validity=.SoilProfileCollectionValidity
)


# analog to apply():
# operates by profile, returns vector of length(object) or nrow(object)
if (!isGeneric("profileApply"))
 	setGeneric("profileApply", function(object, FUN, simplify=TRUE, ...) standardGeneric("profileApply"))

setMethod(f='profileApply', signature='SoilProfileCollection',
	function(object, FUN, simplify=TRUE, ...) {
						
		# get profile IDs
		pIDs <- profile_id(object)
					
		# init empty list
		l <- list()
						
		# iterate over SPC, spliting into a list of SPC_i ... SPC_n
		for(i in seq_along(pIDs)) {
			pID <- pIDs[i]
			l[[pID]] <- do.call(FUN, list(object[i, ], ...))
		}
		
		# optionally simplify
		if(simplify) {
			res <- unlist(l)
			return(res)
		}
		else
			return(l)
	}
)







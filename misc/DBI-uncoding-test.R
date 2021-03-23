# uncoding from a static local NASIS instance
library(aqp)
library(soilDB)
library(tibble)

dsn <- NULL #"~/workspace/NASISlite/nasis_local.db"
SS <- FALSE
stringsAsFactors <- FALSE

# check connection
local_NASIS_defined(dsn = dsn)

# create connection
conn <- NASIS(dsn)

# bare uncode against a local db
tibble(head(uncode(dbQueryNASIS(conn,
                                "SELECT phiid, bounddistinct, boundtopo FROM phorizon"),
                                dsn = dsn)))

# fetchNASIS test
f <- fetchNASIS(dsn = dsn)

# verify codes properly converted to labels
f

# get_comonth with fill=TRUE

get_comonth_from_NASIS_db(fill = TRUE, dsn = dsn)

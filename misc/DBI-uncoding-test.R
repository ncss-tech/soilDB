# uncoding from a static local NASIS instance
library(aqp)
library(soilDB)
library(tibble)

static_path <- NULL #"~/workspace/NASISlite/nasis_local.db"
SS <- FALSE
stringsAsFactors <- FALSE

# check connection
local_NASIS_defined(static_path = static_path)

# create connection
conn <- NASIS(static_path)

# bare uncode against a local db
tibble(head(uncode(dbQueryNASIS(conn,
                                "SELECT phiid, bounddistinct, boundtopo FROM phorizon"),
                                static_path = static_path)))

# fetchNASIS test
f <- fetchNASIS(static_path = static_path)

# verify codes properly converted to labels
f

# get_comonth with fill=TRUE

get_comonth_from_NASIS_db(fill = TRUE, static_path = static_path)

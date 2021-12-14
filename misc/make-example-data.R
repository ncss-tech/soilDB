# make SPC data sets from NASIS
library(aqp)
library(soilDB)

# load current data sets to fetch peiid
data("loafercreek", package = "soilDB")
data("gopheridge", package = "soilDB")
data("mineralKing", package = "soilDB")

# query CA630 and CA792 w/ R08 PEDON/SITE by SSA ID or similar

# load target data sets (CA630 and CA792 pedons)
recent1822a <- fetchNASIS(rmHzErrors = FALSE, SS = FALSE)

# subset 
loafercreek2 <- subset(recent1822a, profile_id(recent1822a) %in% profile_id(loafercreek))
gopheridge2 <- subset(recent1822a, profile_id(recent1822a) %in% profile_id(gopheridge))
mineralKing2 <- subset(recent1822a, profile_id(recent1822a) %in% profile_id(mineralKing))

# ensure that phiid is set as hzID 
hzidname(loafercreek2) <- "phiid"
hzidname(gopheridge2) <- "phiid"
hzidname(mineralKing2) <- "phiid"

# verify completeness
if (all(profile_id(loafercreek) %in% profile_id(loafercreek2)))
  loafercreek <- loafercreek2

if (all(profile_id(gopheridge) %in% profile_id(gopheridge2)))
  gopheridge <- gopheridge2

if (all(profile_id(mineralKing) %in% profile_id(mineralKing2)))
  mineralKing <- mineralKing2

# save to .rda
save(loafercreek, file = "data/loafercreek.rda")
save(gopheridge, file = "data/gopheridge.rda")
save(mineralKing, file = "data/mineralKing.rda")

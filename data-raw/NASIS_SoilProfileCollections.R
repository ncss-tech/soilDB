# make SPC data sets from NASIS
library(aqp)
library(soilDB)

# load current data sets to fetch peiid
data("loafercreek", package = "soilDB")
data("gopheridge", package = "soilDB")
data("mineralKing", package = "soilDB")

# # create CSVs (requires NASIS setup)
# # query CA630 and CA792 w/ R08 PEDON/SITE by SSA ID or similar
# # load source data sets (CA630 and CA792 pedons, including lab pedons)
nasis_pedons <- fetchNASIS(rmHzErrors = FALSE, SS = FALSE)

p <- rebuildSPC(subset(nasis_pedons, siteiid %in% site(c(loafercreek, gopheridge, mineralKing))$siteiid))
write.csv(horizons(p), "data-raw/spc-horizons.csv", row.names = FALSE)
write.csv(site(p), "data-raw/spc-site.csv", row.names = FALSE)
write.csv(diagnostic_hz(p), "data-raw/spc-diagnostic_hz.csv", row.names = FALSE)
write.csv(restrictions(p), "data-raw/spc-restrictions.csv", row.names = FALSE)

recent1822a <- read.csv("data-raw/spc-horizons.csv")
depths(recent1822a) <- peiid ~ hzdept + hzdepb

site(recent1822a) <- read.csv("data-raw/spc-site.csv")
diagnostic_hz(recent1822a) <- read.csv("data-raw/spc-diagnostic_hz.csv")
restrictions(recent1822a) <- read.csv("data-raw/spc-restrictions.csv")

# TODO: PATCH for obs_date
# recent1822a$obs_date <- recent1822a$obsdate
# recent1822a$site_id <- recent1822a$usiteid

# ensure that phiid is set as hzID 
hzidname(recent1822a) <- "phiid"
hzdesgnname(recent1822a) <- "hzname"
hztexclname(recent1822a) <- "texcl"

# subset 
loafercreek2 <- rebuildSPC(subset(recent1822a, profile_id(recent1822a) %in% profile_id(loafercreek)))
gopheridge2 <- rebuildSPC(subset(recent1822a, profile_id(recent1822a) %in% profile_id(gopheridge)))
mineralKing2 <- rebuildSPC(subset(recent1822a, profile_id(recent1822a) %in% profile_id(mineralKing)))

loafercreek2$hzID <- loafercreek$hzID
gopheridge2$hzID <- gopheridge2$hzID
mineralKing2$hzID <- mineralKing2$hzID

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
usethis::use_data(loafercreek, overwrite = TRUE, compress = 'xz')
usethis::use_data(gopheridge, overwrite = TRUE, compress = 'xz')
usethis::use_data(mineralKing, overwrite = TRUE, compress = 'xz')

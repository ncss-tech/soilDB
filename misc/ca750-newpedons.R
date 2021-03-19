# new CA750 data
library(soilDB)
library(sharpshootR)
library(sf)

s.sub <- soilDB::createStaticNASIS("site_View_1")[["site_View_1"]][, c(
  "usiteid",
  "plssmeridian",
  "plsstownship",
  "plssrange",
  "plsssection",
  "plsssdetails",
  "elev",
  "slope",
  "aspect"
)]

colnames(s.sub) <- c("id",
                     "m","t","r","s","plsssdetails",
                     "elev", "slope", "aspect")
s.sub <- subset(s.sub, !grepl("meters", plsssdetails))
s.sub$m <- "CA21"
s.sub$type <- "SN"

is_corner <- grepl("corner$|section$|corner of$", s.sub$plsssdetails)
corners <- gsub("(.*) corner$|(.*) section$|(.*) corner of$", "\\1\\2\\3", s.sub$plsssdetails)

s.sub$qq <- sapply(strsplit(s.sub$plsssdetails, ","), function(x) trimws(x[1]))
s.sub$q <- sapply(strsplit(s.sub$plsssdetails, ","), function(x) trimws(x[2]))

s.complete <- s.sub[complete.cases(s.sub) & 
                      !s.sub$t == "NA" &
                      !s.sub$r == "NA" & !(s.sub$r == "E") &
                      !s.sub$s == "NA" &
                      !s.sub$q == "NA",]

s.complete$t <- paste0("T", trimws(s.complete$t))
s.complete$r <- paste0("R", trimws(s.complete$r))

spad <- paste0(ifelse(nchar(s.complete$s) == 0, "", "00"), s.complete$s)

s.complete$s <- substr(spad, nchar(spad)-1, nchar(spad))
s.complete$plssid <- sharpshootR::formatPLSS(p = s.complete)
s.complete$plssid <- gsub("(.*A)\\d+ meters.*", "\\1", s.complete$plssid)
s.complete <- s.complete[!grepl("NA", s.complete$plssid),]

# fix(s.complete)
res <- PLSS2LL(s.complete)

pts <- st_as_sf(res[complete.cases(res),], coords = c("lon", "lat"), 
                crs = st_crs(4326))
bdy <- fetchSDA_spatial("CA750", geom.src = "sapolygon")
plot(st_as_sf(bdy)$geometry)
plot(pts, add=T)

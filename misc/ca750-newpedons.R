# new CA750 data
library(soilDB)
library(sharpshootR)
library(sf)
library(raster)

# use DBI connection to create a NASIS table list object and select some columns
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

# rename some columns to match PLSS2LL() data.frame format
colnames(s.sub) <- c("id","m","t","r","s","plsssdetails",
                     "elev", "slope", "aspect")

# add meridian section style etc
s.sub$plsssdetails[grepl("meters", s.sub$plsssdetails)] <- ""
s.sub$qq <- ""#sapply(strsplit(s.sub$plsssdetails, ","), function(x) trimws(x[1]))
s.sub$q <- ""#sapply(strsplit(s.sub$plsssdetails, ","), function(x) trimws(x[2]))

s.sub$m <- "CA21"
s.sub$type <- "SN"

is_corner <- grepl("corner$|section$|corner of$", s.sub$plsssdetails)
corners <- gsub("(.*) corner$|(.*) section$|(.*) corner of$", "\\1\\2\\3", s.sub$plsssdetails)

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

# convert the longlat points from PLSS to sf
pts <- st_as_sf(res[complete.cases(res),], 
                coords = c("lon", "lat"), 
                crs = st_crs(4326))

# get SSA polygon
bdy <- fetchSDA_spatial("CA750", geom.src = "sapolygon")

# get raster of mukeys
mukeyras <- mukey.wcs(ssa)

# get data from SDA
ssurgo <- SDA_query(sprintf(
                    "SELECT * FROM legend 
                    INNER JOIN mapunit ON legend.lkey = mapunit.lkey
                    WHERE mukey IN %s", format_SQL_in_statement(unique(values(ras)))))

# get just target survey area
ssurgo.sub <- subset(ssurgo, areasymbol == "CA750")

# get the numbers only out
ssurgo.sub$musym <- as.numeric(gsub("(\\d+)|.*","\\1", ssurgo.sub$musym))

# new raster attribute table based on numeric musyms used in (most of) CA750
rat <- merge(levels(mukeyras)[[1]], ssurgo.sub[,c('mukey',"musym")], 
             by.x = 'ID', by.y = 'mukey', 
             sort = FALSE, all.x = TRUE, incomparables = NA)
levels(mukeyras) <- rat

# make a map
plot(mukeyras, "musym")
plot(st_transform(st_as_sf(bdy), crs(mukeyras))$geometry, add = TRUE)
plot(st_transform(pts, crs(mukeyras))$geometry, add = TRUE)

library(rvest)
library(sf)
data("SCAN_SNOTEL_metadata")

# using site codes with known pedon ID, getting pedlabsampnum
nssc_report_url <- function(site.code) {
  x <- paste0("https://wcc.sc.egov.usda.gov/nwcc/pedon?sitenum=", site.code)
  lnk <- read_html(x) |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  lnk <- lnk[grep("http://ncsslabdatamart.sc.egov.usda.gov/rptExecute.aspx",
             lnk, fixed = TRUE)][1]
  pid <- gsub(".*\\.aspx\\?p=(\\d+).*", "\\1", lnk)
  
  tf <- tempfile()
download.file(lnk, destfile = tf)
  lns <- readLines(tf)
  lns <- lns[grep("Pedon No.", lns)[1]]
  lns <- gsub(".*Pedon No\\..*size=\"1\">(.*)</FONT>.*Kellogg Soil Survey Laboratory.*", "\\1", lns)
  return(data.frame(site.code = site.code, pedon_key = pid, pedlabsampnum = lns))
}

x <- soilDB::SCAN_site_metadata() |> 
  subset(Network == "SCAN", select = c(Site, upedonid, pedlabsampnum)) |> 
  (\(x) { subset(x, !is.na(x$upedonid) & !complete.cases(x)) })()

newx <- unique(do.call('rbind', lapply(x$Site, nssc_report_url)))
lut <- newx$pedlabsampnum
names(lut) <- newx$site.code
na.idx <- is.na(x$pedlabsampnum)
x$pedlabsampnum[na.idx] <- lut[as.character(x$Site)]
SCAN_SNOTEL_metadata$pedlabsampnum[match(x$Site, SCAN_SNOTEL_metadata$Site)] <- x$pedlabsampnum
SCAN_SNOTEL_metadata$pedlabsampnum[647] <- lut[["2178"]]

SCAN_SNOTEL_metadata |> 
  subset(Network == "SCAN", select = c(Site, upedonid, pedlabsampnum)) |> 
  (\(x) { subset(x, !is.na(x$upedonid) & !complete.cases(x)) })()

write.csv(SCAN_SNOTEL_metadata, "misc/scan-snotel-data/station-metadata.csv", row.names = FALSE)
save(SCAN_SNOTEL_metadata, file = "data/SCAN_SNOTEL_metadata.rda", compress = "xz")


x <- soilDB::SCAN_site_metadata() |> 
  subset(Network == "SCAN", select = c(Site, upedonid, pedlabsampnum)) |> 
  (\(x) { subset(x, !is.na(x$pedlabsampnum) & !complete.cases(x)) })()
# none


# TODO: find all KSSL pedons within 10km, take closest and then do QC
get_nearest_KSSL <- function(x, y, maxdist = 10000) {
  pt <- sf::st_as_sf(data.frame(x = x, y = y),
                     coords = letters[24:25],
                     crs = 4326)
  bb <- sf::st_bbox(sf::st_buffer(pt, dist = maxdist))
  # TODO: are some of these pedons missing from SoilWeb snapshot??
  res <- fetchKSSL(bbox = c(bb[3], bb[2], bb[1], bb[4]))
  if (is.null(res))
    return(res)
  res2 <- sf::st_as_sf(aqp::site(res),
                  coords = letters[24:25],
                  crs = 4326)
  res2$distance <- sf::st_distance(pt, res2)[1,]
  res2[which.min(res2$distance)[1],]
}
y <- soilDB::SCAN_site_metadata() |>
  subset(Network == "SCAN", select = c(Site, Longitude, Latitude, upedonid, pedlabsampnum)) |>
  (\(y) { subset(y, is.na(y$upedonid)) })()
y <- split(y, seq_len(nrow(y)))

newy <- do.call('rbind', lapply(y, function(x) {
    xx <- get_nearest_KSSL(x$Longitude, x$Latitude)
    data.frame(Site = x$Site[nrow(xx)], xx)
  }))
# 
# # sites missing KSSL pedon with a KSSL pedon within 500m
subset(newy[, c("Site", "pedon_id", "pedon_key", "pedlabsampnum", "distance")],
       as.numeric(distance) < 500)

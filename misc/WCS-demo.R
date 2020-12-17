library(aqp)
library(soilDB)
library(reshape2)

library(sp)
library(raster)
library(rasterVis)

# note that grid resolution isn't quite right

# BBOX in WGS84 coordinates
a <- c(-121,37,-120,38)

# attempt in AEA ~ 800m
pH_05cm <- ISSR800.wcs(var = 'ph_05cm', aoi = a)
pH_3060cm <- ISSR800.wcs(var = 'ph_3060cm', aoi = a)

clay_05cm <- ISSR800.wcs(var = 'clay_05cm', aoi = a)
clay_3060cm <- ISSR800.wcs(var = 'clay_3060cm', aoi = a)

drainage_class <- ISSR800.wcs(var = 'drainage_class', aoi = a)
weg <- ISSR800.wcs(var = 'weg', aoi = a)
str <- ISSR800.wcs(var = 'str', aoi = a)

# attempt in GCS ~ 600m
pH_05cm.gcs <- ISSR800.wcs(var = 'ph_05cm', aoi = a, res = 0.004, crs = 'EPSG:4326')
drainage_class.gcs <- ISSR800.wcs(var = 'drainage_class', aoi = a, res = 0.004, crs = 'EPSG:4326')
weg.gcs <- ISSR800.wcs(var = 'weg', aoi = a, res = 0.004, crs = 'EPSG:4326')



# AEA
levelplot(stack(pH_05cm, pH_3060cm), margin = FALSE)

levelplot(stack(clay_05cm, clay_3060cm), margin = FALSE)

levelplot(drainage_class, margin = FALSE)
levelplot(weg, margin = FALSE)
levelplot(str, margin = FALSE)

# GCS
levelplot(pH_05cm.gcs, margin = FALSE)
levelplot(drainage_class.gcs, margin = FALSE)
levelplot(weg.gcs, margin = FALSE)


##
## gNATSGO
##

# resampled to ~300m
gn.300m <- mukey.wcs(var = 'gnatsgo', aoi = a, res = 300)

# native ~ 30m
gn.30m <- mukey.wcs(var = 'gnatsgo', aoi = a, res = 30)

# AEA
levelplot(gn.300m, att = 'ID', margin = FALSE, colorkey = FALSE)


## overly-simplistic aggregation of tabular data from SDA

# get unique mukey
ll <- levels(gn.300m)[[1]]
IS <- format_SQL_in_statement(ll$ID)

# query SDA by mukey
ws <- sprintf("mukey IN %s", IS)
x <- fetchSDA(WHERE = ws, duplicates = TRUE, droplevels = TRUE, stringsAsFactors = FALSE)


# component level aggregation
x.a <- slab(x, cokey ~ ph1to1h2o_r + claytotal_r, slab.structure = c(0, 5), slab.fun = mean, na.rm = TRUE)

# long -> wide format
w <- dcast(x.a, cokey ~ variable, value.var = 'value')

# check: ok
head(w)

# MU-level aggregation / subset
s <- site(x)[, c('mukey', 'cokey', 'comppct_r')]
s <- merge(s, w, by = 'cokey', sort = FALSE)


# STATSGO map unit
# l <- split(s, s$mukey)
# i <- i <- l[['660849']]

## TODO: generalize
# component percentage weighted mean
agg.data <- lapply(
  split(s, s$mukey), function(i, var = 'ph1to1h2o_r') {
    # remove NA first
    i <- na.omit(i)
    # weighted mean
    wm <- sum(i[[var]]* i$comppct_r) / sum(i$comppct_r)
    
    # pack results
    res <- data.frame(
      mukey = i$mukey[1],
      var = wm,
      stringsAsFactors = FALSE
    )
    
    # re-name for convenience later
    names(res)[2] <- var
    
    return(res)
  })

# list -> DF
agg.data <- do.call('rbind', agg.data)

# NA present?
# if so, does it matter?

# join into RAT
rat <- merge(ll, agg.data, by.x = 'ID', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# re-pack RAT
levels(gn.300m) <- rat

# convert to raster of values via RAT
gn.300m.pH_05cm <- deratify(gn.300m, att = 'ph1to1h2o_r')

# not too bad
levelplot(gn.300m.pH_05cm, margin = FALSE, main = 'gNATSGO 1:1 H2O pH 0-5cm')


## this has to be done with identical grid topology
# compare
pH_05cm <- ISSR800.wcs(var = 'ph_05cm', aoi = a, res = 300)

rs <- stack(pH_05cm, gn.300m.pH_05cm)
names(rs) <- c('ISSR-800', 'gNATSGO')

levelplot(rs, margin = FALSE, main = '1:1 H2O pH 0-5cm', scales = list(draw = FALSE), maxpixels = 1e6)



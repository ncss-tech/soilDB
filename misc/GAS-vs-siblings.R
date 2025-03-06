library(aqp)
library(soilDB)
library(venn)

s <- 'marshall'
o <- fetchOSD(s, extended = TRUE)

# if missing, `geog_assoc_soils` is FALSE --> should probably be an empty DF
gas <- o$geog_assoc_soils$gas
sib <- siblings(s)
cous <- siblings(s, cousins = TRUE)

# unique vectors / normalized to upper case
sib.all <- toupper(unique(sib$sib$sibling))
# just siblings that are major components
sib.mag <- toupper(unique(sib$sib$sibling[sib$sib$majcompflag]))
# cousins that are major components
cousins <- toupper(unique(cous$cousins$sibling[cous$cousins$majcompflag]))

# d.all <- data.frame(series = sib.all)
# d.maj <- data.frame(series = sib.mag)
# d.gas <- data.frame(series = gas)

l <- list(
  `Geog.\nAssoc. Soils\n` = gas,
  `Siblings\n(All)` = sib.all,
  `Siblings\n(Maj. Components)` = sib.mag
)

par(mar = c(0, 0, 2, 0))
venn(l, ellipse = FALSE, ilcs = 1, sncs = 0.66, par = FALSE, zcolor = 'style', box = FALSE, ilabels = 'counts')
title(toupper(s))


l <- list(
  `Geog.\nAssoc. Soils\n` = gas,
  `Siblings\n(All)` = sib.all,
  `Siblings\n(Maj. Components)` = sib.mag,
  `Competing\nSeries` = o$competing$competing
)

par(mar = c(0, 0, 2, 0))
venn(l, ellipse = TRUE, ilcs = 1, sncs = 0.66, par = FALSE, zcolor = 'style', box = FALSE, ilabels = 'counts')
title(toupper(s))


l <- list(
  `Geog.\nAssoc. Soils\n` = gas,
  `Cousins\n(Maj. Components)` = cousins,
  `Siblings\n(All)` = sib.all,
  `Siblings\n(Maj. Components)` = sib.mag,
  `Competing\nSeries` = o$competing$competing
)

par(mar = c(0, 0, 2, 0))
venn(l, ellipse = TRUE, ilcs = 1, sncs = 0.66, par = FALSE, zcolor = 'style', box = FALSE, ilabels = 'counts')
title(toupper(s))


l <- list(
  `Geog.\nAssoc. Soils\n` = gas,
  `Siblings\n(All)` = sib.all,
  `Competing\nSeries` = o$competing$competing
)

par(mar = c(0, 0, 2, 0))
venn(l, ellipse = TRUE, ilcs = 1, sncs = 0.66, par = FALSE, zcolor = 'style', box = FALSE, ilabels = 'counts')
title(toupper(s))


## TODO: how to quantify overlap across conceptual classification systems?


# search for series with no specified GAS

o <- OSDquery(geog_assoc_soils = 'none')


x <- fetchOSD('KIEV', extended = TRUE)
x$geog_assoc_soils

siblings('kiev')

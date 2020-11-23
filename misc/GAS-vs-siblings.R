library(aqp)
library(soilDB)
library(venn)

s <- 'drummer'
o <- fetchOSD(s, extended = TRUE)

# if missing, `geog_assoc_soils` is FALSE --> should probably be an emtpy DF
gas <- o$geog_assoc_soils$gas
sib <- siblings(s)
cous <- siblings(s, cousins = TRUE)

sib.all <- toupper(unique(sib$sib$sibling))
sib.mag <- toupper(unique(sib$sib$sibling[sib$sib$majcompflag]))

cousins <- toupper(unique(cous$cousins$sibling))

l <- list(
  `Geog.\nAssoc. Soils\n` = gas,
  `Siblings (All)` = sib.all,
  `Siblings (MC)` = sib.mag
)

par(mar = c(0, 0, 2, 0))
venn(l, ellipse = FALSE, ilcs = 1, par = FALSE)
title(toupper(s))


l <- list(
  `Geog.\nAssoc. Soils\n` = gas,
  `Siblings (All)` = sib.all,
  `Siblings (MC)` = sib.mag,
  `Competing` = o$competing$competing
)

par(mar = c(0, 0, 2, 0))
venn(l, ellipse = TRUE, ilcs = 1, par = FALSE)
title(toupper(s))


l <- list(
  `Geog.\nAssoc. Soils\n` = gas,
  `Cousins` = cousins,
  `Siblings (All)` = sib.all,
  `Siblings (MC)` = sib.mag,
  `Competing` = o$competing$competing
)

par(mar = c(0, 0, 2, 0))
venn(l, ellipse = TRUE, ilcs = 1, par = FALSE)
title(toupper(s))


# search for series with no specified GAS

o <- OSDquery(geog_assoc_soils = 'none')


x <- fetchOSD('KIEV', extended = TRUE)
x$geog_assoc_soils

siblings('kiev')

library(soilDB)
library(aqp)

# get roots for all pedon/horizon records from selected set
r <- get_phroots_from_NASIS_db()

# review results
table(r$rootsquantityclass)
table(r$rootslocation)
table(r$rootssize)

head(r, 20)
str(r)

# think about how one might reduce this down to 1 row / horizon

# really lame approach: sum total root quantity by horizon
a <- aggregate(r, rootsquantity ~ phiid, FUN = sum, na.rm = TRUE)

# check
nrow(a)
head(a)


# get pedons from seleced set
p <- fetchNASIS()

p <- HzDepthLogicSubset(p)


# merge aggreated root quantity into horizons, by phiid
horizons(p) <- a

# horizons without roots described imply 0 roots
p$rootsquantity[is.na(p$rootsquantity)] <- 0

# plot some of the data
par(mar = c(0, 0, 0, 3))
plotSPC(p[1:15, ], width = 0.33, name.style = 'center-center', label = 'upedonid')


# evaluate the top 100cm via truncation
x <- trunc(p, 0, 100)

par(mar = c(0, 0, 3, 3))
plotSPC(x, width = 0.4, name = NA, print.id = FALSE, color = 'rootsquantity', lwd = 0)





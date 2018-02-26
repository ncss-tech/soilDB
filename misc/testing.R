library(soilDB)
sessionInfo()


x <- fetchNASIS(what='pedons')
x.all <- fetchNASIS(what='pedons', SS=FALSE)

length(x)
length(x.all)


x <- fetchNASIS(what='components')
x.all <- fetchNASIS(what='components', SS=FALSE)


length(x)
length(x.all)


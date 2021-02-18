library(soilDB)


## outstanding TODO: `everything` API end point returns extra output we don't need


res <- OSDquery(everything = "''Auburn, Alabama'' & plinthite")
head(res)


res <- OSDquery(everything = "flood & plains & toe & slope")
head(res)


## quick check, these should give very similar results
OSDquery(brief_narrative = 'floodplain', mlra = '18')

OSDquery(everything = 'floodplain', mlra = '18')





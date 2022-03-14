library(aqp)
library(soilDB)


# perhaps a very long list of soil series names
s <- c('fresno', 'zook', 'marion', 'leon', 'sierra')

# chunk ids, 2 series / chunk
chunks <- makeChunks(s, size = 2)

# iterate over chunks
# result is a list of SPCs
res <- lapply(split(s, chunks), fetchOSD)

# flatten list -> single SPC
x <- combine(res)

# check, but only for smallish collections (<50)
plotSPC(x)



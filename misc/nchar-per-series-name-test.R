library(aqp)
library(lattice)
library(latticeExtra)
library(tactile)

sc <- get_soilseries_from_NASIS()

testN <- function(n = 100) {
  s <- sample(sc$soilseriesname, n)
  
  x <- 'https://casoilresource.lawr.ucdavis.edu/api/soil-series.php?q=site_hz&s='
  final.url <- paste(x, URLencode(paste(s, collapse = ',')), sep = '')
  nchar(final.url)  
}

nseries <- c(10, 20, 50, 100, 150, 200, 250, 300, 400) 
sim <- lapply(nseries, function(i) {
  d <- data.frame(
    nseries = i,
    nchar = replicate(200, testN(n = i))
  )
  return(d)
})

sim <- do.call('rbind', sim)

bwplot(nchar ~ factor(nseries), data = sim, par.settings = tactile.theme(), scales = list(y = list(log = 10)), yscale.components = yscale.components.log10.3, xlab = 'Number of Series Names', ylab = 'URL Length (characters)', panel = function(...) {
  panel.grid(-1, -1)
  panel.abline(h = log(2048, base = 10), lty = 2)
  panel.bwplot(...)
})


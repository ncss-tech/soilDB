library(soilDB)
library(httr)
library(jsonlite)

# API URL
u <- 'https://casoilresource.lawr.ucdavis.edu/osd-search/search-entire-osd.php'

parameters <- list(
  json = 1,
  mlra = '',
  query = 'peraquic'
)

# POST it
res <- POST(u, body=parameters, encode='form')

# trap errors, likely related to SQL syntax errors
request.status <- try(stop_for_status(res), silent = TRUE)

# the result is JSON
# should simplify to data.frame nicely
r.content <- content(res, as = 'text', encoding = 'UTF-8')
d <- fromJSON(r.content)

str(d)


## compare with OSDquery()
# slightly cleaning output
x <- OSDquery(typical_pedon = 'rhyo:* & tuff:*')
str(x)

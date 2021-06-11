library(soilDB)

# WORKS (dominant condition)
get_SDA_property(c("taxsuborder", "taxorder", "tfact"),
                 method = "dominant condition",
                 areasymbols = "CA630")
# WORKS (min/max)
get_SDA_property(c("ksat_l", "ksat_r", "ksat_h"),
                 method = "min",
                 areasymbols = "CA630")

# WORKS (weighted average)
get_SDA_property(c("ksat_l", "ksat_r", "ksat_h"),
                 method = "weighted average",
                 areasymbols = 'CA630')

# WORKS (dominant component, numeric -- special case of weighted average)
q <- get_SDA_property(
  c("ksat_l", "ksat_r", "ksat_h"),
  method = "dominant component (numeric)",
  areasymbols = 'CA630',
  query_string = TRUE # this just returns the query instead of calling SDA_query
)
# cat(q)
res <- SDA_query(q)
res

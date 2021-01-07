

## overly-simplistic aggregation of tabular data from SDA which must be generalized / abstracted
## this will eventually be available as a macro / stored procedure in SDA

linkMuaggattTabular <- function(r, vars) {
  
  
}

linkComponentTabular <- function(r, vars) {
  
  
}



## TODO:
# option for writing to files
# vectorize over depths
# abstract aggregation to SDA
# slab() optimizations | trunc() | segment | manual operation via data.table
# aggregation method specification
# component data
# muaggatt data
# flattened geomorph | parent material



# method: 1 (component weighted mean)
# method: 2 (largest component)

linkComponentHorizonTabular <- function(r, vars, interval, method = 1) {
  
  
  # get unique mukey
  ll <- levels(r)[[1]]
  IS <- format_SQL_in_statement(ll$ID)
  
  # query SDA by mukey
  # this will bring down most of the interesting site / horizon level attributes from SSURGO/STATSGO
  
  
  ws <-  switch(method,
                '1' = {
                  # component wt. mean, get all components
                  sprintf("mukey IN %s", IS)
                },
                '2' = {
                  # largest component, ignore minor components
                  sprintf("majcompflag = 'Yes' AND mukey IN %s", IS)
                }
  )
  
  x <- suppressMessages(
    fetchSDA(WHERE = ws, duplicates = TRUE, droplevels = TRUE, stringsAsFactors = FALSE, childs = FALSE)
  )
  
  
  ## TODO: account for NAs: source data through aggregation steps
  
  # component level aggregation for variables and depth intervals of interest
  # note that we get an "extra" depth interval of 5-30
  
  f <- as.formula(sprintf("cokey ~ %s", paste(vars, collapse = ' + ')))
  
  
  ## TODO: generalize this for vectorization over depth intervals
  
  x.a <- slab(x, fm = f, slab.structure = interval, slab.fun = mean, na.rm = TRUE)
  
  # long -> wide format
  w <- dcast(x.a, cokey ~ variable, value.var = 'value')
  
  
  # MU-level aggregation / subset
  s <- site(x)[, c('mukey', 'cokey', 'comppct_r')]
  s <- merge(s, w, by = 'cokey', sort = FALSE, all.x = TRUE)
  
  # STATSGO map unit for testing
  # l <- split(s, s$mukey)
  # i <- i <- l[['660849']]
  
  
  
  # component percentage weighted mean
  ss <- split(s, s$mukey)
  
  ## TODO: are there any reasons to use the original mukey set as the left-most join
  # full.rat <- data.frame(mukey = ll, stringsAsFactors = FALSE)
  # names(full.rat) <- 'mukey'
  # rat.list <- c(full.rat, rat.list)
  
  # iterate over variables of interest and perform component weighted mean
  rat.list <- lapply(
    vars,
    function(i) {
      res <- lapply(ss, wt.mean.component, var = i)
      res <- do.call('rbind', res)
      return(res)
    })
  
  
  # wrap base::merge with some preset arguments
  .Safemerge <- function(...) {
    merge(..., by = 'mukey', sort = FALSE, all.x = TRUE)
  }

  # convert list -> data.frame after performing sequential left-joins
  rat <- Reduce(f = .Safemerge, x = rat.list, right = FALSE)
  
  ## TODO: keep track
  # preservation of mukey?
  if(nrow(ll) != nrow(rat)) {
    missing.mukey <- setdiff(ll$ID, rat$mukey)
    message(sprintf("missing mukey in aggregation of SDA sourced data: %s", paste(missing.mukey, collapse = ',')))
  }
  
  
  # make RAT compatible with raster object
  # first column should be named 'ID'
  names(rat)[1] <- 'ID'
  
  # re-pack RAT
  levels(r) <- rat
  
  ## TODO: generalize for mixtures of categorical / continuous
  # deratify all attributes
  rs <- deratify(r)
  
  ## TODO: is this a pointer or "all of the data"?
  return(rs)
  
}


## TODO: generalize
wt.mean.component <- function(i, var, wt = 'comppct_r') {
  
  # remove NA in target variable
  idx <- which(is.na(i[[var]]) | is.na(i[[wt]]))
  if(length(idx) > 0) {
    i <- i[-idx, ] 
  }
  
  # weighted mean
  wm <- sum(i[[var]] * i[[wt]]) / sum(i[[wt]])
  
  # pack results
  res <- data.frame(
    mukey = i$mukey[1],
    var = wm,
    stringsAsFactors = FALSE
  )
  
  # re-name for convenience later
  names(res)[2] <- var
  
  return(res)
}

# largest component
dominant.component <- function(i, var, wt = 'comppct_r') {
  
}


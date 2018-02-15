
## TODO: this function is a large time sink when number of pedons > ~ 500.
## https://github.com/ncss-tech/soilDB/issues/55

# This function heavily biased towared NASIS-specific data structures and assumptions
# d: data.frame with color data from horizon-color table: expects "colorhue", "colorvalue", "colorchroma"
# id.var: name of the column with unique horizon IDs
# ...: further arguments passed to mix_and_clean_colors()
simplifyColorData <- function(d, id.var='phiid', ...) {
  
  # sanity check: must contain 1 row
  if(nrow(d) < 1) {
    warning('0 rows of colors data, doing nothing', call. = FALSE)
    return(d)
  }
  
  # convert Munsell to RGB
  d.rgb <- with(d, munsell2rgb(colorhue, colorvalue, colorchroma, return_triplets=TRUE))
  
  # re-combine
  d <- cbind(d, d.rgb)
  
  # add a fake column for storing `sigma`
  # this is the error associated with the rgb -> munsell transformation
  d$sigma <- NA
  
  # perform lower-case comparison, values differ based on interpretation of NASIS metadata
  d$colormoistst <- tolower(d$colormoistst)
  
  # split into dry / moist
  dry.colors <- d[which(d$colormoistst == 'dry'), ]
  moist.colors <- d[which(d$colormoistst == 'moist'), ]
  
  ## there may be cases where there are 0 records of dry or moist colors
  
  # split-out those data that need color mixing:
  dry.to.mix <- names(which(table(dry.colors[[id.var]]) > 1))
  moist.to.mix <- names(which(table(moist.colors[[id.var]]) > 1))
  
  # names of those columns to retain
  vars.to.keep <- c(id.var, "r", "g", "b", "colorhue", "colorvalue", "colorchroma", 'sigma')
  
  # mix/combine if there are any horizons that need mixing
  if(length(dry.to.mix) > 0) {
    message(paste('mixing dry colors ... [', length(dry.to.mix), ' of ', nrow(dry.colors), ' horizons]', sep=''))
    
    # filter out and mix only colors with >1 color / horizon
    dry.mix.idx <- which(dry.colors[[id.var]] %in% dry.to.mix)
    mixed.dry <- ddply(dry.colors[dry.mix.idx, ], id.var, mix_and_clean_colors, ...)
    
    # combine original[-horizons to be mixed] + mixed horizons
    dry.colors.final <- rbind(dry.colors[-dry.mix.idx, vars.to.keep], mixed.dry)
    names(dry.colors.final) <- c(id.var, 'd_r', 'd_g', 'd_b', 'd_hue', 'd_value', 'd_chroma', 'd_sigma')
  }
  else {# otherwise subset the columns only
    dry.colors.final <- dry.colors[, vars.to.keep]
    names(dry.colors.final) <- c(id.var, 'd_r', 'd_g', 'd_b', 'd_hue', 'd_value', 'd_chroma', 'd_sigma')
  }
  
  # mix/combine if there are any horizons that need mixing
  if(length(moist.to.mix) > 0) {
    message(paste('mixing moist colors ... [', length(moist.to.mix), ' of ', nrow(moist.colors), ' horizons]', sep=''))
    
    # filter out and mix only colors with >1 color / horizon
    moist.mix.idx <- which(moist.colors[[id.var]] %in% moist.to.mix)
    mixed.moist <- ddply(moist.colors[moist.mix.idx, ], id.var, mix_and_clean_colors, ...)
    
    # combine original[-horizons to be mixed] + mixed horizons
    moist.colors.final <- rbind(moist.colors[-moist.mix.idx, vars.to.keep], mixed.moist)
    names(moist.colors.final) <- c(id.var, 'm_r', 'm_g', 'm_b', 'm_hue', 'm_value', 'm_chroma', 'm_sigma')
  }
  else {# otherwise subset the columns only
    moist.colors.final <- moist.colors[, vars.to.keep]
    names(moist.colors.final) <- c(id.var, 'm_r', 'm_g', 'm_b', 'm_hue', 'm_value', 'm_chroma', 'm_sigma')
  }
  
  # merge into single df
  d.final <- join(dry.colors.final, moist.colors.final, by=id.var, type='full')
  
  # make HEX colors
  d.final$moist_soil_color <- NA
  idx <- complete.cases(d.final$m_r)
  d.final$moist_soil_color[idx] <- with(d.final[idx, ], rgb(m_r, m_g, m_b))
  
  d.final$dry_soil_color <- NA
  idx <- complete.cases(d.final$d_r)
  d.final$dry_soil_color[idx] <- with(d.final[idx, ], rgb(d_r, d_g, d_b))
  
  return(d.final)
}


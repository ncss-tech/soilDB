mix_and_clean_colors <-
function(x)
  {
  # fill missing weights with 1
  x$pct[is.na(x$pct)] <- 1
  
  # skip horizons with a single color
  tab <- table(x$hz_id)
  if(tab > 1) {
    r <- with(x, wtd.mean(r, weights=pct))
    g <- with(x, wtd.mean(g, weights=pct))
    b <- with(x, wtd.mean(b, weights=pct))
    # composite
    df <- data.frame(r,g,b)
    }
  else
    df <- x[, c('r','g','b')]
  
  # done
  return(df)
  }


#' @title Simplify Color Data by ID
#' 
#' @description Simplify multiple Munsell color observations associated with each horizon.
#' 
#' This function is mainly intended for the processing of NASIS pedon/horizon
#' data which may or may not contain multiple colors per horizon/moisture
#' status combination. \code{simplifyColorData} will "mix" multiple colors
#' associated with horizons in \code{d}, according to IDs specified by
#' \code{id.var}, using "weights" (area percentages) specified by the \code{wt}
#' argument.
#' 
#' Note that this function doesn't actually simulate the mixture of pigments on
#' a surface, rather, "mixing" is approximated via weighted average in the
#' CIELAB colorspace.
#' 
#' The \code{simplifyColorData} function can be applied to data sources other
#' than NASIS by careful use of the \code{id.var} and \code{wt} arguments.
#' However, \code{d} must contain Munsell colors split into columns named
#' "colorhue", "colorvalue", and "colorchroma". In addition, the moisture state
#' ("Dry" or "Moist") must be specified in a column named "colormoistst".
#' 
#' Examples: 
#'  - [KSSL data](http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html)
#'  - [soil color mixing tutorial](http://ncss-tech.github.io/AQP/soilDB/mixing-soil-color-data.html)
#' 
#' @param d a \code{data.frame} object, typically returned from NASIS, see
#' details
#' @param id.var character vector with the name of the column containing an ID
#' that is unique among all horizons in \code{d}
#' @param wt a character vector with the name of the column containing color
#' weights for mixing
#' @param bt logical, should the mixed sRGB representation of soil color be
#' transformed to closest Munsell chips? This is performed by [aqp::col2Munsell()]
#' @author D.E. Beaudette
#' @keywords manip
#' @export 
#' @importFrom grDevices rgb
simplifyColorData <- function(d, id.var = 'phiid', wt = 'colorpct', bt = FALSE) {
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  # sanity check: must contain at least 1 row
  if (nrow(d) < 1) {
    warning('simplifyColorData: 0 rows of colors data, doing nothing', call. = FALSE)
    return(d)
  }
  
  # convert Munsell -> CIELAB + sRGB
  d.lab <- with(d, aqp::munsell2rgb(colorhue, colorvalue, colorchroma, returnLAB = TRUE, return_triplets = TRUE))
  d <- cbind(d, d.lab)
  
  # add a fake column for storing `sigma`
  # this is the error associated with the CIELAB -> munsell transformation
  d$sigma <- NA
  
  # perform lower-case comparison, values differ based on interpretation of NASIS metadata
  d$colormoistst <- tolower(d$colormoistst)
  
  # split into dry / moist
  .SD <- NULL
  dry.colors <- data.table::as.data.table(d[grep('dry', d$colormoistst, ignore.case = TRUE), ])
  moist.colors <- data.table::as.data.table(d[grep('moist', d$colormoistst, ignore.case = TRUE), ])
  
  ## there may be cases where there are 0 records of dry or moist colors
  
  # split-out those data that need color mixing:
  dry.to.mix <- names(which(table(dry.colors[[id.var]]) > 1))
  moist.to.mix <- names(which(table(moist.colors[[id.var]]) > 1))
  
  # variables to retain before / after mixing
  # note: CIELAB only used internally
  vars.to.keep <- c(id.var, "r", "g", "b", "colorhue", "colorvalue", "colorchroma", 'sigma')
  
  # variables required for mixing
  mix.vars <- c(wt, 'L', 'A', 'B')
  
  # mix/combine if there are any horizons that need mixing
  if (length(dry.to.mix) > 0) {
    message(paste0('mixing dry colors ... [', length(dry.to.mix), ' of ', nrow(dry.colors), ' horizons]'))
    
    # filter out and mix only colors with >1 color / horizon
    dry.mix.idx <- which(dry.colors[[id.var]] %in% dry.to.mix)
    
    # split by horizon ID
    # note: split will re-order IDs
    # dc <- split(dry.colors[dry.mix.idx, mix.vars], f = dry.colors[[id.var]][dry.mix.idx])
    #     # final vesion
    # mixed.dry <- lapply(dc, estimateColorMixture, wt = wt, backTransform = bt)
    # 
    # # flatten and copy id.var from rownames
    # mixed.dry <- do.call('rbind', mixed.dry)
    # mixed.dry[[id.var]] <- row.names(mixed.dry)
    
    mixed.dry <- dry.colors[dry.mix.idx, .SD, .SDcols =  c(id.var, mix.vars)]
    mixed.dry <- mixed.dry[, estimateColorMixture(.SD, wt = wt, backTransform = bt), by = id.var]
    
    # convert sRGB -> Munsell
    # requires >= aqp 2.0.2
    m <- aqp::col2Munsell(as.data.frame(mixed.dry[, .SD, .SDcols = c('r', 'g', 'b')]))
    
    # adjust names to match NASIS
    names(m) <- c("colorhue", "colorvalue", "colorchroma", "sigma")
    
    # combine with mixed sRGB coordinates
    mixed.dry <- cbind(mixed.dry[, .SD, .SDcols = c(id.var, 'r', 'g', 'b')], m)
    
    # combine original[-horizons to be mixed] + mixed horizons
    dry.colors.final <- rbind(dry.colors[-dry.mix.idx, .SD, .SDcols = vars.to.keep], mixed.dry)
    names(dry.colors.final) <- c(id.var, 'd_r', 'd_g', 'd_b', 'd_hue', 'd_value', 'd_chroma', 'd_sigma')
    
  } else {# otherwise subset the columns only
    dry.colors.final <- dry.colors[, .SD, .SDcols =  vars.to.keep]
    names(dry.colors.final) <- c(id.var, 'd_r', 'd_g', 'd_b', 'd_hue', 'd_value', 'd_chroma', 'd_sigma')
  }
  
  # mix/combine if there are any horizons that need mixing
  if (length(moist.to.mix) > 0) {
    message(paste0('mixing moist colors ... [', length(moist.to.mix), ' of ', nrow(moist.colors), ' horizons]'))
    
    # filter out and mix only colors with >1 color / horizon
    moist.mix.idx <- which(moist.colors[[id.var]] %in% moist.to.mix)
    
    # # split by horizon ID
    # # note: split will re-order IDs
    # mc <- split(moist.colors[moist.mix.idx, mix.vars], f = moist.colors[[id.var]][moist.mix.idx])
    # 
    # # final version
    # mixed.moist <- lapply(mc, estimateColorMixture, wt = wt, backTransform = bt)
    # 
    # # flatten and copy id.var from rownames
    # mixed.moist <- do.call('rbind', mixed.moist)
    # mixed.moist[[id.var]] <- row.names(mixed.moist)
    mixed.moist <- moist.colors[moist.mix.idx, .SD, .SDcols =  c(id.var, mix.vars)]
    mixed.moist <- mixed.moist[, estimateColorMixture(.SD, wt = wt, backTransform = bt), by = id.var]
    
    # convert sRGB -> Munsell
    # requires >= aqp 2.0.2
    m <- aqp::col2Munsell(as.data.frame(mixed.moist[, .SD, .SDcols = c('r', 'g', 'b')]))
    
    # adjust names to match NASIS
    names(m) <- c("colorhue", "colorvalue", "colorchroma", "sigma")
    
    # combine with mixed sRGB coordinates
    mixed.moist <- cbind(mixed.moist[, .SD, .SDcols = c(id.var, 'r', 'g', 'b')], m)
    
    # combine original[-horizons to be mixed] + mixed horizons
    moist.colors.final <- rbind(moist.colors[-moist.mix.idx, .SD, .SDcols = vars.to.keep],
                                mixed.moist)
    names(moist.colors.final) <- c(id.var, 'm_r', 'm_g', 'm_b', 'm_hue', 'm_value', 'm_chroma', 'm_sigma')
    
  } else {# otherwise subset the columns only
    moist.colors.final <- moist.colors[, .SD, .SDcols =  vars.to.keep]
    names(moist.colors.final) <- c(id.var, 'm_r', 'm_g', 'm_b', 'm_hue', 'm_value', 'm_chroma', 'm_sigma')
  }
  
  # FULL JOIN dry + moist colors
  d.final <- as.data.frame(merge(dry.colors.final, moist.colors.final, by = id.var, 
                   all.x = TRUE, all.y = TRUE, sort = FALSE, incomparables = NA))
  
  # make HEX colors
  # safely account for NA, rgb() will not accept NA input
  if (nrow(d.final) > 0) {
    d.final$moist_soil_color <- NA
    idx <- complete.cases(d.final$m_r)
    d.final$moist_soil_color[idx] <- with(d.final[idx, ], rgb(m_r, m_g, m_b))
    
    d.final$dry_soil_color <- NA
    idx <- complete.cases(d.final$d_r)
    d.final$dry_soil_color[idx] <- with(d.final[idx, ], rgb(d_r, d_g, d_b))
  } else {
    # this happens if all moisture states are NA
    d.final$moist_soil_color <- character(0)
    d.final$dry_soil_color <- character(0)
  }
  return(d.final)
}






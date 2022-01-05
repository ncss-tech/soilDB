library(soilDB)
x <- SDA_query("SELECT DISTINCT attributekey FROM sdvattribute")
res <- get_SDV_legend_elements(paste0("attributekey = ", x$attributekey))
resall <- data.table::rbindlist(res, fill = TRUE)
sdvmaplegend <- resall

# including attributedescription makes rda 10x bigger
sdvmaplegend$attributedescription <- NULL
save(sdvattribute, file="misc/sdvmaplegend.rda")

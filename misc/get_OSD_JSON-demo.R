# There are many analyses that benefit from having direct access to the contents of OSDs parsed by section. 
# 
# The following code snippet builds a ~30MB .rda file containing OSD data -- a _data.frame_ with one column per "standard OSD section" and one row per series (n=24379). This is too big to deliver as part of the package but it offers significant capabilities over the SoilWeb Postgres fulltext search (soilDB::OSD_query) when one is interested in the details of the OSD narrative rather than just a vector of series names matching some search.
# 
# The parsed OSD table from SoilKnowledgeBase is derived from the set of series specific JSON files in https://github.com/ncss-tech/SoilKnowledgeBase. This dataset gives complementary info to that from the SC database and those derived from series/component names/SSURGO: https://github.com/ncss-tech/SoilTaxonomy/blob/master/inst/extdata/SC-database.csv.gz and https://github.com/ncss-tech/SoilTaxonomy/blob/master/inst/extdata/series-stats.csv.gz. 
# 
# I want to consider some avenues for making the FULL OSD data readily available and _queryable_. This is currently possible if one has an instance of SoilKnowledgeBase / JSON files installed locally. 
# 
# An interesting new option is https://phiresky.github.io/blog/2021/hosting-sqlite-databases-on-github-pages/ which allows a simple SQLite database to be hosted (read only) via static GitHub Pages. 
# 
# library(soilDB)
# 
# path <- "../SoilKnowledgeBase/inst/extdata/OSD"
# 
# # list all
# series <- gsub("(.*).json|(.*log$)", "\\1", basename(list.files(path, recursive = TRUE)))
# series <- series[nchar(series) > 0]
# 
# # this uses a _local_ cloned instance of SKB repo (pretty fast) stored in same parent directory as soilDB
# #  - base_url = NULL or missing will use GitHub
# res <- soilDB::get_OSD_JSON(series, base_url = path)
# 
# ST_series <- res
# save(ST_series, file = "misc/ST_series.rda")
# 
# colnames(ST_series)
# #>  [1] "SERIES"                          "STATUS"                         
# #>  [3] "BYREV"                           "REVDATE"                        
# #>  [5] "STATES"                          "OVERVIEW"                       
# #>  [7] "TAXONOMIC.CLASS"                 "TYPICAL.PEDON"                  
# #>  [9] "TYPE.LOCATION"                   "RANGE.IN.CHARACTERISTICS"       
# #> [11] "COMPETING.SERIES"                "GEOGRAPHIC.SETTING"             
# #> [13] "GEOGRAPHICALLY.ASSOCIATED.SOILS" "DRAINAGE.AND.PERMEABILITY"      
# #> [15] "USE.AND.VEGETATION"              "DISTRIBUTION.AND.EXTENT"        
# #> [17] "REGIONAL.OFFICE"                 "ORIGIN"                         
# #> [19] "REMARKS" 
# 
# nrow(ST_series)
# #> [1] 24379


library(soilDB)
library(tibble)
library(SoilTaxonomy)

path <- "../SoilKnowledgeBase/inst/extdata/OSD"

# list all
series <- gsub("(.*).json|(.*log$)", "\\1", basename(list.files(path,
                                                                recursive = TRUE)))
series <- series[nchar(series) > 0]

# this uses a _local_ cloned instance of SKB repo (pretty fast)
#  - base_url = NULL or missing will use GitHub
res <- get_OSD_JSON(series, base_url = path)

ST_series <- res
save(ST_series, file = "misc/ST_series.rda")

# 2023 series are "incomplete" in one or more sections
tibble(series = series[match(res$SERIES, toupper(series))],
       complete = complete.cases(res))  %>%
  subset(!complete)


# tabulate "missing" sections
apply(res, 2, function(x) sum(is.na(x)))


(tibble(res) %>%
    subset(is.na(TAXONOMIC.CLASS)))$SERIES

(tibble(res) %>%
    subset(is.na(TYPICAL.PEDON)))$SERIES


# most common states
allstates <- table(unlist(strsplit(res$STATES, ",")))

# count by state
allstates[order(allstates, decreasing = TRUE)]

# proportion by state
round(prop.table(allstates[order(allstates, decreasing = TRUE)]), 2)

# how many albolls?
idx <- grep("albolls$", res$TAXONOMIC.CLASS)
res_albolls <- res[idx,]
zzx <- lapply(res_albolls$GEOGRAPHIC.SETTING, function(x) {cat("\n\n"); cat(x)})

# examine the geographic setting of albolls
sum(sapply(res_albolls$GEOGRAPHIC.SETTING, function(x) length(grep("lake|lacustrine", x)) > 0))
sum(sapply(res_albolls$GEOGRAPHIC.SETTING, function(x) length(grep("plain", x)) > 0))
sum(sapply(res_albolls$GEOGRAPHIC.SETTING, function(x) length(grep("loess", x)) > 0))
sum(sapply(res_albolls$GEOGRAPHIC.SETTING, function(x) length(grep("till", x)) > 0))
sum(sapply(res_albolls$GEOGRAPHIC.SETTING, function(x) length(grep("alluvium|stream|terrace", x)) > 0))

# mollic epipedon thickness in range in characteristics

x <- data.table::rbindlist(lapply(seq_along(res$SERIES), function(i)
    data.frame(
      series = res$SERIES[i],
      taxonomy = res$TAXONOMIC.CLASS[i],
      ric.content = as.character(strsplit(res$RANGE.IN.CHARACTERISTICS[i], "\\. |\\n")[[1]])
    )))

x.mollic <- subset(x, grepl('[^n][^o][^t] thick', ric.content, ignore.case = TRUE) &
                      grepl('mollic epipedon', ric.content, ignore.case = TRUE))
x.mollic$units <- NA_character_
x.mollic$numbers <- gsub(".*[ \\-](\\d+ to \\d+) (in|inches|cm|centimeters).*", "\\1;\\2", x.mollic$ric.content)

x.mollic.split <- strsplit(x.mollic$numbers, " to |;")
names(x.mollic.split) <- x.mollic$series

taxonfamily <- gsub("TAXONOMIC CLASS: ?", "", x.mollic$taxonomy)
names(taxonfamily) <- x.mollic$series

x.mollic.result <- data.table::rbindlist(lapply(seq_along(x.mollic.split), function(i) {
    if (length(x.mollic.split[[i]]) == 3) {
      aSeries <- names(x.mollic.split)[[i]]
      data.frame(
        series = aSeries,
        taxonomy = taxonfamily[[aSeries]],
        top = x.mollic.split[[i]][1],
        bottom = x.mollic.split[[i]][2],
        units = x.mollic.split[[i]][3]
      )
    }
  }))

families <- SoilTaxonomy::parse_family(x.mollic.result$taxonomy)
families$series <- x.mollic.result$series

x.mollic.result <- merge(x.mollic.result, families, all.x=TRUE, sort=FALSE, incomparables=NA)

inidx <- x.mollic.result[, .I[units %in% c("in","inches")]]

x.mollic.result$classes_split <- NULL
x.mollic.result$taxonomy <- NULL

x.mollic.result$topt <- as.numeric(x.mollic.result$top)
x.mollic.result$topt[inidx] <- x.mollic.result$topt[inidx]*2.54

x.mollic.result$bottomt <- as.numeric(x.mollic.result$bottom)
x.mollic.result$bottomt[inidx] <- x.mollic.result$bottomt[inidx]*2.54

plot(density(round(x.mollic.result$topt), na.rm=T))
plot(density(round(x.mollic.result$bottomt), na.rm=T))

sort(table(round(x.mollic.result$topt)))
sort(table(round(x.mollic.result$bottomt)))
  
parenttaxa <- data.table::rbindlist(lapply(SoilTaxonomy::getParentTaxa(code = x.mollic.result$subgroup_code),
                                           function(x) {
                                             if (length(x) == 3) {
                                               data.frame(order = x[1],
                                                          suborder = x[2],
                                                          greatgroup = x[3])
                                             } else {
                                               data.frame(order = NA,
                                                          suborder = NA,
                                                          greatgroup = NA)
                                             }
                                           }),
                                    fill = TRUE)

x.mollic.result <- cbind(parenttaxa, x.mollic.result)

min_lt18 <- subset(x.mollic.result, topt < 17.5)
write.csv(min_lt18, file = "misc/osd_mollic_min_thickness_lt18.csv")

length(unique(min_lt18$greatgroup)) 
# [1] 21

sort(table(min_lt18$greatgroup), decreasing = TRUE)[1:5]
# top 5 affected: 
# Haplustolls  Argiustolls  Argixerolls Haploxerolls Calcixerolls 
# 14            7            4            4            2 

max_lt25 <- subset(x.mollic.result, bottomt < 24.5)
write.csv(max_lt25, file = "misc/osd_mollic_max_thickness_lt25.csv")
# great groups affected
length(unique(max_lt25$greatgroup))
# [1] 13

sort(table(max_lt25$greatgroup), decreasing = TRUE)[1:5]
# top 5 affected: 
# Haplustolls Calciustolls  Argixerolls  Calciargids Calcixerolls 
# 4            2            1            1            1 

min_is18 <- subset(x.mollic.result, round(topt) == 18)
write.csv(min_is18, file = "misc/osd_mollic_min_thickness_18.csv")
sort(table(min_is18$greatgroup), decreasing = TRUE)[1:5]
# top 5 affected: 
# Argiustolls Haploxerolls  Haplustolls Calciaquolls   Hapludolls 
# 73           69           64           43           43 

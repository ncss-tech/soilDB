library(soilDB)
library(tibble)

path <- "E:/workspace/SoilKnowledgeBase/inst/extdata/OSD"

# list all
series <- gsub("(.*).json|(.*log$)", "\\1", basename(list.files(path,
                                                                recursive = TRUE)))
series <- series[nchar(series) > 0]

# this uses a _local_ cloned instance of SKB repo (pretty fast)
#  - base_url = NULL or missing will use GitHub
res <- fetchOSD_JSON(series, base_url = path)

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

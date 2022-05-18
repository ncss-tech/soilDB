# build NASIS table column data set
library(soilDB)

# or some other vector of table names
# table_names <- get_NASIS_table_name_by_purpose()
table_names <- DBI::dbListTables({con <- soilDB::NASIS()})
table_names <- table_names[grep("_SS|_State|_0|NONDEL|View", table_names, invert = TRUE)]
table_names <- table_names[1:346]
DBI::dbDisconnect(con)

# get all column names
table_colnames <- lapply(table_names, function(x) colnames(.dump_NASIS_table(x)))

# combine
table_colnames_cmb <- sapply(table_colnames, paste0, collapse = ",")

# identify foreign keys (assuming it comes first in column name order)
table_fkeys <- sapply(table_colnames, function(x) {
    ldx1 <- grepl("iid", x)
    # ldx2 <- !grepl("dbiid", x)
    ldx2 <- ldx1
    x[which(ldx1 & ldx2)[1]]
  })

table_pkeys <- sapply(table_colnames, function(x) {
  idx <- which(grepl("iid$", x) & !grepl("dbiid|tbl_", x))
  if (length(idx) == 0) return(NA)
  x[idx[length(idx)]]
})

parent_pkeys <- gsub("ref", "", table_fkeys)

cardinal_tables <- c(
  "site",
  "siteassoc",
  "siteassocsite",
  "pedon",
  "transect",
  "vegplot",
  "vegtransect",
  "datamapunit",
  "mapunit",
  "plant",
  "ecologicalsite",
  "othvegclass",
  "geomorfeattype",
  "project",
  "techsoilservice",
  "calculation", 
  "customchoicelistset", 
  "domaingroup", 
  "editsetup", 
  "form", 
  "nasisgroupmember",
  "othvegclasstype", 
  "query", 
  "report",
  "wsimportmap"
)
cardinal_fkeys <- c(
  "siteiid",
  "siteassociid",
  "siteiid",
  "peiid",
  "tsectiid",
  "vegplotiid",
  "vegtransectiid",
  "dmuiid",
  "muiid",
  "plantiid",
  "ecositeiid",
  "ovegcliid",
  "geomftiid",
  "projectiid",
  "tssiid",
  "calc_iid", 
  "customchoicelistsetiid", 
  "domgrpiid", 
  "edtsuiid", 
  "formiid", 
  "nasisgroupmember",
  "ovegcltypiid", 
  "qryiid", 
  "rptiid",
  "wsimportmapiid"
)
names(cardinal_fkeys) <- cardinal_tables
# subset(NASIS_table_column_keys, fkey == "grpiidref") |> View()
replace.idx <-  match(cardinal_tables, table_names)

# these are "cardinal" iids, they come last in their respective tables, not first
table_fkeys[replace.idx] <- cardinal_fkeys
parent_pkeys[replace.idx] <- cardinal_fkeys

# cross check
NASIS_table_column_keys <- data.frame(table = table_names,
                                      column = table_colnames_cmb,
                                      fkey = table_fkeys,
                                      pkeyref = parent_pkeys,
                                      pkey = table_pkeys)

# f <- read.table("misc/nasis_pedon_object/pedon_table_columns.txt",
#                 sep = ",", header = TRUE)

# old <- as.list(trimws(f$fkey))
# names(old) <- trimws(f$table)
# nu <- as.list(NASIS_table_column_keys$fkey)
# names(nu) <- NASIS_table_column_keys$table

# checks against jay's pedonpc table
# cmpr_oldnu <-  sapply(NASIS_table_column_keys$table,  function(x) {
#       res <- (old[[x]] == nu[[x]])
#       if(length(res) == 0) { 
#         return(NA) 
#       } 
#       return(res)
#     })
# which(!cmpr_oldnu)

usethis::use_data(NASIS_table_column_keys, overwrite = TRUE, compress = 'xz')



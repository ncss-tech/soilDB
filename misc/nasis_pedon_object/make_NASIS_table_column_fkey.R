# build NASIS table column dataset

# or some other vector of table names
table_names <- soilDB:::.get_NASIS_table_name_by_purpose()

# get all colum names
table_colnames <- lapply(table_names, function(x) colnames(soilDB:::.dump_NASIS_table(x)))

# combine
table_colnames_cmb <- sapply(table_colnames, paste0, collapse = ",")

# identify foreign keys (assuming it comes first in column name order)
table_fkeys <- gsub("ref", "", sapply(table_colnames, function(x) x[grep("[^d][^b]iid", x)[1]]))

cardinal_tables <- c(
  "site",
  "siteassoc",
  "siteassocsite",
  "pedon",
  "transect",
  "vegplot",
  "vegtransect",
  "datamapunit",
  "component",
  "plant",
  "ecologicalsite",
  "othvegclass",
  "geomorfeattype"
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
  "coiid",
  "plantiid",
  "ecositeiid",
  "ovegcliid",
  "geomftiid"
)
names(cardinal_fkeys) <- cardinal_tables

replace.idx <-  match(cardinal_tables, table_names)

# these are "cardinal" iids, they come last in their respective tables, not first
table_fkeys[replace.idx] <- cardinal_fkeys

# cross check
NASIS_table_column_fkey <- data.frame(table = table_names,
                                      column = table_colnames_cmb,
                                      fkey = table_fkeys)

f <- read.table("misc/nasis_pedon_object/pedon_table_columns.txt",
                sep = ",", header = TRUE)

old <- as.list(trimws(f$fkey))
names(old) <- trimws(f$table)
nu <- as.list(NASIS_table_column_fkey$fkey)
names(nu) <- NASIS_table_column_fkey$table

cmpr_oldnu <-  sapply(NASIS_table_column_fkey$table,  function(x) {
      res <- (old[[x]] == nu[[x]])
      if(length(res) == 0) { 
        return(NA) 
      } 
      return(res)
    })
which(!cmpr_oldnu)

save(NASIS_table_column_fkey, file = "data/NASIS_table_column_fkey.rda")

# View(NASIS_table_column_fkey)

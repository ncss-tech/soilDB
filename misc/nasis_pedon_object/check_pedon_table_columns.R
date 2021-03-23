# first install latest soilDB from nasisDBI branch (unless that is merged when one reads this)
# remotes::install_github("ncss-tech/soilDB@nasisDBI", dependencies = FALSE)
library(DBI)
library(soilDB)

# read file with table,column,fkey as headers
#                where `column` value is quoted comma delimited string within a CSV
f <- read.table("misc/nasis_pedon_object/pedon_table_columns.txt",
                sep = ",", header = TRUE)

# create colname lookup list, named by table
cols <- strsplit(f$column, ",")
names(cols) <- f$table

# create foreign key lookup list, named by table
fkeys <- f$fkey
names(fkeys) <- f$table

# inspect
head(cols)

head(fkeys)

# get list of tables from NASIS connection
nasis <- soilDB::NASIS()
nasis_tables_all <- DBI::dbListTables(nasis)

# all table names exist in current NASIS?
all(f$table %in% nasis_tables_all)

# bad tables names?
badtidx <- which(!f$table %in% nasis_tables_all)
f$table[badtidx]

# what "veg" tables are in NASIS?
# nasis_tables_all[grep("veg", nasis_tables_all)]

# this uses ncss-tech/soilDB@nasisDBI to create a list of tables queried by name 
test <- soilDB::createStaticNASIS(tables = f$table) #, output_file = "test.sqlite")

# check the lookup lists against NASIS (find stuff not in NASIS)
test.res <- sapply(f$table, function(aTable) {
  any(!cols[[aTable]] %in% colnames(test[[aTable]]))
})
test.res[sapply(test.res, isTRUE)]

# now check NASIS against lookup list (find new stuff in NASIS)
test.res2 <- sapply(f$table, function(aTable) {
  # recwlupdated and recuseriidref are calculated internally by NASIS so they are "freebies"
  testcols <- colnames(test[[aTable]])
  res <- testcols[!testcols %in% c(cols[[aTable]], c("recwlupdated","recuseriidref"))]
})
test.res2[sapply(test.res2, length) > 0]

# check tables
chktbls <- names(test.res[test.res])
chkcols <- lapply(chktbls, function(fixtable) {
  testcols <- cols[[fixtable]]
  srccols <- colnames(test[[fixtable]])
  list(pedonpc = testcols[!testcols %in% srccols], 
       nasis_current = srccols[!srccols %in% testcols])
})
names(chkcols) <- chktbls
chkcols

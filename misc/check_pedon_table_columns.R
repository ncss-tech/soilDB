# first install latest soilDB
#remotes::install_github("ncss-tech/soilDB", dependencies = FALSE)

# 'pedon_table_columns.txt' defines a set of columns from a slightly older NASIS data model
# This script compares those contents with a local NASIS instance.

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

# create a list of tables queried by name 
test <- soilDB::createStaticNASIS(f$table)

# check the lookup lists against NASIS (find stuff not in NASIS)
test.res <- sapply(f$table, function(aTable) {
  any(!cols[[aTable]] %in% colnames(test[[aTable]]))
})
# these have one or more columns missing from current nasis
test.res[sapply(test.res, isTRUE)]

# now check NASIS against lookup list (find new stuff in NASIS)
test.res2 <- sapply(f$table, function(aTable) {
  # recwlupdated and recuseriidref are calculated internally by NASIS so they are "freebies"
  testcols <- colnames(test[[aTable]])
  res <- testcols[!testcols %in% c(cols[[aTable]], c("recwlupdated","recuseriidref"))]
})
# these have columns in current nasis missing from old def file
test.res2[sapply(test.res2, length) > 0]

# create a list of comparisons
chktbls <- names(test.res[test.res])
chkcols <- lapply(chktbls, function(fixtable) {
  testcols <- cols[[fixtable]]
  srccols <- colnames(test[[fixtable]])
  list(pedonpc = testcols[!testcols %in% srccols], 
       nasis_current = srccols[!srccols %in% testcols])
})
names(chkcols) <- chktbls
chkcols

# inspect differences as JSON
# jsonlite::prettify(jsonlite::toJSON(chkcols))

# create a file-based database at this path
test_dsn <- "test.sqlite"

# what additional tables required to run fetchNASIS and other methods?
meta_tables <- c('MetadataDomainDetail', # metadata needed for uncode()
                 'MetadataDomainMaster', # metadata needed for uncode()
                 'MetadataTableColumn', # metadata needed for uncode()
                                        #
                 'ecologicalsite', 'plant', # basic code lookup tables 
                                            # (ES for fetchNASIS, plant for fetchVegdata)
                                            # 
                 'geomorfeat', 'geomorfeattype', # basic code lookup tables
                                                 # geomorphic features and feature types for fetchNASIS
                                                 # 
                 'datamapunit', 'datamapunit_View_1',
                 'component', 'component_View_1',
                 'chorizon' , 'chorizon_View_1',
                 'chtexturegrp', 'chtexturegrp_View_1',
                 'chstructgrp', 'chstructgrp_View_1',
                 'cogeomordesc', 'cogeomordesc_View_1',
                 'copmgrp', 'copmgrp_View_1', 'copm', 'copm_View_1',
                 'coothvegclass', 'coothvegclass_View_1', 'othvegclass',
                 'coecosite', 'coecosite_View_1',
                 'codiagfeatures', 'codiagfeatures_View_1',
                 'corestrictions', 'corestrictions_View_1',
                 'vegplot', 'vegplot_View_1', # for fetchVegdata
                 'plotplantinventory', 'plotplantinventory_View_1', # for fetchVegdata
                 'vegtransect', 'vegtransect_View_1', # for fetchVegdata
                 'vegplottext', 'vegplottext_View_1', # for fetchVegdata
                 'vegtransectplantsummary', 'vegtransectplantsummary_View_1', # for fetchVegdata
                 'plottreesiteindexsummary', 'plottreesiteindexsummary_View_1',# for fetchVegdata
                 'plottreesiteindexdetails', 'plottreesiteindexdetails_View_1') # for fetchVegdata

# write to SQLite file `test_dsn`
test_con <- soilDB::NASIS(test_dsn)
regular_con <- soilDB::NASIS()

any("chorizon_View_1" %in% DBI::dbListTables(regular_con))
any("chorizon_View_1" %in% DBI::dbListTables(test_con))

# reordering but data are same
daff::render_diff(daff::diff_data(DBI::dbReadTable(test_con, "chorizon_View_1"),
                 DBI::dbReadTable(regular_con, "chorizon_View_1")))

# add any additonal tables
res <- soilDB::createStaticNASIS(tables = c(f$table[!f$table %in% DBI::dbListTables(test_con)], 
                                            # paste0(f$table, '_View_1'),
                                            meta_tables[!meta_tables %in% DBI::dbListTables(test_con)]), 
                                 SS = TRUE, 
                                 # SS = TRUE for createStaticNASIS means include (View_1) tables
                                 output_path = test_dsn)

# fetch pedons from the SQLite file `test_dsn`
# f <- fetchNASIS(dsn = test_dsn, SS = FALSE) # SS = FALSE for fetchNASIS means use the "full" tables
# f <- fetchNASIS(dsn = test_dsn, SS = TRUE) # SS = TRUE for fetchNASIS means use the View_1 tables
# 
# # fetch components
f <- fetchNASIS(from = "components", dsn = test_dsn, SS = FALSE) # SS = FALSE for fetchNASIS means use the "full" tables
f <- fetchNASIS(from = "components", dsn = test_dsn, SS = TRUE) # SS = TRUE for fetchNASIS means use the View_1 tables

# fetch vegdata
v <- fetchVegdata(dsn = test_dsn, SS = FALSE)
v <- fetchVegdata(dsn = test_dsn, SS = TRUE)

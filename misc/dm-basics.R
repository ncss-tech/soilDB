# working with a "static" (non-transactional, not authenticated)
# instance of the NASIS database schema/tables with {soilDB}+{dm}
# 
# a demonstration of building the project data model
# 
# last update: 2020/03/26
# andrew g. brown
# 
# 
library(soilDB)
library(dm)
library(daff)
library(dplyr)

# get projects from selected set using createStaticNASIS
res <- createStaticNASIS(get_NASIS_table_name_by_purpose("project", SS = TRUE))

# list project related tables manually
con <- soilDB::NASIS()
x <- DBI::dbListTables(con)
DBI::dbDisconnect(con)

# use full join manually
projects <- left_join(res[["project_View_1"]],
                      by = c("projectiid" = "projectiidref"),
                      res[["projectmapunit_View_1"]])

# get lookup table for specified tables
tkey <- get_NASIS_table_key_by_name(names(res))
fkeys <- tkey$fkey
names(fkeys) <- tkey$table

# specify important tables that need primary keys
tnprj <- c("project_View_1", "projectlandcatbreakdown_View_1", "projectmilestone_View_1")
tnprj_pkey <- .get_NASIS_pkey_by_name(tnprj)

# determine child tables of each of the above (will have a foreign key)
project_tables <- subset(tkey, pkeyref == tnprj_pkey[1] & pkeyref != fkey)
lcb_tables <- subset(tkey, pkeyref == tnprj_pkey[2] & pkeyref != fkey)
pmilestone_tables <- subset(tkey, pkeyref == tnprj_pkey[3] & pkeyref != fkey)

# use dm to set primary keys
resdm <- as_dm(res) %>%
  dm_add_pk(!!tnprj[1], !!tnprj_pkey[1]) %>% 
  dm_add_pk(!!tnprj[2], !!tnprj_pkey[2]) %>% 
  dm_add_pk(!!tnprj[3], !!tnprj_pkey[3]) 

# use dm to set foreign keys for project table
#  e.g projectstaff, projectmapunit, projectmilestone are children of project
fkey.sub <- fkeys[which(fkeys == unique(project_tables$fkey))]
for(f in seq_along(fkey.sub)) {
  resdm <- resdm %>%
    dm_add_fk(!!names(fkey.sub)[f], !!fkey.sub[f], ref_table = !!tnprj[1])
}

# set projectmappingprogress child of projectlandcatbreakdown
fkey.sub <- fkeys[which(fkeys == unique(lcb_tables$fkey))]
for(f in seq_along(fkey.sub)) {
  resdm <- resdm %>%
    dm_add_fk(!!names(fkey.sub)[f], !!fkey.sub[f], ref_table = !!tnprj[2])
}

# set projectmilestoneprogress child of projectmilestone
fkey.sub <- fkeys[which(fkeys == unique(pmilestone_tables$fkey))]
for(f in seq_along(fkey.sub)) {
  resdm <- resdm %>%
    dm_add_fk(!!names(fkey.sub)[f], !!fkey.sub[f], ref_table = !!tnprj[3])
}

# inspect a wimpy schema (but a fine demo)
resdm %>% 
  dm_draw()

# try flattening using {dm} and defined PKEY/FKEY relationship
projects_dm <- resdm %>%
  dm_flatten_to_tbl(start = "projectmapunit_View_1")

# compare column names
colnames(projects)
colnames(projects_dm)

# count number of project mapunits by project (basic join)
test1a <- projects %>%
  group_by(uprojectid) %>%
  count() %>%
  arrange(desc(n))
test1a

# count number of project mapunits by project (using {dm})
test1b <- projects_dm %>%
  group_by(uprojectid) %>%
  count() %>%
  arrange(desc(n))
test1b

diff_data(test1a, test1b)

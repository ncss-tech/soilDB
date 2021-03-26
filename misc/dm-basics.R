# working with a "static" (non-transactional, not authenticated)
# instance of the NASIS database with {soilDB}+{dm}
# 
# a demonstration of building the project data model

library(soilDB)
library(dm)
library(daff)
library(dplyr)

# get projects from selected set
res <- createStaticNASIS(soilDB:::.get_NASIS_table_name_by_purpose("project", 
                                                                   SS = TRUE))

# use full join manually
projects <- left_join(res[["project_View_1"]],
                      by = c("projectiid" = "projectiidref"),
                      res[["projectmapunit_View_1"]])

# use dm to set primary and foreign keys
resdm <- as_dm(res) %>%
  dm_add_pk("project_View_1", "projectiid") %>% 
  dm_add_pk("projectmappinggoal_View_1", "projectmappinggoaliid") %>% 
  dm_add_pk("projectconcern_View_1", "projectconcerniid") %>% 
  dm_add_pk("projectdataneed_View_1", "projectdataneediid") %>% 
  dm_add_pk("projectmilestone_View_1", "projectmilestoneiid") 
  
fkeys <- soilDB:::.get_NASIS_table_fkey_by_name(names(res))
pkeys <- soilDB:::.get_NASIS_table_pkey_by_name(names(res))

fkey.sub <- fkeys[which(fkeys == "projectiidref")]
for(f in seq_along(fkey.sub)) {
  resdm <- resdm %>%
    dm_add_fk(!!names(fkey.sub)[f], !!fkey.sub[f], ref_table = "project_View_1")
}

fkey.sub <- fkeys[which(fkeys == "projectlandcatbrkdniidref")]
for(f in seq_along(fkey.sub)) {
  resdm <- resdm %>%
    dm_add_fk(!!names(fkey.sub)[f], !!fkey.sub[f], ref_table = "projectmappinggoal_View_1")
}

fkey.sub <- fkeys[which(fkeys == "projectconcerntypedbiidref")]
for(f in seq_along(fkey.sub)) {
  resdm <- resdm %>%
    dm_add_fk(!!names(fkey.sub)[f], !!fkey.sub[f], ref_table = "projectconcern_View_1")
}

fkey.sub <- fkeys[which(fkeys == "projectdatatypedbiidref")]
for(f in seq_along(fkey.sub)) {
  resdm <- resdm %>%
    dm_add_fk(!!names(fkey.sub)[f], !!fkey.sub[f], ref_table = "projectdataneed_View_1")
}

fkey.sub <- fkeys[which(fkeys == "projectmilestoneiidref")]
for(f in seq_along(fkey.sub)) {
  resdm <- resdm %>%
    dm_add_fk(!!names(fkey.sub)[f], !!fkey.sub[f], ref_table = "projectmilestone_View_1")
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

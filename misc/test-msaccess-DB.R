dput(list.files("E:/AccessDBs", "db$", recursive=TRUE, full.names=TRUE))
# c(
#   "E:/AccessDBs/AKSite/AKSite.accdb",
#   "E:/AccessDBs/MT_RangeDB/RangeDB_back-end_1.5_20081205.mdb",
#   "E:/AccessDBs/NPS_PLOTS/PLOTS_v32_BE.accdb",
#   "E:/AccessDBs/PedonPC/pedon.accdb"
# )
library(soilDB)

veg_aksite <- get_veg_from_AK_Site("E:/AccessDBs/AKSite/AKSite.accdb")

# relies on backend too
veg_mt <- get_veg_from_MT_veg_db("E:/AccessDBs/MT_RangeDB/RangeDB_data_entry_form_ver-1G.mdb")
veg_mt_other <- get_veg_other_from_MT_veg_db("E:/AccessDBs/MT_RangeDB/RangeDB_data_entry_form_ver-1G.mdb")
veg_mt_species <- get_veg_species_from_MT_veg_db("E:/AccessDBs/MT_RangeDB/RangeDB_data_entry_form_ver-1G.mdb")

veg_nps <- get_veg_from_NPS_PLOTS_db("E:/AccessDBs/NPS_PLOTS/PLOTS_v32_BE.accdb")

pedonpc_hz <- get_hz_data_from_pedon_db("E:/AccessDBs/PedonPC/pedon.accdb")
pedonpc_site <- get_site_data_from_pedon_db("E:/AccessDBs/PedonPC/pedon.accdb")
pedonpc_colors <- get_colors_from_pedon_db("E:/AccessDBs/PedonPC/pedon.accdb")
pedonpc_extended <- get_extended_data_from_pedon_db("E:/AccessDBs/PedonPC/pedon.accdb")


## using version from CRAN
# 1.8.14

## development version
# 1.8.16

library(soilDB)
v <- utils::packageDescription("soilDB", field="Version")

## NASIS local DB:

# pedons (user site ID ~ *CA630* ): CA630 pedons
pedons_reference <- fetchNASIS()
save(pedons_reference, file=paste0('reference_data/pedons-CA630-', v, '.Rda'))

horizon_reference <- get_hz_data_from_NASIS_db()
save(horizon_reference, file=paste0('reference_data/hz_data-CA630-', v, '.Rda'))

color_reference <- get_colors_from_NASIS_db()
save(color_reference, file=paste0('reference_data/color_data-CA630-', v, '.Rda'))

extended_reference <- get_extended_data_from_NASIS_db()
save(extended_reference, file=paste0('reference_data/extended_data-CA630-', v, '.Rda'))

# components (Area/Lmapunit/Mapunit/DMU ~ CA630): CA630 legend
# components_reference <- fetchNASIS_component_data() # old version
components_reference <- fetchNASIS(what='components') # new version
save(components_reference, file=paste0('reference_data/components-CA630-', v, '.Rda'))

correlation_reference <- get_component_correlation_data_from_NASIS_db()
save(correlation_reference, file=paste0('reference_data/correlation-CA630-', v, '.Rda'))

comonth_reference <- get_comonth_from_NASIS_db()
save(comonth_reference, file=paste0('reference_data/comonth-CA630-', v, '.Rda'))

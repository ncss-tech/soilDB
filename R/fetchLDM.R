# new tools for querying lab data from SDA Lab Data Mart
# 

#' Query data from Kellogg Soil Survey Laboratory Data Mart via Soil Data Access
#' 
#' LDM model diagram: 
#' 
#'  - https://jneme910.github.io/Lab_Data_Mart_Documentation/Documents/SDA_KSSL_Data_model.html
#'
#' @param x a vector of values to find in column specified by `what`
#' @param what a single column name from either the `lab_combine_nasis_ncss` or the `lab_area` tables
#' @param chunk.size number of pedons per chunk (for queries that may exceed `maxJsonLength`)
#'
#' @return a `SoilProfileCollection`
#' @export
#'
#' @examples
#' 
#' # fetch by ssa_key
#' fetchLDM(8297, what = "ssa_key")
#' 
#' # get pedons correlated as taxonomic subgroup "Typic Argialbolls"
#' fetchLDM("Typic Argialbolls", what = "corr_taxsubgrp")
#'  
#' # TODO: generalize area_code functionality to include parent areas
#' 
#' ## fetch by area_code (must be a soil survey area code for now; the lowest-level area that is always (?) populated)
#' # fetchLDM("CA630", what = "area_code")
#' 
#' @importFrom aqp `depths<-` `site<-`
#' @importFrom data.table rbindlist
fetchLDM <- function(x, what = "pedlabsampnum", chunk.size = 1000) {
  what <- match.arg(what, choices = c("pedon_key", "site_key", "pedlabsampnum", "pedoniid", "upedonid", 
                                      "labdatadescflag", "priority", "priority2", "samp_name", "samp_class_type", 
                                      "samp_classdate", "samp_classification_name", "samp_taxorder", 
                                      "samp_taxsuborder", "samp_taxgrtgroup", "samp_taxsubgrp", "samp_taxpartsize", 
                                      "samp_taxpartsizemod", "samp_taxceactcl", "samp_taxreaction", 
                                      "samp_taxtempcl", "samp_taxmoistscl", "samp_taxtempregime", "samp_taxminalogy", 
                                      "samp_taxother", "samp_osdtypelocflag", "corr_name", "corr_class_type", 
                                      "corr_classdate", "corr_classification_name", "corr_taxorder", 
                                      "corr_taxsuborder", "corr_taxgrtgroup", "corr_taxsubgrp", "corr_taxpartsize", 
                                      "corr_taxpartsizemod", "corr_taxceactcl", "corr_taxreaction", 
                                      "corr_taxtempcl", "corr_taxmoistscl", "corr_taxtempregime", "corr_taxminalogy", 
                                      "corr_taxother", "corr_osdtypelocflag", "SSL_name", "SSL_class_type", 
                                      "SSL_classdate", "SSL_classification_name", "SSL_taxorder", "SSL_taxsuborder", 
                                      "SSL_taxgrtgroup", "SSL_taxsubgrp", "SSL_taxpartsize", "SSL_taxpartsizemod", 
                                      "SSL_taxceactcl", "SSL_taxreaction", "SSL_taxtempcl", "SSL_taxmoistscl", 
                                      "SSL_taxtempregime", "SSL_taxminalogy", "SSL_taxother", "SSL_osdtypelocflag", 
                                      "siteiid", "usiteid", "site_obsdate", "latitude_decimal_degrees", 
                                      "longitude_decimal_degrees", "country_key", "state_key", "county_key", 
                                      "mlra_key", "ssa_key", "npark_key", "nforest_key", "note", "samp_taxfamhahatmatcl", 
                                      "corr_taxfamhahatmatcl", "SSL_taxfamhahatmatcl", "pedobjupdate", 
                                      "siteobjupdate", "area_key", "area_type", "area_sub_type", "parent_area_key", 
                                      "parent_org_key", "area_code", "area_name", "area_abbrev", "area_desc"))
  
  # TODO: set up arbitrary area queries by putting area table into groups:
  #       country, state, county, mlra, ssa, npark, nforest
  
  # get site/pedon/area information
  sites <- SDA_query(sprintf(
            "SELECT * FROM lab_combine_nasis_ncss 
              LEFT JOIN lab_webmap ON 
                  lab_combine_nasis_ncss.pedon_key = lab_webmap.pedon_key
              LEFT JOIN lab_site ON 
                  lab_combine_nasis_ncss.site_key = lab_site.site_key
              LEFT JOIN lab_pedon ON 
                  lab_combine_nasis_ncss.site_key = lab_pedon.site_key
              LEFT JOIN lab_area ON 
                  lab_combine_nasis_ncss.ssa_key = lab_area.area_key
            WHERE %s IN %s", # final JOIN to SSA (most detailed required portion of lab_area table)
            what, format_SQL_in_statement(x)))
    
  if (!inherits(sites, 'try-error')) {
    
    sites <- sites[,unique(colnames(sites))]
    
    # get data for lab layers within pedon_key returned
    hz <- .get_lab_layer_by_pedon_key(sites$pedon_key)
    
    if (inherits(hz, 'try-error')) {
      chunk.idx <- makeChunks(sites$pedon_key, chunk.size)
      hz <- as.data.frame(data.table::rbindlist(lapply(unique(chunk.idx),
                                                   function(i) {
                                                     keys <- sites$pedon_key[chunk.idx == i]
                                                     res <- .get_lab_layer_by_pedon_key(keys)
                                                     if(inherits(res, 'try-error')) return(NULL)
                                                     res
                                                   })))
    }
    
    if (!is.null(hz) && nrow(hz) > 0) {  
      hz <- hz[,unique(colnames(hz))]
      hz$site_key <- NULL
      
      # build SoilProfileCollection
      depths(hz) <- pedon_key ~ hzn_top + hzn_bot
      site(hz) <- sites
      
      return(hz)
    } else {
      return(NULL)
    }
    
  }
  NULL
}

.get_lab_layer_by_pedon_key <- function(pedon_key) {
  suppressWarnings(SDA_query(sprintf(
            "SELECT * FROM lab_layer 
              LEFT JOIN lab_physical_properties ON 
                           lab_layer.labsampnum = lab_physical_properties.labsampnum
              LEFT JOIN lab_mineralogy_glass_count ON 
                           lab_layer.labsampnum = lab_mineralogy_glass_count.labsampnum
              LEFT JOIN lab_chemical_properties ON 
                           lab_layer.labsampnum = lab_chemical_properties.labsampnum
              LEFT JOIN lab_major_and_trace_elements_and_oxides ON 
                           lab_layer.labsampnum = lab_major_and_trace_elements_and_oxides.labsampnum
              LEFT JOIN lab_xray_and_thermal ON 
                           lab_layer.labsampnum = lab_xray_and_thermal.labsampnum
              LEFT JOIN lab_calculations_including_estimates_and_default_values ON 
                           lab_layer.labsampnum = lab_calculations_including_estimates_and_default_values.labsampnum
             WHERE pedon_key IN %s", 
            format_SQL_in_statement(pedon_key))))
            # TODO: rosetta key does not have labsampnum, leave it out for now 
  
}

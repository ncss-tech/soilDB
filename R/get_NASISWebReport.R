# foo <- readLines(file("misc/mlraproject_stephens_nasiswebreports_index.html"))
# # urls <-  foo[grep("report_name=(WEB-PROJECT.*)\\&", foo)]
# # x <- gsub(".*report_name=(WEB-PROJECT[^\\&]*)\\&?.*$", "\\1", urls)
# urls <-  foo[grep("report_name=(WEB_Stephens.*)\\&", foo)]
# x <- gsub(".*report_name=(WEB_Stephens[^\\&]*)\\&?.*$", "\\1", urls)
# dput(unique(x[nchar(x) > 0]))

# rpts <- c(
#   # "WEB-PROJECT-Data_QC_QA_Plan_Mapunits_List_and_Acres", 
#   # "WEB-PROJECT-Progr-Goals-Ac-Appr-by-MSSO-FY-Proj", 
#   "WEB-PROJECT-Reg%20Office%20Plan%20Proposal%20by%20proj%20name%20GMAP%202.2", 
#   "WEB-PROJECT-Data%20QC%20QA%20Properties%20Comparison%20v1.5", 
#   "WEB-PROJECT-Data%20QC%20QA%20Interp%20Muti%20Rules%20MANU%20html%20v1.1%20REAL", 
#   "WEB-PROJECT-Data%20QC%20QA%20Pedon%20List%20by%20Name,%20type,%20Proj%202.0", 
#   "WEB-PROJECT-CORR QC QA Generated Text Notes REAL v2.1", 
#   "WEB-PROJECT-CORR-Correlation_Letter_v1.31", 
#   "WEB-PROJECT-Data-QA-QC-Final-Check-Corr-Cert-Doc-HTML", 
#   "WEB-PROJECT_Description_Draft_Generator_and_table_tools_v2_6", 
#   "WEB-PROJECT-Data_MLRAMU_KitchenSink", 
#   "WEB-PROJECT-Data_MLRAMU_KitchenSink2"
# )
# 
# rpts <- c(
#     "WEB_Stephens_elevation_slope_aspect",
#     "WEB_Stephens_climate",
#     "WEB_Stephens_fragments_ALL_textures",
#     "WEB_Stephens_structure",
#     "WEB_Stephens_restrictions",
#     "WEB_Stephens_RV_parent_material_group",
#     "WEB_Stephens_geomorphic_properties",
#     "WEB_Stephens_diagnostic_features",
#     "WEB_Stephens_classification",
#     "WEB_Stephens_water_properties",
#     "WEB_Stephens_water_table_flooding_ponding",
#     "WEB_Stephens_ecosite_otherveg_class",
#     "WEB_Stephens_existing_plants",
#     "WEB_Stephens_trees_to_manage",
#     "WEB_Stephens_crop_yield",
#     "WEB_Stephens_surface_fragments",
#     "WEB_Stephens_subsidence_frost%20action",
#     "WEB_Stephens_pedons_linked_to_components"
#   )
# lst <- get_NASISWebProjects(msso = "2-SON", fy = 2021, project = "MLRA%")
# res <- sapply(rpts, get_NASISWebReport, proj_id = 183103)
# 
# sapply(res, function(x) print(class(x)))

#' @title Low-level methods for getting projects and tables from NASIS Web Reports
#' 
#' @description `get_NASISWebReport()`: Gets a table from the "ReportData" section of specified `report_name`
#' 
#' @param report_name internal report name
#' @param proj_id integer; Project record ID
#' @param verbose Default: `TRUE`; print messages with report name and parameters?
#' 
#' @aliases get_NASISWebProjects
#' 
#' @rdname get_NASISWebReport
#' 
#' @export
#' @importFrom rvest read_html, html_node, html_table
get_NASISWebReport <- function(report_name, proj_id, verbose = TRUE) {
  
  if (!requireNamespace("rvest")) {
    stop("package `rvest` is required", call. = FALSE)
  }
  
  base.url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx"
  
  if (verbose) {
    message(sprintf("Running report_name=%s for proj_id=%s...", report_name, proj_id))
  }
  
  report.url <- paste0(base.url, "?report_name=", report_name)
  report.project.url <- paste0(report.url, "&proj_id=", proj_id)
  
  res <- try(as.data.frame(rvest::html_table(rvest::html_node(rvest::read_html(report.project.url), 
                                                   xpath = '//*[@id="ReportData"]'), 
                                  header = TRUE)), silent = !verbose)
  if (inherits(res, 'try-error')) {
    return(invisible(res))
  }
  res
}

#' @description `get_NASISWebProjects()`: Gets a table of project name, state, approval statu and project record ID for specified office, fiscal year and project name pattern.
#' 
#' @param msso MLRA soil survey office e.g. `"2-SON"`
#' @param fy Fiscal year e.g. `2020`
#' @param project Expression to match in project name e.g. `"MLRA 18%"`
#' 
#' @return A data.frame, or (invisible) try-error if request fails
#' 
#' @rdname get_NASISWebReport
#' @export
#' @importFrom rvest read_html, html_node, html_nodes, html_attr, html_table
get_NASISWebProjects <- function(msso, fy, project, verbose = TRUE) {
  
  if (!requireNamespace("rvest")) {
    stop("package `rvest` is required", call. = FALSE)
  }
  report_name = "WEB_Stephens_PROJECT-REPORTS"
  base.url <- "https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx"
  
  if (verbose) {
    message(sprintf("Running report_name=%s for msso=%s, fy=%s, project=%s...", report_name, msso, fy, project))
  }
  
  report.url <- paste0(base.url, "?report_name=", report_name)
  report.project.url <- paste0(report.url, 
                               "&msso=", msso, 
                               "&fy=", fy, 
                               "&project=", URLencode(project))
  
  html <- try(rvest::read_html(report.project.url), silent = !verbose)
  
  if (inherits(html, 'try-error')) {
    return(invisible(html))
  }
  
  nd <- rvest::html_node(html, xpath = '//*[@id="ReportData"]')
  links <- rvest::html_attr(rvest::html_nodes(nd, "a"), "href")
  res <- try(as.data.frame(rvest::html_table(nd, header = TRUE))[,1:3], silent = !verbose)
  
  if (inherits(res, 'try-error')) {
    return(invisible(res))
  }
  
  res$proj_id <- unique(as.integer(gsub(".*&proj_id=(.*)$", "\\1", 
                                        links[grepl("&proj_id=(.*)$", links)])))
  colnames(res) <- c("project_name", "states", "approved", "proj_id")
  res
}


# filter_geochem
# 
#' @title Filter KSSL Geochemical Table
#' @description A function to subset KSSL "geochem" / elemental analysis result table to obtain rows/columns based on: column name, preparation code, major / trace element method.
#' @param geochem geochemical data, as returned by fetchKSSL
#' @param columns Column name(s) to include in result
#' @param prep_code Character vector of prep code(s) to include in result.
#' @param major_element_method Character vector of major element method(s) to include in result.
#' @param trace_element_method Character vector of trace element method(s) to include in result.
#' @return A data.frame, subsetted according to the constraints specified in arguments.
#' @author Andrew G. Brown.
#' @rdname filter_geochem
#' @export filter_geochem
filter_geochem <- function(geochem, 
                           columns=NULL,
                           prep_code=NULL, 
                           major_element_method=NULL, 
                           trace_element_method=NULL) {
  if(!inherits(geochem,'data.frame'))
    stop('filter_geochem expects a data.frame as input', call. = FALSE)
  
  if(is.null(columns))
    columns <- colnames(geochem)
  
  if(is.null(prep_code)) {
    prep.match <- rep(TRUE, nrow(geochem))
  } else {
    prep.match <- geochem$prep_code %in% prep_code
  }
  
  if(is.null(major_element_method)) {
    major.match <- rep(TRUE, nrow(geochem))
  } else {
    major.match <- geochem$major_element_method %in% major_element_method
  }
  
  if(is.null(trace_element_method)) {
    trace.match <- rep(TRUE, nrow(geochem))
  } else {
    trace.match <- geochem$trace_element_method %in% trace_element_method
  }
  
  return(geochem[which(prep.match & major.match & trace.match), columns])
}

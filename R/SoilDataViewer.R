#' Get Soil Data Viewer Attribute Information
#'
#' @param where WHERE clause for query of Soil Data Access `sdvattribute` table
#' @param alpha transparency value applied in calculation of hexadecimal color. Default: `255` (opaque).
#' @param notratedcolor Used to add 'Not rated' color entries where applicable. Default: `"#FFFFFF00"` (transparent white).
#' @param simplify Return a data.frame when `attributekey` is length 1? Default: `TRUE`
#'
#' @return A list with a data.frame element for each input `attributekey` containing `"attributekey"`, `"attributename"`, `"value"`, `"label"`, `"order"`, `"red"`, `"green"`, and `"blue"` columns.
#' @export
#'
#' @importFrom xml2 read_xml as_list 
get_SDV_legend_elements <- function(where,
                                    alpha = 255,
                                    notratedcolor = rgb(1, 1, 1, 0),
                                    simplify = TRUE) {
  
  y <- lapply(where, function(ak) {
    
    x <- SDA_query(paste0("SELECT attributekey, attributename, attributetype,
                                     attributetablename, attributecolumnname,
                                     attributedescription, maplegendxml, 
                                     nasisrulename, notratedphrase
                                   FROM sdvattribute WHERE ", ak))
    
    # parse map legend symbology/elements
    x2 <- xml2::as_list(xml2::read_xml(x$maplegendxml))
    
    # just returning the Legend_Elements as a data.frame
    res <- do.call('rbind', lapply(x2$Map_Legend$Legend_Elements, 
                                   function(z){ 
                                      d <- data.frame(
                                        attributekey = x$attributekey,
                                        attributename = x$attributename,
                                        attributetype = x$attributetype,
                                        attributetablename = x$attributetablename,
                                        attributecolumnname = x$attributecolumnname,
                                        attributedescription = x$attributedescription,
                                        nasisrulename = x$nasisrulename,
                                        value = attr(z, 'value'), # Are these ever different?
                                        label = attr(z, 'label'), # Are these ever different?
                                        order = attr(z, 'order'),
                                        red = attr(z$Color, 'red'),
                                        green = attr(z$Color, 'green'),
                                        blue = attr(z$Color, 'blue')
                                      )
                                      d$hex <- rgb(
                                        red = d$red,
                                        green = d$green,
                                        blue = d$blue,
                                        alpha = alpha,
                                        maxColorValue = 255
                                      )
                                      return(d)
                                  }))
    if (!is.na(x$notratedphrase) && x$notratedphrase != ""){
      dnr <- res[1,]
      dnr$value <- gsub("^not", "Not", x$notratedphrase)
      dnr$label <- dnr$value
      dnr$order = 0 
      dnr$red = NA
      dnr$green = NA
      dnr$blue = NA
      dnr$hex <- notratedcolor
      res <- rbind(res, dnr)
    }
    rownames(res) <- NULL
    res
  })
  if (length(y) == 1 & simplify) return(y[[1]])
  y
}
       


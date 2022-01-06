#' Get Soil Data Viewer Attribute Information
#'
#' @param WHERE WHERE clause for query of Soil Data Access `sdvattribute` table
#' @param alpha transparency value applied in calculation of hexadecimal color. Default: `255` (opaque).
#' @param notratedcolor Used to add 'Not rated' color entries where applicable. Default: `"#FFFFFF00"` (transparent white).
#' @param simplify Return a data.frame when `WHERE` is length 1? Return a list with 1 element per legend when `WHERE` is length > `1`? Default: `TRUE`
#'
#' @return A list with a data.frame element for each element of `where` containing `"attributekey"`, `"attributename"`, `"attributetype"`, `"attributetablename"`, `"attributecolumnname"`, `"attributedescription"`, `"nasisrulename"`, `"label"`, `"order"`, `"value"`, `"lower_value"`, `"upper_value"`,`"red"`, `"green"`, `"blue"` and `"hex"` columns.
#' @export
#'
#' @importFrom xml2 read_xml as_list 
get_SDV_legend_elements <- function(WHERE,
                                    alpha = 255,
                                    notratedcolor = rgb(1, 1, 1, 0),
                                    simplify = TRUE) {
  
  y <- lapply(WHERE, function(ak) {
    
    x <- SDA_query(paste0("SELECT attributekey, attributename, attributetype,
                                     attributetablename, attributecolumnname,
                                     attributedescription, maplegendxml, 
                                     nasisrulename, notratedphrase
                                   FROM sdvattribute WHERE ", ak))
    
    if (inherits(x, 'try-error'))
      stop(paste0("Invalid WHERE clause: ", ak), call. = FALSE)
    
    lapply(1:nrow(x), function(i) {
      .process_SDV_legend_elements(x[i, ], 
                                   alpha = alpha, 
                                   notratedcolor = notratedcolor)
    })
  })

  if ((length(y) == 1) && length(y[[1]]) == 1 && simplify) {
    return(y[[1]][[1]])
  } else if (simplify) return(do.call('c', y))
  y
}

.process_SDV_legend_elements <- function(x, 
                                         alpha = 255,
                                         notratedcolor = rgb(1, 1, 1, 0)) {
  
  # parse map legend symbology/elements
  x2 <- xml2::as_list(xml2::read_xml(x$maplegendxml))
  
  # just returning the Legend_Elements as a data.frame
  res <- do.call('rbind', lapply(x2$Map_Legend$Legend_Elements, 
                                 function(z){ 
                                   
                                   # handle single value labels versus upper/lower bounds
                                   val <- attr(z, 'value')
                                   attvalue <- data.frame(
                                     value = val,
                                     lower_value = NA[length(val)],
                                     upper_value = NA[length(val)]
                                   )
                                   if (nrow(attvalue) == 0) {
                                     attvalue <- data.frame(
                                       value = NA,
                                       lower_value = attr(z, 'lower_value'),
                                       upper_value = attr(z, 'upper_value')
                                     )
                                   }
                                   d <- data.frame(
                                     attributekey = x$attributekey,
                                     attributename = x$attributename,
                                     attributetype = x$attributetype,
                                     attributetablename = x$attributetablename,
                                     attributecolumnname = x$attributecolumnname,
                                     attributedescription = x$attributedescription,
                                     nasisrulename = x$nasisrulename,
                                     label = attr(z, 'label'),
                                     order = attr(z, 'order'))
                                   d <- cbind(d, attvalue)
                                   d2 <- data.frame(
                                     red = attr(z$Color, 'red'),
                                     green = attr(z$Color, 'green'),
                                     blue = attr(z$Color, 'blue')
                                   )
                                   if (nrow(d2) == 0) {
                                     d2 <- data.frame(red = NA, 
                                                      green = NA, 
                                                      blue = NA, 
                                                      hex = notratedcolor)
                                   } else {                                      
                                     newcolor <- rgb(
                                       red = d2$red,
                                       green = d2$green,
                                       blue = d2$blue,
                                       alpha = alpha,
                                       maxColorValue = 255
                                     )
                                     newcolor <- ifelse(length(newcolor) == 0, NA, newcolor)
                                     d2$hex <- newcolor
                                   }
                                   
                                   cbind(d, d2)
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
  type.convert(res, as.is = TRUE)
}
       


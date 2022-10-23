

#' Get SRI
#'
#' @description This function calls the ECOSHARE data access (zip files) to get Soil Inventory Resource (SRI) data. These
#' datasets contains both spatial and non-spatial data in the form of a gdb.
#' @param gdb A \code{character} of the gdb, e.g. \code{'Deschutes'}.
#' @param layer A \code{character} of the layer within the gdb, e.g. \code{'MapUnits'} (default).
#'
#' @return An \code{sf} or \code{data.frame} object.
#'
#' @note Please use \link{get_SRI_layers} to get the layer id information needed for the layer argument. This will
#' help with joining \code{sf} and \code{data.frame} objects.
#'
#' @details  Due to the fact that many Region 6 Forests do not have NRCS SSURGO surveys (at a scale of 1:24,000, these are the highest-resolution soils data generally available), Region 6 initiated a project in 2012 to bring these legacy SRI soils data into digital databases to facilitate their use in regional planning activities.  The datasets available on this page are the results of that effort.
#'
#' The SRI were originally compiled in 20 volumes, with the original year of publication ranging from 1969 to 1979. The Gifford-Pinchot SRI was redone following the eruption of Mt Saint Helens, and that version was published in 1992. The Olympic NF also produced two versions, the original version being published in 1969, with an update in 1982. The Colville National Forest was the only Region 6 forest that did not compile a SRI.
#'
#' The data are organized into one single regional geodatabase, together with twenty individual forest-level geodatabases.  The regional database contains polygons from all twenty SRIs together with a common set of attributes for the two or three soil layers delineated in the individual mapping unit descriptions, such as texture, depth, color, rock content, etc.  In general, the regional database contains physical soil attributes that could be compiled more or less completely and consistently across all forests. The individual forest-level databases contain the polygons for each individual SRI, together with various tables of management interpretations and laboratory data, together with a variety of miscellaneous tables.  The information contained in these forest-level databases varies widely from forest to forest, which is why they were not merged into a regional view.  Full metadata are included with each database, and scans of the original SRI volumes are provided for reference as well.  A Forest Service General Technical Report that fully describes the available data is currently in preparation.
#'
#' The gdb's currently available:
#' \itemize{
#' \item  \strong{Region6}
#' \item  \strong{Deschutes}
#' \item \strong{Fremont}
#' \item \strong{GiffordPinchot}
#' \item \strong{Malheur}
#' \item \strong{MtBaker}
#' \item  \strong{MtHood}
#' \item  \strong{Ochoco}
#' \item \strong{Okanogan}
#' \item \strong{Olympic}
#' \item \strong{RogueRiver}
#' \item \strong{Siskiyou}
#' \item  \strong{Siuslaw}
#' \item \strong{Umatilla}
#' \item \strong{Umpqua}
#' \item \strong{WallowaWhitman}
#' \item  \strong{Wenatchee}
#' \item \strong{Willamette}
#' \item \strong{Winema}
#' }

#' @export
#'
#' @examples
#' \donttest{
#'
#' #get Deschutes SRI
#' sri_deschutes <- get_SRI('Deschutes')
#' }
#'
get_SRI <- function(gdb, layer = 'MapUnits') {

  gdb <- ifelse(gdb == 'Willamette', 'Wilamette', gdb)

  temp <- tempfile(fileext = paste0(gdb, '.zip'))

  download.file(paste0('https://ecoshare.info/uploads/soils/soil_resource_inventory/',gdb,'_SoilResourceInventory.gdb.zip'),temp)

  temp2 <- tempfile()

  unzip(temp, exdir = temp2)

  sri <- sf::st_read(paste0(temp2, '/', gdb,'_SoilResourceInventory.gdb' ), layer = layer)

  unlink(temp)
  unlink(temp2)

  sri

  }

#' Get SRI Layers
#'
#' @param gdb A \code{character} of the gdb, e.g. \code{'Deschutes'}.
#'
#' @return A list of metadata about the gdb.
#' @export
#'
#' @note Refer to \link{get_SRI} for information on gdb availability.
#'
#' @examples
#' \donttest{
#' sri_layers <- get_SRI_layers('Willamette')
#' }
#'
get_SRI_layers <- function(gdb) {

  gdb <- ifelse(gdb == 'Willamette', 'Wilamette', gdb)
  sf::st_layers(paste0('https://ecoshare.info/uploads/soils/soil_resource_inventory/',gdb,'_SoilResourceInventory.gdb.zip'))

}


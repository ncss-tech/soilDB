
#' Fetch Soil Inventory Resource (SRI) for USFS Region 6
#'
#' @description This is a higher level wrapper around the \link{get_SRI} and \link{get_SRI_layers}
#' functions. This function can fetch multiple File Geodatabases (GDB) and returns all the layers within the GDB.
#' @param gdb A \code{character} vector of the GDB(s), e.g. \code{'Deschutes'}.
#' @param ... Arguments to pass to \link{get_SRI}.
#' @author Josh Erickson
#' @return A list.
#' @seealso `get_SRI()` `get_SRI_layers()`
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # fetch Willamette and Winema SRI
#'
#' sri <- fetchSRI(gdb = c('will', 'win'), quiet = TRUE)
#'
#' }

fetchSRI <- function(gdb, ...) {
  stop("US Forest Service 'Ecoshare' Region 6 Soil Resource Inventory datasets have moved to <https://www.fs.usda.gov/main/ecoshare/datasets/soils>. Please use the alternative data sources available there. `fetchSRI()` and related functions will be removed in the next minor release.", call. = FALSE)
  gdb <- .get_SRI_gdb_names(gdb)

  sapply(gdb, function(x) {
    suppressWarnings(get_SRI(x, layers = get_SRI_layers(x)$name, ...))
  })
}

#' Get Soil Inventory Resource (SRI) for USFS Region 6
#'
#' @description This function calls ECOSHARE (zip files) to get Soil Inventory Resource (SRI) data for USFS Region 6. These
#' datasets contain both spatial and non-spatial data in the form of a File Geodatabase (GDB).
#' @param gdb A \code{character} of the GDB, e.g. \code{'Deschutes'}.
#' @param layers A \code{character} of the layer(s) within the GDB, e.g. \code{'MapUnits'} (default).
#' @param quiet A \code{logical}; suppress info on name, driver, size and spatial reference, or signaling no or multiple layers.
#' @param simplify A \code{logical}; whether to return a simplified list (\code{data.frame} or \code{sf}) if length(layers) == 1.
#' @author Josh Erickson
#' @seealso `get_SRI_layers()`
#' @return An \code{sf} or \code{data.frame} object.
#'
#' @note Please use \code{\link{get_SRI_layers}} to get the layer id information needed for the layer argument. This will
#' help with joining \code{sf} and \code{data.frame} objects.
#'
#' @details  Due to the fact that many Region 6 Forests do not have NRCS SSURGO surveys (at a scale of 1:24,000, these are the highest-resolution soils data generally available), Region 6 initiated a project in 2012 to bring these legacy SRI soils data into digital databases to facilitate their use in regional planning activities.  The datasets available on this page are the results of that effort.
#'
#' The SRI were originally compiled in 20 volumes, with the original year of publication ranging from 1969 to 1979. The Gifford-Pinchot SRI was redone following the eruption of Mt Saint Helens, and that version was published in 1992. The Olympic NF also produced two versions, the original version being published in 1969, with an update in 1982. The Colville National Forest was the only Region 6 forest that did not compile a SRI.
#'
#' The data are organized into one single regional GDB, together with twenty individual forest-level GDBs.  The regional database contains polygons from all twenty SRIs together with a common set of attributes for the two or three soil layers delineated in the individual mapping unit descriptions, such as texture, depth, color, rock content, etc.  In general, the regional database contains physical soil attributes that could be compiled more or less completely and consistently across all forests. The individual forest-level databases contain the polygons for each individual SRI, together with various tables of management interpretations and laboratory data, together with a variety of miscellaneous tables.  The information contained in these forest-level databases varies widely from forest to forest, which is why they were not merged into a regional view.  Full metadata are included with each database, and scans of the original SRI volumes are provided for reference as well.  A Forest Service General Technical Report that fully describes the available data is currently in preparation.
#'
#' The GDB's currently available:
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
#' @examples
#' \dontrun{
#'
#' # get Deschutes SRI
#' sri_deschutes <- get_SRI('Deschutes')
#'
#' # get multiple layers in a list
#'
#' sri_deschutes_multiple <- get_SRI(gdb = 'Deschutes',
#' layers = c('MapUnits', 'ErosionAndHydro', 'SampleSites_MaterialsTesting'))
#'
#' }
#'
#'
get_SRI <- function(gdb, layers = 'MapUnits', quiet = FALSE, simplify = TRUE) {
  stop("US Forest Service 'Ecoshare' Region 6 Soil Resource Inventory datasets have moved to <https://www.fs.usda.gov/main/ecoshare/datasets/soils>. Please use the alternative data sources available there. `fetchSRI()` and related functions will be removed in the next minor release.", call. = FALSE)
  
  if (!requireNamespace("sf"))
    stop("package `sf` is required", call. = FALSE)

  gdb <- .get_SRI_gdb_names(gdb)

  sri <- list()

    for(i in layers){

      sri_get <- try(list(sf::read_sf(paste0('/vsizip//vsicurl/https://ecoshare.info/uploads/soils/soil_resource_inventory/',gdb,'_SoilResourceInventory.gdb.zip'), layer = i, quiet = quiet, as_tibble = FALSE)), silent = TRUE)

      names(sri_get) <- i

      sri <- append(sri, sri_get)

    }

  if(length(layers) == 1) {if(isTRUE(simplify)){sri <- sri[[1]]} else {sri}}

  sri

  }

#' Get SRI Layers
#'
#' @param gdb A \code{character} of the GDB, e.g. \code{'Deschutes'}.
#' @author Josh Erickson
#'
#' @return A list of metadata about the GDB
#' @export
#'
#' @note Refer to \code{\link{get_SRI}} for information on File Geodatabase (GDB) availability.
#'
#' @examples
#' \dontrun{
#' sri_layers <- get_SRI_layers('Willamette')
#' }
#'
get_SRI_layers <- function(gdb) {
  
  stop("US Forest Service 'Ecoshare' Region 6 Soil Resource Inventory datasets have moved to <https://www.fs.usda.gov/main/ecoshare/datasets/soils>. Please use the alternative data sources available there. `fetchSRI()` and related functions will be removed in the next minor release.", call. = FALSE)
  
  if (!requireNamespace("sf"))
    stop("package `sf` is required", call. = FALSE)

  gdb <- .get_SRI_gdb_names(gdb)

  layers <- try(sf::st_layers(paste0('/vsizip//vsicurl/https://ecoshare.info/uploads/soils/soil_resource_inventory/',gdb,'_SoilResourceInventory.gdb.zip')), silent = TRUE)

  as.data.frame(sapply(layers, I))
}


#' matching helper
#' @param gdb A character.
#' @return A gdb character.
#' @noRd
.get_SRI_gdb_names <- function(gdb) {

  gdb_names <- tolower(c('Region6', 'Deschutes', 'Fremont', 'GiffordPinchot', 'Malheur',
  'MtBaker', 'MtHood', 'Ochoco', 'Okanogan', 'Olympic', 'RogueRiver',
  'Siskiyou', 'Siuslaw', 'Umatilla', 'Umpqua', 'WallowaWhitman',
  'Wenatchee', 'Willamette', 'Winema', 'Region 6', 'Gifford Pinchot',
  'Mt. Baker', 'Mt. Hood', 'Mt Hood', 'Mt Baker',
  'Rogue River','Wallowa Whitman', 'Wilamette'
  ))

  gdb <- match.arg(tolower(gdb), choices = gdb_names, several.ok = TRUE)

  ifelse(gdb %in% c('region6', 'region 6'), 'Region6',
           ifelse(gdb %in% c('deschutes'), 'Deschutes',
           ifelse(gdb %in% c('fremont'), 'Fremont',
           ifelse(gdb %in% c('giffordpinchot', 'giford pinchot'), 'GiffordPinchot',
           ifelse(gdb %in% c('malheur'), 'Malheur',
           ifelse(gdb %in% c('mtbaker','mt. baker', 'mt baker'), 'MtBaker',
           ifelse(gdb %in% c('mthood','mt hood','mt. hood'), 'MtHood',
           ifelse(gdb %in% c('ochoco'), 'Ochoco',
           ifelse(gdb %in% c('okanogan'), 'Okanogan',
           ifelse(gdb %in% c('olympic'), 'Olympic',
           ifelse(gdb %in% c('rogueriver','rogue river'), 'RogueRiver',
           ifelse(gdb %in% c('siskiyou'), 'Siskiyou',
           ifelse(gdb %in% c('siuslaw'), 'Siuslaw',
           ifelse(gdb %in% c('umatilla'), 'Umatilla',
           ifelse(gdb %in% c('umpqua'), 'Umpqua',
           ifelse(gdb %in% c('wallowawhitman','wallowa whitman'), 'WallowaWhitman',
           ifelse(gdb %in% c('wenatchee'), 'Wenatchee',
           ifelse(gdb %in% c('willamette', 'wilamette'), 'Wilamette',
           ifelse(gdb %in% c('winema'), 'Winema', NA)))))))))))))))))))

}

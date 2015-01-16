# .onLoad <- function(lib, pkg)  {
#     packageStartupMessage("This is aqp ", utils::packageDescription("aqp", field="Version"), "\n", "see http://casoilresource.lawr.ucdavis.edu/drupal/taxonomy/term/56 for examples", appendLF = TRUE)
# }

.onAttach <- function(lib, pkg)  {
    packageStartupMessage("This is aqp ", utils::packageDescription("aqp", field="Version"), appendLF = TRUE)
}

## Utility functions for getting / prepping live (HTML) or local (TXT, OSDRegistry) OSDs.
## Migrated / adapted from parseOSD repo
## 2021-06-21
## D.E. Beaudette


# generate a link to the "live" HTML OSDs for a single soil series name
.seriesNameToURL <- function(s) {
  base.url <- 'http://soilseriesdesc.sc.egov.usda.gov/OSD_Docs/'
  s <- toupper(s)
  # convert space to _
  s <- gsub(pattern = ' ', replacement = '_', s, fixed = TRUE)
  # TODO: convert apostrophe
  
  # final URL
  u <- paste0(base.url, substr(s, 1, 1), '/', s, '.html')
  return(u)
}

# generate a link to local (TXT) OSDs, typically provided by OSDRegistry
.seriesNameToFileName <- function(s) {
  s <- toupper(s)
  # convert space to _
  s <- gsub(pattern = ' ', replacement = '_', s)
  # first-letter indexing
  u <- sprintf('%s/%s.%s', substr(s, 1, 1), s, 'txt')
  return(u)
}

# remove blank lines from HTML text
.removeBlankLines <- function(chunk) {
  # extract lines and remove blank / NA lines
  chunk.lines <- readLines(textConnection(chunk))
  chunk.lines <- chunk.lines[which(chunk.lines != '')]
  chunk.lines <- chunk.lines[which(!is.na(chunk.lines))]
  return(chunk.lines)
}

# get "live" OSD from HTML record, convert to lines of text (HTML stripped)
# s: a soil series name
.getLiveOSD <- function(s) {
  
  # sanity check
  if( !requireNamespace('rvest', quietly=TRUE))
    stop('please install the `rvest` package', call.=FALSE)
  
  # make URL
  u <- .seriesNameToURL(s)
  # get HTML content and strip blank / NA lines
  s.html.text <- rvest::html_text(rvest::read_html(u))
  s.html.text <- .removeBlankLines(s.html.text)
  # strip double quotes by converting to " inches"
  s.html.text <- gsub('"', ' inches', s.html.text)
  # done
  return(s.html.text)
}


# get local (TXT) OSD, typically from OSDRegistry working copy
.getLocalOSD <- function(s, path = '') {
  # file name
  fn <- .seriesNameToFileName(s)
  # local path
  fp <- file.path(path, fn)
  # load text lines
  s.text <- readLines(fp)
  # remove blank lines
  s.text <- s.text[which(s.text != '')]
  s.text <- s.text[which(!is.na(s.text))]
  # strip double quotes by converting to " inches"
  s.text <- gsub('"', ' inches', s.text)
  # done
  return(s.text)
}



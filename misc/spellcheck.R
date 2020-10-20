#
# check spelling, after ignoring a set of package-specific allowed words
# 
#install.packages("spelling")

do_spellcheck <- function() {
  allowed.words <- read.csv("misc/soilDB-spellcheck-allowed.csv")$x
  res <- spelling::spell_check_package()
  idx <- which(!res[,1] %in% allowed.words)
  if (length(idx) == 0) {
    message("No spelling errors found.")
    return(data.frame())
  }
  return(res[idx,])
}

do_spellcheck()

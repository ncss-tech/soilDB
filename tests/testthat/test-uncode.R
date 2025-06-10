test_that("uncode() works", {
  x <- data.frame(texcl = 1:10)
  expect_equivalent(uncode(x)$texcl, c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", "cosl", "sl")) 
})

test_that("uncode() works w/ NASISDomainsAsFactor(TRUE)", {
  NASISDomainsAsFactor(TRUE)
  x <- data.frame(texcl = 1:10)
  expect_equivalent(uncode(x)$texcl, structure(1:10, .Label = c("cos", "s", "fs", "vfs", "lcos", "ls", 
                                                           "lfs", "lvfs", "cosl", "sl", "fsl", "vfsl", "l", "sil", "si", 
                                                           "scl", "cl", "sicl", "sc", "sic", "c"), class = "factor")) 
  NASISDomainsAsFactor(FALSE)
})

test_that("code() works", {
  x <- data.frame(texcl = c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", "cosl", "sl"))
  expect_equivalent(code(x)$texcl, 1:10) 
  
  # heterogeneous names and labels
  x <- data.frame(texcl = c("cos", "Sand", "fs", "vfs", "lcos", "Loamy sand", "lfs", "lvfs", "cosl", "Sandy loam"))
  expect_equivalent(code(x)$texcl, 1:10) 
})

test_that("code() works w/ NASISDomainsAsFactor(TRUE)", {
  NASISDomainsAsFactor(TRUE)
  x <- data.frame(texcl = c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", "cosl", "sl"))
  expect_equivalent(code(x)$texcl, 1:10) 
  NASISDomainsAsFactor(FALSE)
})

test_that("NASISChoiceList() works", {
  x <- NASISChoiceList(1:3, colnames = "texcl")
  expect_equivalent(x, structure(c(3L, 12L, 5L), .Label = c("c", "cl", "cos", "cosl", 
                                       "fs", "fsl", "l", "lcos", "lfs", "ls", "lvfs", "s", "sc", "scl", 
                                       "si", "sic", "sicl", "sil", "sl", "vfs", "vfsl"), class = "factor"))
  
  x <- NASISChoiceList(1:3, colnames = "pondfreqcl", factor = FALSE)
  expect_equivalent(x, c('none', 'rare', 'occasional'))
  
  # convert a label to value
  x <- NASISChoiceList("Clay loam", colnames = "texcl", choice = "ChoiceValue")
  expect_equivalent(x, 17L)

  # ordered factor including obsolete choices
  x <- NASISChoiceList("common", colnames = "flodfreqcl", choice = "ChoiceName", obsolete = TRUE)
  expect_equivalent(x, structure(5L, .Label = c("none", "very rare", "rare", "occasional", 
                                           "common", "frequent", "very frequent"), 
                            class = c("ordered", "factor")))
  
  # obsolete value, ordered factor excluding obsolete choices
  x <- NASISChoiceList("common", colnames = "flodfreqcl", choice = "ChoiceName")
  expect_equivalent(x, structure(NA_integer_, levels = c("none", "very rare", "rare", 
                                                   "occasional", "frequent", "very frequent"), 
                            class = c("ordered", "factor")))
})

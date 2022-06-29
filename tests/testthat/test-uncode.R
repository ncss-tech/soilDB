test_that("uncode() works", {
  x <- data.frame(texcl = 1:10)
  expect_equal(uncode(x)$texcl, c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", "cosl", "sl")) 
})

test_that("uncode() works w/ NASISDomainsAsFactor(TRUE)", {
  NASISDomainsAsFactor(TRUE)
  x <- data.frame(texcl = 1:10)
  expect_equal(uncode(x)$texcl, structure(1:10, .Label = c("cos", "s", "fs", "vfs", "lcos", "ls", 
                                                           "lfs", "lvfs", "cosl", "sl", "fsl", "vfsl", "l", "sil", "si", 
                                                           "scl", "cl", "sicl", "sc", "sic", "c"), class = "factor")) 
  NASISDomainsAsFactor(FALSE)
})

test_that("code() works", {
  x <- data.frame(texcl = c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", "cosl", "sl"))
  expect_equal(code(x)$texcl, 1:10) 
})

test_that("code() works w/ NASISDomainsAsFactor(TRUE)", {
  NASISDomainsAsFactor(TRUE)
  x <- data.frame(texcl = c("cos", "s", "fs", "vfs", "lcos", "ls", "lfs", "lvfs", "cosl", "sl"))
  expect_equal(code(x)$texcl, 1:10) 
  NASISDomainsAsFactor(FALSE)
})

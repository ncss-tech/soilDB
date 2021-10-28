context("Simplification of artifact data (from NASIS)")

## some complex data from NASIS phhuart table
d.artifact.hz <- structure(list(phiid = c(10101, 10101, 10102), 
                              huartvol = c(10, 5, 95), 
                              huartsize_l = c(2, 76, 2), 
                              huartsize_r = c(39, 163, 39), 
                              huartsize_h = c(75, 250, 75), 
                              huartkind = c("boiler slag", "boiler slag", "boiler slag"), 
                              huartco = c("cohesive", "cohesive", "noncohesive"), 
                              huartshp = c("irregular", "irregular", "flat"), 
                              huartrnd = c("subrounded", "subrounded", "angular"), 
                              huartpen = c("nonpenetrable", "nonpenetrable", "penetrable"), 
                              huartsafety = c("innocuous artifacts","innocuous artifacts", "innocuous artifacts"), 
                              huartper = c("persistent", "persistent", "persistent"), 
                              recwlupdated = structure(c(NA_real_, NA_real_, NA_real_), class = c("POSIXct", "POSIXt"), tzone = ""), 
                              recuseriidref = c(NA_integer_, NA_integer_, NA_integer_), 
                              phhuartiid = c(NA_integer_, NA_integer_, NA_integer_)), 
                              row.names = c(NA, 3L), class = "data.frame")

## new tests for rockFragmentSieve: missing frag sizes / unspecified class
test_that("artifactSieve puts artifacts without huartsize into 'unspecified' class", {
  
  d <- data.frame(huartvol=25, huartsize_l=NA, huartsize_r=NA, huartsize_h=NA, 
                  huartshp=NA, huartco=NA, huartshp=NA, huartrnd=NA, huartpen=NA, 
                  huartsafety=NA, huartper=NA)
  res <- soilDB:::.artifactSieve(d)
  
  expect_equal(res$class, 'art_unspecified')
  
})


test_that("artifactSieve assumptions are applied, results correct", {
  
  d <- data.frame(huartvol=25, huartsize_l=NA, huartsize_r=50, huartsize_h=NA, 
                  huartshp=NA, huartco=NA, huartshp=NA, huartrnd=NA, huartpen=NA, 
                  huartsafety=NA, huartper=NA)
  res <- soilDB:::.artifactSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$huartshp, 'irregular')
  expect_equal(res$huartco, 'cohesive')
  
})


test_that("artifactSieve assumptions are applied when all NA", {
  
  d <- data.frame(huartvol=NA, huartsize_l=NA, huartsize_r=NA, huartsize_h=NA, 
                  huartshp=NA, huartco=NA, huartshp=NA, huartrnd=NA, huartpen=NA, 
                  huartsafety=NA, huartper=NA)
  res <- soilDB:::.artifactSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$huartshp, 'irregular')
  expect_equal(res$huartco, 'cohesive')
  
  # class should be NA
  expect_true(is.na(res$class))
  
})


test_that("artifactSieve safe fall-back from high to rv fragsize", {
  
  # full specification
  d <- data.frame(huartvol=10, huartsize_l=15, huartsize_r=50, huartsize_h=75, 
                  huartshp="irregular", huartco="cohesive", huartshp=NA, huartrnd=NA, huartpen=NA, 
                  huartsafety=NA, huartper=NA)
  res <- soilDB:::.artifactSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$huartshp, 'irregular')
  expect_equal(res$huartco, 'cohesive')
  
  # correct class in the absence of fragment shape / hardness
  expect_equal(res$class, 'art_gr')
  
  # only RV available
  d <- data.frame(huartvol=10, huartsize_l=NA, huartsize_r=50, huartsize_h=NA, 
                  huartshp="irregular", huartco="cohesive", huartshp=NA, huartrnd=NA, huartpen=NA, 
                  huartsafety=NA, huartper=NA)
  res <- soilDB:::.artifactSieve(d)
  
  # assumptions in the absence of fragment shape / hardness
  expect_equal(res$huartshp, 'irregular')
  expect_equal(res$huartco, 'cohesive')
  
  # correct class in the absence of fragment shape / hardness
  expect_equal(res$class, 'art_gr')
  
})




test_that("artifactSieve complex sample data from NASIS, single horizon", {
  
  # pretty common, many fragments specified for a single horizon
  res <- soilDB:::.artifactSieve(d.artifact.hz)
  
  # correct classes
  expect_equal(res$class, c('art_gr', 'art_cb', 'art_ch'))
  
})


test_that("simplifyArtifactData complex sample data from NASIS, single horizon", {
  
  # pretty common, many fragments specified for a single horizon
  res <- soilDB::simplifyArtifactData(d.artifact.hz, id.var = 'phiid', nullFragsAreZero = TRUE)
  
  # correct class totals
  expect_equal(res$art_fgr, c(0,0))
  expect_equal(res$art_gr, c(10,0))
  expect_equal(res$art_cb, c(5,0))
  expect_equal(res$art_ch, c(0,95))
  expect_equal(res$art_fl, c(0,0))
  expect_equal(res$art_st, c(0,0))
  
  # correct total 
  expect_equal(res$total_art_pct, c(15,95))
  
  # correct subtotal cohesive
  expect_equal(res$huartvol_cohesive, c(15, 0))  
  
  # correct subtotal penetrable
  expect_equal(res$huartvol_penetrable,  c(0,95))  
  
  # correct subtotal noxious
  expect_equal(res$huartvol_noxious, c(0,0))
  
  # correct subtotal persistent
  expect_equal(res$huartvol_persistent, c(15,95))
  
})



test_that("simplifyArtifactData when missing fragment sizes, low/rv/high", {
  
  # all fragments are coallated into the unspecified column
  # totals should be correct
  # some horizons have no fragment records, should generate a warning
  d.missing.size <- d.artifact.hz
  d.missing.size[,c("huartsize_l", "huartsize_r", "huartsize_h")] <- NA
  d.missing.size[4,] <- d.missing.size[3,]
  d.missing.size[4,] <- NA
  d.missing.size[4,'phiid'] <- "10102"
  expect_message(res <- simplifyArtifactData(d.missing.size, id.var = 'phiid', nullFragsAreZero = TRUE))
  
  # rows missing fragvol should be removed from the simplified result
  expect_true(nrow(d.missing.size) == 4)
  expect_true(nrow(res) == 2)
  
  # unspecified total should match RF sums
  expect_equal(res$art_unspecified, res$total_art_pct)
  
  # totals lesss than or equal to 100
  expect_equal(all(res$total_art_pct <= 100), TRUE)
  expect_equal(all(res$huartvol_cohesive <= 100), TRUE)  
  expect_equal(all(res$huartvol_penetrable <= 100), TRUE)  
  expect_equal(all(res$huartvol_noxious <= 100), TRUE)
  expect_equal(all(res$huartvol_persistent <= 100), TRUE)
  
})


test_that("simplifyArtifactData warning generated when NA in huartvol", {
  d.missing.artvol <- d.artifact.hz
  d.missing.artvol$huartvol <- NA
  d.missing.artvol[1,'huartvol'] <- 10
  expect_message(simplifyArtifactData(d.missing.artvol, id.var = 'phiid', nullFragsAreZero = TRUE),
                 regexp = 'some records are missing artifact volume')
})


test_that("simplifyArtifactData warning generated when all fragvol are NA", {
  d.all.NA.artvol <- d.artifact.hz
  d.all.NA.artvol$huartvol <- NA
  expect_message(simplifyArtifactData(d.all.NA.artvol, id.var = 'phiid', nullFragsAreZero = TRUE),
                 regexp = 'all records are missing artifact volume')
})

test_that("simplifyArtifactData nullFragsAreZero works as expected", {
  d.missing.artvol <- d.artifact.hz
  a <- simplifyArtifactData(d.missing.artvol, id.var = 'phiid', nullFragsAreZero = FALSE)
  b <- simplifyArtifactData(d.missing.artvol, id.var = 'phiid', nullFragsAreZero = TRUE)
  expect_equal(as.logical(is.na(a)), 
               c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, 
                 TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, 
                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
               ))
  expect_true(!all(as.logical(is.na(b))))
})

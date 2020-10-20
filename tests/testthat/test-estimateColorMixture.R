context("estimating color mixtures")

## mixing done in CIE LAB by default now


## example data
# http://ncss-tech.github.io/AQP/soilDB/mixing-soil-color-data.html
colors <- c('10YR 5/6', '7.5YR 6/2')
weights <- c(80, 20)

# re-make the data expected by estimateColorMixture()
x <- cbind(
  parseMunsell(colors, convertColors=FALSE),
  parseMunsell(colors, return_triplets=TRUE, returnLAB=TRUE),
  pct=weights,
  col=parseMunsell(colors, convertColors=TRUE)
) 

names(x)[1:3] <- paste0('color', names(x)[1:3])
x$id <- 1
x$colormoistst <- "Dry"


# setting a baseline here, there is nothing behind the results apart from anecdotal evidence
test_that("estimateColorMixture basic functionality", {
  
  # there is no back-transformation to Munsell by default, request it
  res <- estimateColorMixture(x, backTransform = TRUE)
  
  # should be a single row
  expect_equal(nrow(res), 1)
  
  # check closest Munsell chip, should be ~ 10YR 5/5
  expect_equal(res$colorhue, '10YR')
  expect_equal(res$colorvalue, 5)
  expect_equal(res$colorchroma, 5)
  
  # check sRGB coordinates
  expect_equal(res$r, 0.62591)
  expect_equal(res$g, 0.4755)
  expect_equal(res$b, 0.27674)
  
})

## TODO: many more examples of colors + resulting mixutres

## TODO: test mixing in the absence of color percentages


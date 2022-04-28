test_that("get_SDA_cosurfmorph works", {
  
  skip_on_cran()
  
  skip_if_offline()
  
   # Summarize by 3D geomorphic components by component name (default `by='compname'`)
  x <- get_SDA_cosurfmorph(WHERE = "areasymbol = 'CA630'")
  expect_true(inherits(x, 'data.frame'))
  
   # Whole Soil Survey Area summary (using `by = 'areasymbol'`)
  x <- get_SDA_cosurfmorph(WHERE = "areasymbol = 'CA630'", by = 'areasymbol')
  expect_true(inherits(x, 'data.frame'))

   # 2D Hillslope Position summary(using `table = 'cosurfmorphhpp'`)
  x <- get_SDA_cosurfmorph(WHERE = "areasymbol = 'CA630'", table = 'cosurfmorphhpp')
  expect_true(inherits(x, 'data.frame'))

   # Surface Shape summary (using `table = 'cosurfmorphss'`)
  x <- get_SDA_cosurfmorph(WHERE = "areasymbol = 'CA630'", table = 'cosurfmorphss')
  expect_true(inherits(x, 'data.frame'))
})

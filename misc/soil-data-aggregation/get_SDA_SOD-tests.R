library(soilDB)

# select a single mukey to check that top and bottom depth and different methods produce stable results
keys <- c(463276)

s1 <- get_SDA_property(property = 'Available Water Capacity - Rep Value', 
                       method = 'Weighted Average', 
                       mukeys = keys, 
                       top_depth = 0, 
                       bottom_depth = 200)
s2 <- get_SDA_property(property = 'Available Water Capacity - Rep Value', 
                       method = 'Weighted Average', 
                       mukeys = keys, 
                       top_depth = 0, 
                       bottom_depth = 1000)
testthat::expect_equal(s1, s2)
testthat::expect_equal(s1$awc_r, 0.13)

s3 <- get_SDA_property(property = 'Available Water Capacity - Rep Value', 
                       method = 'Dominant Component (Numeric)', 
                       mukeys = keys, 
                       top_depth = 0, 
                       bottom_depth = 200)
s4 <- get_SDA_property(property = 'Available Water Capacity - Rep Value', 
                       method = 'Dominant Component (Numeric)', 
                       mukeys = keys, 
                       top_depth = 0, 
                       bottom_depth = 1000)
testthat::expect_equal(s3, s4)
testthat::expect_equal(s3$awc_r, 0.18)

s5 <- get_SDA_interpretation(rulename = 'FOR - Mechanical Planting Suitability', 
                       method = 'Weighted Average', 
                       mukeys = keys)
testthat::expect_equal(s5$rating_FORMechanicalPlantingSuitability, 0.99)

s6 <- get_SDA_interpretation(rulename = 'FOR - Mechanical Planting Suitability', 
                       method = 'Dominant Component', 
                       mukeys = keys)
testthat::expect_equal(s6$rating_FORMechanicalPlantingSuitability, 0.987)

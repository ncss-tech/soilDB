test_that("Low-level NASIS Web Report tools works", {
  res <- get_NASISWebProjects(msso = "2-SON", fy = 2018, project = "MLRA 18%")
  expect_equal(nrow(res), length(unique(res$proj_id)))
  
  expect_true(nrow(get_NASISWebReport("WEB_Stephens_pedons_linked_to_components", res$proj_id[1])) > 0)
})

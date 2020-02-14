test_that("data functions work", {
  # case of multiple EMS_Code only one row output
  x <- wqg_filter("Fluoride Total", "Aquatic Life - Freshwater", "Water", 
                  "Short-term acute", "No Effect", "max")
  expect_identical(nrow(x), 2L)
  expect_length(unique(x$EMS_Code), 1L)
  
  x <- wqg_clean(x)
  expect_length(x, 17L)
})

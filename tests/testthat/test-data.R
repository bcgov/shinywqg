test_that("data functions work", {
  
  cvalues <- list(EMS_0107 = 9, EMS_1107 = 5)
  ### test LimitNotes
  x <- wqg_filter("Chloride Dissolved", "Aquatic Life - Marine", "Water", 
                  "Long-term chronic", "No Effect", "mean")
  expect_identical(nrow(x), 2L)
  expect_length(unique(x$EMS_Code), 1L)
  
  y <- wqg_evaluate(x, cvalues, 2)
  expect_identical(sum(y$ConditionPass), 2L)
  expect_identical(y$Guideline, c("10% decrease background", 
                                  "10% increase background"))
  
  # case of multiple EMS_Code only one row output
  x <- wqg_filter("Fluoride Total", "Aquatic Life - Freshwater", "Water", 
                  "Short-term acute", "No Effect", "max")
  expect_identical(nrow(x), 2L)
  expect_length(unique(x$EMS_Code), 1L)
  
  ## test conditions and guidelines
  y <- wqg_evaluate(x, list(EMS_0107 = 5, EMS_1107 = 12), 2)
  expect_identical(sum(y$ConditionPass), 2L)
  
  y <- wqg_evaluate(x, list(EMS_0107 = 9, EMS_1107 = 5), 2)
  expect_identical(sum(y$ConditionPass), 1L)
  expect_identical(y$Guideline, c("<= 0.4 (mg/L)", "<= 0.37 (mg/L)"))
  
  ### check sigfig
  y <- wqg_evaluate(x, list(EMS_0107 = 9, EMS_1107 = 5), 3)
  expect_identical(y$Guideline, c("<= 0.4 (mg/L)", "<= 0.366 (mg/L)"))
  
  ### check clean - one row
  y <- wqg_clean(y)
  expect_identical(nrow(y), 1L)
  expect_identical(y$Guideline, "<= 0.4 (mg/L)")
  
  ### case of missing EMS_Code
  x <- wqg_filter("Linear alkylbenzene sulphonates (LAS)",
                  "Aquatic Life - Freshwater",
                  "Water", "Long-term chronic",
                  "No Effect", "mean")
  expect_true(is.na(x$EMS_Code))
  expect_identical(nrow(x), 1L)
  
})

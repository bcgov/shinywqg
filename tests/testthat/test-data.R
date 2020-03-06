test_that("data functions work", {

  cvalues <- list(EMS_0107 = 9, EMS_1107 = 5, EMS_1104 = 10)
  ### test LimitNotes
  x <- wqg_filter("Chloride", "Dissolved", "Aquatic Life - Marine", "Water",
    "Long-term chronic", "No Effect")
  expect_identical(nrow(x), 2L)
  expect_length(unique(x$EMS_Code), 1L)

  y <- wqg_evaluate(x, cvalues)
  expect_identical(sum(y$ConditionPass), 2L)
  expect_identical(y$Guideline, c(9, 11))

  # case of multiple EMS_Code only one row output
  x <- wqg_filter("Fluoride", "Total", "Aquatic Life - Freshwater", "Water",
    "Short-term acute", "No Effect")
  expect_identical(nrow(x), 2L)
  expect_length(unique(x$EMS_Code), 1L)

  ## test conditions and guidelines
  y <- wqg_evaluate(x, list(EMS_0107 = 5, EMS_1107 = 12))
  expect_identical(sum(y$ConditionPass), 2L)

  y <- wqg_evaluate(x, list(EMS_0107 = 9, EMS_1107 = 5))
  expect_identical(sum(y$ConditionPass), 1L)
  
  z <- wqg_clean(y, 2)
  expect_identical(z$Guideline, c("<= 0.4 (mg/L)"))
  
  # check cvalue = NA
  y <- wqg_evaluate(x, list(EMS_0107 = 9, EMS_1107 = NA))
  expect_identical(sum(y$ConditionPass), 1L)
  
  z <- wqg_clean(y, 2)
  expect_identical(z$Guideline, c("<= 0.4 (mg/L)"))

  ### check sigfig
  y <- wqg_evaluate(x, list(EMS_0107 = 11, EMS_1107 = NA))
  z <- wqg_clean(y, 3)
  expect_identical(z$Guideline, c("<= 0.447 (mg/L)"))

  ### case of missing EMS_Code
  x <- wqg_filter("Linear alkylbenzene sulphonates (LAS)", "Total",
    "Aquatic Life - Freshwater",
    "Water", "Long-term chronic",
    "No Effect")
  expect_true(is.na(x$EMS_Code))
  expect_identical(nrow(x), 1L)

  ### egt_table works
  x <- wqg_filter("Ammonia", "Total",
    "Aquatic Life - Freshwater",
    "Water", c("Long-term chronic"),
    "No Effect")

  cvalues <- list(EMS_0004 = 9, EMS_0013 = 10)
  y <- wqg_evaluate(x, cvalues) %>% wqg_clean(2)
  expect_identical(nrow(y), 1L)
})

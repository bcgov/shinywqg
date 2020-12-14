test_that("data functions work", {

  cvalues <- list(EMS_0107 = 9, EMS_1107 = 5, EMS_1104 = 10)
  ### test LimitNotes
  x <- wqg_filter("Chloride", "Aquatic Life - Marine", "Water")
  expect_identical(nrow(x), 2L)
  expect_length(unique(x$EMS_Code), 1L)

  y <- wqg_evaluate(x, cvalues)
  expect_identical(sum(y$ConditionPass), 2L)
  expect_identical(y$Guideline, c(9, 11))

  # case of multiple EMS_Code only one row output
  x <- wqg_filter("Fluoride", "Aquatic Life - Freshwater", "Water")
  expect_identical(nrow(x), 2L)
  expect_length(unique(x$EMS_Code), 1L)

  ## test conditions and guidelines
  y <- wqg_evaluate(x, list(EMS_0107 = 5, EMS_1107 = 12))
  expect_identical(sum(y$ConditionPass), 2L)

  y <- wqg_evaluate(x, list(EMS_0107 = 9, EMS_1107 = 5))
  expect_identical(sum(y$ConditionPass), 1L)
  
  z <- wqg_clean(y, 2)
  expect_identical(z$Guideline, c("<= 0.4 (mg/L) "))
  
  # check cvalue = NA
  y <- wqg_evaluate(x, list(EMS_0107 = 9, EMS_1107 = NA))
  expect_identical(sum(y$ConditionPass), 1L)
  
  z <- wqg_clean(y, 2)
  expect_identical(z$Guideline, c("<= 0.4 (mg/L) "))

  ### check sigfig
  y <- wqg_evaluate(x, list(EMS_0107 = 11, EMS_1107 = NA))
  z <- wqg_clean(y, 3)
  expect_identical(z$Guideline, c("<= 0.447 (mg/L) "))

  ### case of missing EMS_Code
  x <- wqg_filter("Linear alkylbenzene sulphonates (LAS)", 
    "Aquatic Life - Freshwater",
    "Water")
  expect_true(is.na(x$EMS_Code))
  expect_identical(nrow(x), 1L)

  ### egt_table works
  x <- wqg_filter("Ammonia", 
    "Aquatic Life - Freshwater",
    "Water")

  cvalues <- list(EMS_0004 = 9, EMS_0013 = 10)
  y <- wqg_evaluate(x, cvalues) %>% wqg_clean(2)
  expect_identical(nrow(y), 2L)
  
  ### check NarrativeWQG
  var <- dplyr::filter(limits, !is.na(.data$NarrativeWQG))
  x <- wqg_filter("Colour True", 
                  "Aquatic Life - Freshwater",
                  "Water") %>%
    wqg_evaluate(cvalues) %>%
    wqg_clean()
  expect_true(all(is.na(x$Notes)))
  expect_identical(x$Guideline[1], NA)
  expect_identical(x$`WQG Narrative`, "30-day average true colour in filtered water samples should not exceed background levels by more than 5 mg/L Pt in clear water systems or 20% in coloured water systems.")
  
  ### check ConditionNotes and missing limits/narrative/limitnotes
  cvalues$EMS_0004 <- 4
  x <- wqg_filter("Resin acids", 
                  "Aquatic Life - Freshwater",
                  "Water") %>%
    wqg_evaluate(cvalues) %>%
    wqg_clean(2)
  expect_true(is.na(x$Guideline))
  expect_identical(x$Notes, "No WQG value for pH < 5, site assessment may be necessary. ")

  ### check Look-up
  # cvalues <- list(EMS_0004 = 4.8, EMS_1126 = 0.05, EMS_0107 = 8)
  # x <- wqg_filter("Copper", 
  #                 "Aquatic Life - Freshwater",
  #                 "Water", 
  #                 x = limits) 
  # # issue with file path location, this will be fixed once data is uploaded to BC Data Cat
  # # fix to your machine until data is uploaded 
  # setwd("~/Code/shinywqg/inst/app")
  # y <- wqg_evaluate(x, cvalues) 
  #   wqg_clean()
  # expect_identical(y$Condition[1], "EMS_0004 EMS_1126 EMS_0107 ")
  # expect_identical(y$Guideline[1], 0.2)
  # expect_identical(y$ConditionPass[1], 0.2)
  
  
  
  
  
  
  
    
})

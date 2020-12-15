library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(wqbc)

limits <- readr::read_csv("https://raw.githubusercontent.com/bcgov/wqg_data/master/all_wqgs.csv")
# limits <-  bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")

## modify limits to be what databc should be
## switch off code to read from databc therefore uses internals.
## once working then update databc with new limits and lookups.
limits$Units[limits$Variable == "Copper" & limits$Component == "Dissolved"] <- "ug/L"
limits$Direction[limits$Variable == "Copper" & limits$Component == "Dissolved"] <- "Upper Limit"
limits$Limit[limits$Variable == "Copper" &
          limits$Use == "Aquatic Life - Freshwater" &
          limits$Media == "Water" &
          limits$Type == "Short-term acute"] <- "EMS_CU-D_water_aquatic_fresh_acute_lookup.csv"
limits$Limit[limits$Variable == "Copper" &
          limits$Use == "Aquatic Life - Freshwater" &
          limits$Media == "Water" &
          limits$Type == "Long-term chronic"] <- "EMS_CU-D_water_aquatic_fresh_chronic_lookup.csv"
limits$LimitNotes[limits$Variable == "Copper" & limits$Component == "Dissolved"] <- NA

## also add lookups to internal as well.
## will need to change to bcgov repo once they have been uploaded there
cu_h20_aq_fresh_acute_lookup <- readr::read_csv("data-raw/EMS_CU-D_water_aquatic_fresh_acute_lookup.csv")
cu_h20_aq_fresh_chronic_lookup <- readr::read_csv("data-raw/EMS_CU-D_water_aquatic_fresh_chronic_lookup.csv")
  

codes <-  wqbc::codes
codes <- codes %>% dplyr::rename(EMS_Code = Code)

# Change Organic Carbon Dissolved to Dissolved Organic Carbon
codes$Variable[codes$EMS_Code == "EMS_1126"] <- "Dissolved Organic Carbon"

missing_help <- "There are two reasons why guideline values may be missing:
                1. A condition was not met;
                2. There is no available equation for that variable/use/term combination."

empty_raw <- limits[0, ]
empty_evaluate <- limits %>%
  mutate(ConditionPass = NA, Guideline = NA)
empty_evaluate <- empty_evaluate[0, ]

empty_report <- empty_evaluate[c("Variable", "Use", "Media", "PredictedEffectLevel",
                                 "Type", "Statistic", "Guideline", "Reference",
                                 "Reference Link", "Overview Report Link",
                                 "Technical Document Link")]
empty_report <- empty_report %>% rename(`Effect Level` = PredictedEffectLevel)

usethis::use_data(limits, cu_h20_aq_fresh_acute_lookup, cu_h20_aq_fresh_chronic_lookup,
                  codes, empty_raw, empty_report, empty_evaluate, missing_help, 
                  internal = TRUE, overwrite = TRUE)


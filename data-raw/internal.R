library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(wqbc)

hash_limits <- "85d3990a-ec0a-4436-8ebd-150de3ba0747"
limits <-  bcdata::bcdc_get_data(record = hash_limits)

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

internal_tables <- list(limits)
#### need to fix hash to be correct - missing last 2 digits 
names(internal_tables) <- "85d3990a-ec0a-4436-8ebd-150de3ba07"
lookup_hash <- limits$Limit[!is.na(limits$Limit) & stringr::str_detect(limits$Limit, "[.]csv$")]
#### will need to change to once data has been uploaded to data catalogue 
for (file in lookup_hash) {
  #### uncomment once uploaded to bcdata catalogue 
  # lookup <- bcdata::bcdc_get_data(record = file)
  lookup <- readr::read_csv(file.path("data-raw", file))
  internal_tables[[paste0(file)]] <- lookup
}


codes <-  wqbc::codes
codes <- codes %>% dplyr::rename(EMS_Code = Code)

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

usethis::use_data(limits, internal_tables, codes, empty_raw, empty_report, 
                  empty_evaluate, missing_help, internal = TRUE, overwrite = TRUE)

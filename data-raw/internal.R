library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(wqbc)

hash_limits <- "85d3990a-ec0a-4436-8ebd-150de3ba0747"
limits <-  bcdata::bcdc_get_data(
  record = hash_limits, 
  resource = "6f32a85b-a3d9-44c3-9a14-15175eba25b6"
)

hash_cu_acute <- "23ada5c3-67a6-4703-9369-c8d690b092e1"
hash_cu_chronic <- "a35c7d13-76dd-4c23-aab8-7b32b0310e2f"

#limits <- readr::read_csv("https://raw.githubusercontent.com/bcgov/wqg_data/master/all_wqgs.csv")

## modify limits to be what databc should be
## switch off code to read from databc therefore uses internals.
## once working then update databc with new limits and lookups.
# limits$Units[limits$Variable == "Copper" & limits$Component == "Dissolved"] <- "ug/L"
# limits$Direction[limits$Variable == "Copper" & limits$Component == "Dissolved"] <- "Upper Limit"
# limits$Limit[limits$Variable == "Copper" &
#           limits$Use == "Aquatic Life - Freshwater" &
#           limits$Media == "Water" &
#           limits$Type == "Short-term acute"] <- hash_acute
# limits$Limit[limits$Variable == "Copper" &
#           limits$Use == "Aquatic Life - Freshwater" &
#           limits$Media == "Water" &
#           limits$Type == "Long-term chronic"] <- hash_chronic
# limits$LimitNotes[limits$Variable == "Copper" & limits$Component == "Dissolved"] <- NA

internal_tables <- list(limits)
#### need to fix hash to be correct - missing last 2 digits 
#names(internal_tables) <- "85d3990a-ec0a-4436-8ebd-150de3ba07"
names(internal_tables) <- hash_limits


lookup_hash <- c(hash_cu_chronic, hash_cu_acute)
for (file in lookup_hash) {
  lookup <- bcdata::bcdc_get_data(record = file)
  internal_tables[[paste0(file)]] <- lookup
}

internal_tbl_names <- list(
  hash_limits = "guidelines",
  hash_cu_acute = "acute copper guidelines",
  hash_cu_chronic = "chronic copper guidelines"
)

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

usethis::use_data(
  limits, internal_tables, internal_tbl_names, codes, empty_raw, empty_report, 
  empty_evaluate, missing_help, internal = TRUE, overwrite = TRUE
)

library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(wqbc)

limits <- readr::read_csv("https://raw.githubusercontent.com/bcgov/wqg_data/master/all_wqgs.csv")
# limits <-  bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")

codes <-  wqbc::codes
codes <- codes %>% dplyr::rename(EMS_Code = Code)

### add Calcium Dissolved
codes <- bind_rows(codes,
                   tibble(Variable = "Calcium Dissolved",
                          EMS_Code = "EMS_CA_D",
                          Units = "mg/L"))

extract_codes <- function(x) {
  setdiff(unique(unlist(lapply(x, function(y){
    stringr::str_extract_all(y, "EMS_[[:alnum:]][[:alnum:]_]{3,3}")
  }))), NA)
}

limit_codes <- extract_codes(limits$Limit)
condition_codes <- extract_codes(limits$Condition)
cvalue_codes <- unique(c(limit_codes, condition_codes))

# remove EMS_1107 as Hardness just one
cvalue_codes <- setdiff(cvalue_codes, "EMS_1107")

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

usethis::use_data(limits, codes, cvalue_codes,
                  empty_raw, empty_report, empty_evaluate, 
                  missing_help, internal = TRUE, overwrite = TRUE)


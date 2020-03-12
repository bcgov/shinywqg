library(readr)
library(dplyr)

bcgov_pal <- list(
  blue = "#043363",
  yellow = "#fab933",
  black = "#303032",
  white = "#ffffff"
)

limits <-  bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")
limits <- dplyr::mutate(limits, Condition = dplyr::if_else(Condition == "", NA_character_, Condition))

# codes <- limits %>%
#   dplyr::select(Variable, EMS_Code, Units) %>%
#   dplyr::group_by(Variable, EMS_Code) %>%
#   dplyr::arrange(Variable, EMS_Code, Units) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup()
codes <-  wqbc::codes
codes <- codes %>% dplyr::rename(EMS_Code = Code,
  Statistic = Average)

### add Calcium Dissolved
codes <- bind_rows(codes,
  tibble(Variable = "Calcium Dissolved",
    EMS_Code = "EMS_CA_D",
    Units = "mg/L",
    Statistic = "mean"))

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

### for now, remove rows with notes but put back later
# limits <- limits %>%
#   filter(Notes == "" | is.na(Notes))

duplicates <- limits %>%
  group_by(Variable, EMS_Code, Use, Media, Type, PredictedEffectLevel,
    Condition, ConditionNotes, Direction, Statistic) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(Variable, EMS_Code, Use, Media, Type, PredictedEffectLevel, Condition, ConditionNotes)

# expect_identical(nrow(duplicates), 0L)

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
  empty_raw, empty_report, empty_evaluate, bcgov_pal,
  missing_help, internal = TRUE, overwrite = TRUE)


library(readr)
library(dplyr)

limits <-  readr::read_csv(system.file(package = "shinywqg", "extdata/all_wqgs.csv"))
limits <- dplyr::mutate(limits, Condition = dplyr::if_else(Condition == "", NA_character_, Condition))

# codes <- limits %>% 
#   dplyr::select(Variable, EMS_Code, Units) %>%
#   dplyr::group_by(Variable, EMS_Code) %>%
#   dplyr::arrange(Variable, EMS_Code, Units) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup()
codes <-  read.csv(system.file(package = "shinywqg", "extdata/codes.csv"), stringsAsFactors = FALSE)
codes <- codes %>% dplyr::rename(EMS_Code = Code,
                                 Statistic = Average)

### add Calcium Dissolved
codes <- bind_rows(codes,
                   tibble(Variable = "Calcium Dissolved",
                          EMS_Code = "EMS_CA_D",
                          Units = "mg/L",
                          Statistic = "mean"))

extract_codes1 <- function(x){
  if(is.na(x)){
    return(NULL)
  }
  reg <- gregexpr("EMS_", x)[[1]]
  if(length(reg) < 2){
    if(reg < 0){
      return(NULL)
    }
  }
  sapply(reg, function(y){
    substr(x, y, y+7)
  }) %>% unique()
}

extract_codes2 <- function(x){
  unique(unlist(lapply(x, extract_codes1)))
}

limit_codes <- extract_codes2(limits$Limit)
condition_codes <- extract_codes2(limits$Condition)
cvalue_codes <- unique(c(limit_codes, condition_codes))

### for now, remove rows with notes but put back later
limits <- limits %>%
  filter(Notes == "" | is.na(Notes)) 

# remove 4 duplicates until issues resolved
limits <- limits %>%
  group_by(Variable, EMS_Code, Use, Media, Type, PredictedEffectLevel, 
           Condition, ConditionNotes, Direction, Statistic) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(Variable, EMS_Code, Use, Media, Type, PredictedEffectLevel, Condition, ConditionNotes) 

duplicates <- limits %>%
  group_by(Variable, EMS_Code, Use, Media, Type, PredictedEffectLevel, 
           Condition, ConditionNotes, Direction, Statistic) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(Variable, EMS_Code, Use, Media, Type, PredictedEffectLevel, Condition, ConditionNotes) 

expect_identical(nrow(duplicates), 0L)

missing_help <- "There are two reasons why guideline values may be missing:
                1. A condition was not met;
                2. There is no available equation for that variable/use/term combination."

usethis::use_data(limits, codes, cvalue_codes, missing_help, internal = TRUE, overwrite = TRUE)
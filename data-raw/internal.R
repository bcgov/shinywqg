library(readr)
library(dplyr)
library(stringr)

## Run to see if new lookup tables been added 
reg_pat <- "^[:alnum:]{8}-[:alnum:]{4}-[:alnum:]{4}-[:alnum:]{4}-[:alnum:]{12}$"
limits[!is.na(limits$Limit) & stringr::str_detect(limits$Limit, reg_pat),] |>
  select(Variable, Type, Limit)

## Define tables to be downloaded for internal data storage
## When new tables added to limits they need to be added here
hash_limits <- "85d3990a-ec0a-4436-8ebd-150de3ba0747"

hash_cu_acute <- "23ada5c3-67a6-4703-9369-c8d690b092e1"
hash_cu_chronic <- "a35c7d13-76dd-4c23-aab8-7b32b0310e2f"

hash_ni_acute <- "065581bf-fd52-4aad-aa82-fc510c074cab"
hash_ni_chronic <- "d25b348f-a8da-41fc-8141-bd29df155e9c"

hash_nh3_acute <- "6b1cc604-18d1-426c-9d9a-c31b5ba15a16"
hash_nh3_chronic <- "2396b78b-8782-444d-bade-4487b75b789c"

## Create a list of names of tables, used to let users know which table was pulled from Internal
internal_tbl_names <- list()

internal_tbl_names[[hash_limits]] <- "guidelines"
internal_tbl_names[[hash_cu_acute]] <- "acute copper guidelines"
internal_tbl_names[[hash_cu_chronic]] <- "chronic copper guidelines"
internal_tbl_names[[hash_ni_acute]] <- "acute nickel guidelines"
internal_tbl_names[[hash_ni_chronic]] <- "chronic nickel guidelines"
internal_tbl_names[[hash_nh3_acute]] <- "acute ammonia guidelines"
internal_tbl_names[[hash_nh3_chronic]] <- "chronic ammonia guidelines"

internal_tbl_names

limits <-  bcdata::bcdc_get_data(
  record = hash_limits,
  resource = "6f32a85b-a3d9-44c3-9a14-15175eba25b6"
)

codes <- read_csv("data-raw/codes.csv")


## Create list of lookup table data
internal_tables <- list(limits)
names(internal_tables) <- hash_limits

lookup_hash <- c(hash_cu_chronic, hash_cu_acute)

for (file in lookup_hash) {
  lookup <- bcdata::bcdc_get_data(record = file)
  internal_tables[[paste0(file)]] <- lookup
}

missing_help <- "There are two reasons why guideline values may be missing:
                1. A condition was not met;
                2. There is no available equation for that variable/use/term combination."

empty_raw <- limits[0, ]
empty_evaluate <- limits |>
  mutate(ConditionPass = NA, Guideline = NA)
empty_evaluate <- empty_evaluate[0, ]

empty_report <- empty_evaluate[c("Variable", "Use", "Media", "PredictedEffectLevel",
                                 "Type", "Statistic", "Guideline", "Reference",
                                 "Reference Link", "Overview Report Link",
                                 "Technical Document Link")]
empty_report <- empty_report |> rename(`Effect Level` = PredictedEffectLevel)

usethis::use_data(
  limits, internal_tables, internal_tbl_names, codes, empty_raw, empty_report, 
  empty_evaluate, missing_help, internal = TRUE, overwrite = TRUE
)

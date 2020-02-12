limits <-  read.csv(system.file(package = "shinywqg", "extdata/all_wqgs.csv"), stringsAsFactors = FALSE)
limits <- dplyr::mutate(limits, Condition = dplyr::if_else(Condition == "", NA_character_, Condition))

codes <- limits %>% dplyr::select(Variable, EMS_Code, Units) %>% dplyr::distinct()
# codes <-  read.csv(system.file(package = "shinywqg", "extdata/codes.csv"), stringsAsFactors = FALSE)

missing_help <- "There are two reasons why guideline values may be missing:
                1. A condition was not met;
                2. There is no available equation for that variable/use/term combination."

usethis::use_data(limits, codes, missing_help, internal = TRUE, overwrite = TRUE)
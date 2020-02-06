limits <-  read.csv(system.file(package = "shinywqg", "extdata/limits.csv"), stringsAsFactors = FALSE)
limits <- dplyr::mutate(limits, Condition = dplyr::if_else(Condition == "", NA_character_, Condition))
limits2 <- limits[1:20,] 
limits2$Use <- "Salty Times"
limits <- rbind(limits, limits2)
codes <-  read.csv(system.file(package = "shinywqg", "extdata/codes.csv"), stringsAsFactors = FALSE)

missing_help <- "There are two reasons why guideline values may be missing:
                1. A condition was not met;
                2. There is no available equation for that variable/use/term combination."

usethis::use_data(limits, limits2, codes, missing_help, internal = TRUE, overwrite = TRUE)
limits <- read.csv("data-raw/limits.csv", stringsAsFactors = FALSE)
codes <- read.csv("data-raw/codes.csv", stringsAsFactors = FALSE)

get_variable <- function(code){
  code <- unique(code)
  sapply(code, function(x){
    codes$Variable[codes$Code == x]
  }, simplify = TRUE, USE.NAMES = FALSE) 
}

extract_codes <- function(variable, guideline){
  x <- get_data(variable, guideline)$UpperLimit
  # not inlcuding EMS_HG_T because not an arg in lookup_lmits
  codes <- c("EMS_0004|EMS_0107|EMS_HGME|EMS_0104")
  sapply(x, function(y){
    str_extract_all(y, codes)[[1]] 
  }, simplify = TRUE, USE.NAMES = FALSE) %>%
    unlist() %>%
    unique() 
}

get_data <- function(variable, guideline){
  limits[limits$Variable %in% variable & limits$Use %in% guideline,]
}

get_guidelines <- function(variable){
  unique(limits$Use[limits$Variable %in% variable])
}

# x <- limits$UpperLimit[3]
# y <- extract_codes(x)
# get_variable(y)

# str_contains <- function(x, code = "EMS_0004"){
#   grepl(code, x)
# }
# 
# contains_ph <- function(x){
#   str_contains(x, code = "EMS_0004")
# }
# 
# contains_hardness <- function(x){
#   str_contains(x, code = "EMS_0107")
# }
# 
# contains_mercury_total <- function(x){
#   str_contains(x, code = "EMS_HG_T")
# }
# 
# contains_mercury_methyl <- function(x){
#   str_contains(x, code = "EMS_HGME")
# }
# 
# clean_limits <- function(limits){
#   limits$Ph <- contains_ph(limits$UpperLimit)
#   limits$Hardness <- contains_hardness(limits$UpperLimit)
#   limits$MercuryTotal <- contains_mercury_total(limits$UpperLimit)
#   limits$MercuryMethyl <- contains_mercury_methyl(limits$UpperLimit)
#   limits$ConditionPh <- contains_ph(limits$Condition)
#   limits$ConditionHardness <- contains_hardness(limits$Condition)
#   limits$ConditionMercuryTotal <- contains_mercury_total(limits$Condition)
#   limits$ConditionMercuryMethyl <- contains_mercury_methyl(limits$Condition)
#   limits
# }
# 
# limits <- clean_limits(limits)
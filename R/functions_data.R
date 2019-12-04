limits <- read.csv("data-raw/limits.csv", stringsAsFactors = FALSE)
limits <- dplyr::mutate(limits, Condition = if_else(Condition == "", NA_character_, Condition))
limits2 <- limits[1:20,] 
limits2$Use <- "Salty Times"
limits <- rbind(limits, limits2)
codes <- read.csv("data-raw/codes.csv", stringsAsFactors = FALSE)

missing_help <- "There are two reasons why guideline values may be missing:
                1. A condition was not met;
                2. There is no available equation for that variable/use/term combination."
# eval_limit <- function(equation, cvalues){
#   x <- try(eval(parse(text = as.character(x)), envir = cvalues), 
#       silent = TRUE)
#   x
# }
# parse(text = as.character(x))
# x <- limits[3,]$UpperLimit
# cvalue <- as.list(3)
# names(cvalue) <- "EMS_0004"
# 
# eval_limit(limits$UpperLimit[3], cvalue)

get_variable <- function(code){
  code <- unique(code)
  sapply(code, function(x){
    codes$Variable[codes$Code == x]
  }, simplify = TRUE, USE.NAMES = FALSE) 
}

extract_codes <- function(variable, guideline){
  x <- get_data(variable, guideline)$UpperLimit
  codes <- c("EMS_0004|EMS_0107|EMS_HGME|EMS_HG_T|EMS_0104")
  unique(unlist(sapply(x, function(y){
    str_extract_all(y, codes)[[1]] 
  }, simplify = TRUE, USE.NAMES = FALSE)))
}

get_data <- function(variable, guideline){
  limits[limits$Variable %in% variable & limits$Use %in% guideline,]
}

get_guidelines <- function(variable){
  unique(limits[["Use"]][limits[["Variable"]] %in% variable])
}

add_missing <- function(x, variable, term, guideline){
  limits <- limits[limits$Use %in% guideline,]
  all <- do.call("rbind", lapply(term, function(x){
    y <- limits[limits$Variable %in% variable, c("Variable", "Units")]
    y$Term = x
    y
  }))
  missing <- dplyr::anti_join(all, x, c("Variable", "Term")) %>%
    dplyr::distinct()
  dplyr::bind_rows(x, missing) 
}

filter_missing <- function(df, rm_missing, variable, term, guideline){
  
  condition_missing <- df[is.na(df$UpperLimit),]
  x <- sort(rm_missing)
  if(is.null(x)){
    x <- "foo"
  }
  
  if(!("equation" %in% x)){
    df <- df %>% add_missing(variable, term, guideline)
    if(x == "condition"){
      df <- dplyr::anti_join(df, condition_missing, by = names(condition_missing))
    }
  }
  
  if(identical(x, c("condition", "equation"))){
    df <- df[!is.na(df$UpperLimit),]
  }
  df
}

filter_limits <- function(variable, use, term){
  limits %>% dplyr::filter(Variable %in% variable, 
                           Use %in% use,
                           tolower(Term) %in% tolower(term))
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

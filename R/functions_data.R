limits <- read.csv("data-raw/limits.csv", stringsAsFactors = FALSE)
limits <- dplyr::mutate(limits, Condition = if_else(Condition == "", NA_character_, Condition))
limits2 <- limits[1:20,] 
limits2$Use <- "Salty Times"
limits <- rbind(limits, limits2)
codes <- read.csv("data-raw/codes.csv", stringsAsFactors = FALSE)

missing_help <- "There are two reasons why guideline values may be missing:
                1. A condition was not met;
                2. There is no available equation for that variable/use/term combination."

get_variable <- function(code){
  code <- unique(code)
  sapply(code, function(x){
    codes$Variable[codes$Code == x]
  }, simplify = TRUE, USE.NAMES = FALSE) 
}

extract_codes <- function(variable, use){
  x <- get_data(variable, use)$UpperLimit
  codes <- c("EMS_0004|EMS_0107|EMS_HGME|EMS_HG_T|EMS_0104")
  unique(unlist(sapply(x, function(y){
    str_extract_all(y, codes)[[1]] 
  }, simplify = TRUE, USE.NAMES = FALSE)))
}

get_data <- function(variable, use){
  limits[limits$Variable %in% variable & limits$Use %in% use,]
}

get_use <- function(variable){
  unique(limits[["Use"]][limits[["Variable"]] %in% variable])
}

add_missing <- function(x, variable, term, use){
  term <- tools::toTitleCase(term)
  limits <- limits[limits$Use %in% use,]
  all <- do.call("rbind", lapply(term, function(x){
    y <- limits[limits$Variable %in% variable, c("Variable", "Use")]
    y$Term = x
    y
  }))
  missing <- dplyr::anti_join(all, x, c("Variable", "Term")) %>%
    dplyr::distinct()
  dplyr::bind_rows(x, missing) 
}

filter_missing <- function(df, rm_missing, variable, term, use){
  
  condition_missing <- df[is.na(df$Guideline),]
  x <- sort(rm_missing)
  if(is.null(x)){
    x <- "foo"
  }
  
  if(!("equation" %in% x)){
    df <- df %>% add_missing(variable, term, use)
    if(x == "condition"){
      df <- dplyr::anti_join(df, condition_missing, by = names(condition_missing))
    }
  }
  
  if(identical(x, c("condition", "equation"))){
    df <- df[!is.na(df$Guideline),]
  }
  df
}

filter_limits <- function(variable, use, term){
  limits %>% dplyr::filter(Variable %in% variable, 
                           Use %in% use,
                           tolower(Term) %in% tolower(term))
}

get_refs <- function(x){
  l <- as.list(setdiff(unique(x$Reference), NA_character_))
  names(l) <- 1:length(l)
  l
}

clean_cvalues <- function(x){
  x[sapply(x, is.null)] <- NULL
  names(x) <- replace_codes(names(x))
  x
}

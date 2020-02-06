limits <- read.csv("data-raw/limits.csv", stringsAsFactors = FALSE)
limits <- dplyr::mutate(limits, Condition = dplyr::if_else(Condition == "", NA_character_, Condition))
limits2 <- limits[1:20,] 
limits2$Use <- "Salty Times"
limits <- rbind(limits, limits2)
codes <- read.csv("data-raw/codes.csv", stringsAsFactors = FALSE)

missing_help <- "There are two reasons why guideline values may be missing:
                1. A condition was not met;
                2. There is no available equation for that variable/use/term combination."

get_variable <- function(code, units = TRUE){
  code <- unique(code)
  sapply(code, function(x){
    y <- codes$Variable[codes$Code == x]
    if(!units)
      return(y)
    unit <- gsub(" ", "", paste("(", codes$Units[codes$Code == x], ")"))
    if(unit == "(pH)")
      unit <- ""
    paste(y, unit)
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
  setdiff(unique(x$Reference), NA_character_)
}

clean_cvalues <- function(x, variable, use){
  codes <- extract_codes(variable, use)
  x <- x[codes]
  names(x) <- get_variable(names(x), units = FALSE)
  for(i in names(x)){
    if(grepl("Chloride|Hardness", i))
      x[[i]] <- paste(x[[i]], "mg/L")
    if(grepl("Mercury",  i))
      x[[i]] <- paste(x[[i]], "ug/L")
  }
  x
}

clean_table <- function(x){
  x %>%
  dplyr::mutate(Term = paste(Term, "Term"),
                Guideline = dplyr::if_else(is.na(Guideline), NA_character_, 
                                    paste(signif(Guideline, digits = 2), Units)),
                Equation = dplyr::if_else(!is.na(Equation), Equation, NA_character_)) %>%
    dplyr::select(Variable, Term, Guideline, Equation, Reference) %>%
    dplyr::arrange(Variable, Term)
}

as_math <- function(x){
  paste0("$", x, "$")
}

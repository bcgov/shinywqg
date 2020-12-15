test_condition <- function(x, cvalues) {
  if(is.na(x))
    return (TRUE)
  # pass condition for look-up values
  if(!stringr::str_detect(x, "[>|<|=]"))
    return(TRUE)
  x <- try(eval(parse(text = x), envir = cvalues), silent = TRUE)
  if(class(x) != "logical")
    return (FALSE)
  if(is.na(x))
    return (FALSE)
  x
}

calc_limit <- function(x, cvalues) {
  x <- try(eval(parse(text = as.character(x)), envir = cvalues), silent = TRUE)
  if(class(x) != "numeric")
    return (NA)
  x
}

lookups <- function(lookup_table, cvalues){
  # read's in look-up table 
  # this will need to be adjusted once we get them uploaded to BC Data Cat
  file_path <- paste0("../../data-raw/", lookup_table)
  lookup_table <- readr::read_csv(file_path)
  
  limit_row <- which(rowSums(lookup_table[names(cvalues)] == cvalues) == length(cvalues))
  lookup_table$Limit[[limit_row]]
}

evaluate_guideline <- function(limit, cvalues) {
  ### deals with one limit at a time
  if(!length(limit)) 
    return()
  # if csv present in limit do lookup otherwise calc as per normal
  if(!is.na(limit)){
    if(stringr::str_detect(limit, "[.]csv$")){
      if (!length(cvalues)) {
        return(NA)
      }
    return(lookups(limit, cvalues))
   }
  }
  calc_limit(limit, cvalues)
}

format_guideline <- function(guideline, direction, units, limitnote, sigfig){
  
  if(is.na(guideline) & is.na(limitnote))
    return(NA)
  
  if(is.na(guideline) & !is.na(limitnote))
    return(limitnote)
  
  if(is.na(limitnote))
    limitnote <- ""
  
  prefix <- switch(direction,
                   "Upper Limit" = "<= ",
                   "Lower Limit" = ">= ",
                   "")
  paste0(prefix, signif(guideline, sigfig), " (", units, ") ", limitnote)
}

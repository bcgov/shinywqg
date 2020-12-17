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
  limit_row <- which(rowSums(lookup_table[names(cvalues)] == cvalues) == length(cvalues))
  guideline <- try(lookup_table$Limit[[limit_row]], silent = TRUE)
  if(is_try_error(guideline)){
    guideline <- NA
  }
  guideline
}

evaluate_guideline <- function(limit, lookup, cvalues) {
  ### deals with one limit at a time
  if(!length(limit)) 
    return()
  # if lookup table present do lookup otherwise calc as per normal
  if(!is.null(lookup[[1]])){
      if (!length(cvalues)) {
        return(NA)
      }
    return(lookups(lookup[[1]], cvalues))
  }
  calc_limit(limit, cvalues)
}

format_guideline <- function(guideline, direction, units, limitnote, lookupnotes, sigfig){
  
  if(is.na(guideline) & !is.na(lookupnotes))
    return(lookupnotes)
  
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

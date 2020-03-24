test_condition <- function(x, cvalues) {
  if(is.na(x))
    return (TRUE)
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

evaluate_guideline <- function(limit, cvalues) {
  ### deals with one limit at a time
  if(!length(limit)) 
    return()
  
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

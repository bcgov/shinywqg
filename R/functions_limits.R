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

evaluate_guideline <- function(limit, note, direction, units, cvalues, sigfig) {
  if(!length(limit) | !length(note)) 
    return()
  
  prefix <- switch(direction,
                   "Upper Limit" = "<= ",
                   "Lower Limit" = ">= ",
                   "")
  
  if(!is.na(note) & is.na(limit))
    return(paste0(prefix, note))
  
  limit <- signif(calc_limit(limit, cvalues), sigfig)
  paste0(prefix, limit, " (", units, ")")
}

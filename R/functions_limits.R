code_values <- function(ph, hardness, chloride, methyl_mercury){
  l <- list(ph, hardness, chloride, methyl_mercury, mercury_total)
  names(l) <- c("EMS_0004", "EMS_0107", "EMS_HGME", "EMS_0104")
  l
}

lookup_equation <- function(x, value, code_values){
  if(all(is.na(x)))
    return(value)
  con <- sapply(x, test_condition, code_values)
  if(!length(unlist(con)))
    return(NA)
  if(all(!con))
    return(NA)
  value[con] 
}

pretty_equation <- function(x){
  x <- gsub("EMS_0004", "pH", x)
  x <- gsub("EMS_0107", "Hardness", x)
  x <- gsub("EMS_0104", "Chloride", x)
  x <- gsub("EMS_HGME", "Mercury Methyl", x)
  x
}

test_condition <- function (x, cvalues) {
  if(is.na(x))
    return (TRUE)
  x <- try(eval(parse(text = x), envir = cvalues), silent = TRUE)
  if(class(x) != "logical")
    return (FALSE)
  return (x)
}

calc_limit <- function (x, cvalues) {
  x <- try(eval(parse(text = as.character(x)), envir = cvalues), silent = TRUE)
  if(class(x) != "numeric")
    return (NA)
  return (x)
}

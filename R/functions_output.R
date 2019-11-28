wqg_table <- function(variable, guideline, term,
                      ph, hardness, methyl_mercury, chloride){
  x <- try(do.call("rbind", lapply(term, function(x){
    y <- lookup_limits2(
      ph = ph, 
      hardness = hardness, 
      methyl_mercury = methyl_mercury, 
      chloride = chloride,
      variable = variable,
      term = x)
    y$Term <- x
    dplyr::mutate_if(y, is.factor, as.character)
  })), silent = TRUE)
  if(is_try_error(x))
    return()
  x <- add_missing(x, variable, term)
  x
}


wqg_report <- function(x){
  
}
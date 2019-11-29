wqg_table <- function(variable, guideline, term,
                      ph, hardness, 
                      methyl_mercury, chloride){
  do.call("rbind", lapply(term, function(x){
    y <- lookup_limits2(
      ph = ph, 
      hardness = hardness, 
      methyl_mercury = methyl_mercury, 
      chloride = chloride,
      variable = variable,
      term = x)
    if(nrow(y) > 0){
      y$Term <- x
    } else {
      y <- cbind(y, data.frame(Term = character(), stringsAsFactors = FALSE))
    }
      
    dplyr::mutate_if(y, is.factor, as.character)
  }))
}
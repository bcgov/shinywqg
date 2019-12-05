replace_refs <- function(x){
  l <- get_refs(x)
  for(i in names(l)){
    x$Reference[x$Reference == l[i]] <- i
  }
  x
}

gt_table <- function(x){
  x %>%
    mutate(Term = paste(Term, "Term"),
           Guideline = if_else(is.na(Guideline), NA_character_, 
                               paste(signif(Guideline, digits = 2), Units)),
           Equation = if_else(is.na(as.numeric(Equation)), Equation, NA_character_)) %>%
    select(Variable, Term, Guideline, Equation, Reference) %>%
    gt::gt(rowname_col = "Term", groupname_col = "Variable") %>%
    gt::fmt_missing(columns = gt::everything())
}

wqg_table <- function(variable, use, term, cvalues){
  
  x <- filter_limits(variable, use, term) %>%
    dplyr::group_by(Variable, Use, Term) %>%
    dplyr::mutate(Equation = lookup_equation(Condition, UpperLimit, cvalues)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() 
  
  x$Guideline <- sapply(x$Equation, calc_limit, cvalues)

  x %>% 
    dplyr::mutate(Equation = replace_codes(Equation)) %>%
    dplyr::select(Variable, Use, Term, Guideline,
                  Units, Reference, Equation)
    
}

# variable <- c("Aluminium Dissolved", "Arsenic Total")
# use = "Freshwater Life"
# term = c("short", "long")
# ph = 3
# hardness = NULL
# chloride = NULL
# mercury_methyl = NULL

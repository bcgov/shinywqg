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

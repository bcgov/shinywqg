wqg_table <- function(variable, use, term,
                      ph, hardness, 
                      methyl_mercury, chloride){
  
  cvalues <- code_values(ph = ph,
                             hardness = hardness,
                             chloride = chloride,
                             methyl_mercury = methyl_mercury)
  
  x <- filter_limits(variable, use, term) %>%
    dplyr::group_by(Variable, Use, Term) %>%
    dplyr::mutate(Equation = lookup_equation(Condition, UpperLimit, cvalues)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() 
  
  x$Guideline <- sapply(x$Equation, calc_limit, cvalues)

  x %>% 
    dplyr::mutate(Equation = pretty_equation(Equation)) %>%
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

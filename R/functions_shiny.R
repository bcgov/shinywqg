numeric_inputs <- function(x, ns) {
  x <- lapply(x, function(y) {
    value <- NULL
    numericInput(inputId = ns(y), label = code_to_variable(y), value = value)
  })
  x
}

waiter_html <- function(x){
  tagList(waiter::spin_chasing_dots(),
          br2(),
          h3(x))
}

dropdown_inputs <- function(codes, ns, data){
  print(data)
  x <- lapply(codes, function(y){
    if(nrow(data) != 0){
      print("actual choices")
    selectInput(inputId = ns(y),
               label = code_to_variable(y),
               choices = unique(data[y])) 
    } else{
      print("null output")
      selectInput(inputId = ns(y),
                  label = code_to_variable(y),
                  choices = NULL) 
    }
  })
  x
}




# library(tidyverse)
# 
# x <- limits %>% 
#   filter(Variable == "Copper") %>% 
#   filter(Media == "Water")
# 
# x <- process_lookups(x)
# 
# # create df, of all options 
# z <- tibble::tibble()
# for (i in x$lookup){
#   z <- bind_rows(z, i)
# }
# 
# z %<>% select(-Limit) %>% 
#   distinct()
# 
# inactive_codes <- c("EMS_CA_D", "EMS_0220")
# 
# for (i in inactive_codes){
#   print(i)
#   z[i] <- NA
# }

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
  print(data$lookup)
  
  x <- lapply(codes, function(y){
   selectInput(inputId = ns(y),
               label = code_to_variable(y),
               choices = NULL) 
  })
  x
}




# dropdown_inputs_1 <- function(ns) {
#   
#   a <- selectInput(inputId = ns("EMS_0004"),
#               label = code_to_variable("EMS_0004"),
#               choices = sort(unique(cu_h20_aq_fresh_acute_lookup$EMS_0004))
#               )
# 
#   
#   b <- selectInput(inputId = ns("EMS_1126"),
#               label = code_to_variable("EMS_1126"),
#               choices = sort(unique(cu_h20_aq_fresh_acute_lookup$EMS_1126))
#               )
# 
#   d <- selectInput(inputId = ns("EMS_0107"),
#               label = code_to_variable("EMS_0107"),
#               choices = sort(unique(cu_h20_aq_fresh_acute_lookup$EMS_0107))
#               )
#   
#   tag_names <- list(a, b, d)
#   return(tag_names)
#   
# }

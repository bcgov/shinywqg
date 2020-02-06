numeric_inputs <- function(x, ns){
  lapply(x, function(y){
    value <- 10
    if(y == "EMS_0004"){
      value <- 7
    }
    numericInput(inputId = ns(y), label = get_variable(y), value = value)
  })
}
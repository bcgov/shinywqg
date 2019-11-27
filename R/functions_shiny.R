numeric_inputs <- function(x, ns){
  lapply(x, function(y){
    numericInput(inputId = ns(y), label = get_variable(y), value = 0)
  })
}
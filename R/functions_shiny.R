numeric_inputs <- function(x, ns) {
  x <- lapply(x, function(y) {
    value <- 10
    if(y == "EMS_0004") {
      value <- 7
    }
    numericInput(inputId = ns(y), label = code_to_variable(y), value = value)
  })
  x
}

waiter_html <- function(x){
  tagList(waiter::spin_chasing_dots(),
          br2(),
          h3(x))
}

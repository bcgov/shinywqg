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

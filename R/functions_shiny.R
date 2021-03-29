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
  x <- lapply(codes, function(y){
    choices <- try(sort(unique(data[[y]])))
    if(is_try_error(choices)){
      choices <- NULL
    }
    selectInput(inputId = ns(y),
               label = code_to_variable(y),
               choices = choices)
  })
  x
}

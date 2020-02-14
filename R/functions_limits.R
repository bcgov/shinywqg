
test_condition <- function (x, cvalues) {
  if(is.na(x))
    return (TRUE)
  x <- try(eval(parse(text = x), envir = cvalues), silent = TRUE)
  if(class(x) != "logical")
    return (FALSE)
  x
}

calc_limit <- function (x, cvalues) {
  x <- try(eval(parse(text = as.character(x)), envir = cvalues), silent = TRUE)
  if(class(x) != "numeric")
    return (NA)
  x
}

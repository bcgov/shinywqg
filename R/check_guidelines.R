#' Check guidelines
#'
#' Check that updated DataBC guidelines are valid
#' 
#' @param x A data.frame of the guidelines.
#' @return A invisible copy of the data.frame of the guidelines.
#' @export
check_guidelines <- function(x){
  
  chk::check_names(x, names(limits))
  
  limit_codes <- extract_codes(x$Limit)
  condition_codes <- extract_codes(x$Condition)
  cvalue_codes <- unique(c(limit_codes, condition_codes))
  cvalues <- vector(mode = "list", length = length(cvalue_codes))
  names(cvalues) <- cvalue_codes
  cvalues <- lapply(cvalues, function(x) x <- 1)
  

  check_valid_expression <- function(x, cvalues) {
    for(i in 1:length(x)){
      limit <- x[i]
      if(is.na(limit))
        next
      y <- try(eval(parse(text = limit), envir = cvalues), silent = TRUE)
      if(is_try_error(y))
        stop("Expression ", i, " is not valid")
    }}

  check_valid_expression(x$Condition, cvalues)
  check_valid_expression(x$Limit, cvalues)
  
  x
}
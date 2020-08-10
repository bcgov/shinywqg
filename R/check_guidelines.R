#' Check guidelines
#'
#' Check that updated DataBC guidelines are valid
#' 
#' @param x A data.frame of the guidelines. IF NULL, pull from BC Data Catalogue.
#' @return A invisible copy of the data.frame of the guidelines.
#' @export
check_guidelines <- function(x = NULL){
  
  if(is.null(x)){
    message("reading data from BC Data Catalogue ...")
    x <-  bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")
    x <- process_limits(x)
  }
  
  chk::check_names(x, names(limits))
  
  chk_not_any_na(x$Variable)
  chk_not_any_na(x$Use)
  chk_not_any_na(x$Media)
  
  limit_codes <- extract_codes(x$Limit)
  condition_codes <- extract_codes(x$Condition)
  cvalue_codes <- unique(c(limit_codes, condition_codes))
  cvalues <- vector(mode = "list", length = length(cvalue_codes))
  names(cvalues) <- cvalue_codes
  cvalues <- lapply(cvalues, function(x) x <- 1)
  
  check_valid_expression <- function(x, cvalues, colname) {
    for(i in 1:length(x)){
      limit <- x[i]
      if(is.na(limit))
        next
      y <- try(eval(parse(text = limit), envir = cvalues), silent = TRUE)
      if(is_try_error(y))
        stop(paste0("The value in row ", i, " of column '", colname, 
                   "' is not a valid R expression. Please fix."))
    }}

  check_valid_expression(x$Condition, cvalues, "Condition")
  check_valid_expression(x$Limit, cvalues, "Limit")
  
  x
}
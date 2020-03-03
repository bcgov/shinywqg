wqg_filter <- function(variable, use, media, type, effect, statistic, x = limits) {
  x <- x %>%
    dplyr::filter(Variable == variable,
      Use %in% use,
      Media %in% media,
      Type %in% type,
      PredictedEffectLevel %in% effect,
      Statistic %in% statistic)
  # remove duplicates caused by multiple EMS_Codes
  if(all(is.na(x$EMS_Code))) {
    return(x)
  }
  ems <- sort(unique(x$EMS_Code))
  x %>%
    dplyr::filter(EMS_Code == ems[1])
}

wqg_evaluate <- function(x, cvalues) {
  x$ConditionPass <- sapply(x$Condition, test_condition, cvalues, USE.NAMES = FALSE)
  ### assumes that never a LimitNote AND Limit
  x$Guideline <- sapply(1:nrow(x), function(y) {
    evaluate_guideline(x$Limit[y],
      x$LimitNotes[y],
      cvalues)
  })
  x
}

wqg_clean <- function(data, sigfig) {
  data <- data %>%
    dplyr::filter(ConditionPass)
  
  data$Guideline <- sapply(1:nrow(data), function(x) {
    format_guideline(data$Guideline[x],
                     data$Direction[x],
                     data$Units[x],
                     sigfig)
  })
  
  data %>%
    dplyr::select(Variable, Use, Media, Type, Statistic,
      `Predicted Effect Level` = PredictedEffectLevel,
      Status, `Condition Notes` = ConditionNotes, 
      Guideline, Reference:`Technical Document Link`) %>%
    dplyr::select_if(function(x) !all(is.na(x)))
}

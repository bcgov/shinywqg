process_limits <- function(x){
  x <- x %>%
    dplyr::mutate(Condition = dplyr::if_else(Condition == "", NA_character_, Condition))
  
  ### remove Hardness Dissolved in OR context
  modified <- x$Condition[which(stringr::str_detect(x$Condition, "EMS_0107"))] %>%
    stringr::str_split_fixed("\\|", 2)
  modified <- modified[, 1]
  x$Condition[which(stringr::str_detect(x$Condition, "EMS_0107"))] <- modified
  x
}

wqg_filter <- function(variable, component, use, x = limits) {
  x <- x %>%
    dplyr::filter(Variable == variable,
                  Component == component,
      Use %in% use)
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
      # x$NarrativeWQG[y],
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
    dplyr::select(Variable, Component, Value = Use, Media, Type, 
      `Predicted Effect Level` = PredictedEffectLevel,
      Status, `Condition Notes` = ConditionNotes,
      `WQG Narrative` = NarrativeWQG, `Method Notes` = MethodNotes, 
      `Limit Notes` = LimitNotes,
      Guideline, Reference:`Technical Document Link`)
}

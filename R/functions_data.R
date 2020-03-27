utils::globalVariables(c("."))

process_limits <- function(limits){
  limits <- limits %>%
    dplyr::mutate(Condition = dplyr::if_else(.data$Condition == "", NA_character_, .data$Condition))
  
  ### remove Hardness Dissolved in OR context
  modified <- limits$Condition[which(stringr::str_detect(limits$Condition, "EMS_0107"))] %>%
    stringr::str_split_fixed("\\|", 2)
  modified <- modified[, 1]
  limits$Condition[which(stringr::str_detect(limits$Condition, "EMS_0107"))] <- modified
  
  #### start convert background percent to limit notes #####
  limits <- dplyr::mutate(limits, PC = !is.na(.data$Limit) & stringr::str_detect(.data$Limit, paste0(
    "^(\\s*", .data$EMS_Code, "\\s*[*]\\s*\\d[.]\\d+\\s*$)|",
    "(^\\s*\\d[.]\\d+\\s*[*]\\s*", .data$EMS_Code, "\\s*$)")))
  
  limits$Limit[limits$PC] %<>% stringr::str_replace("EMS_[[:alnum:]_]{4,4}", "100")
  
  limits$Limit[limits$PC] %<>% sapply(function(x) eval(parse(text=x)))
  
  limit_notes <- stringr::str_c(limits$Limit[limits$PC], "% background")
  limits$Limit[limits$PC] <- NA_character_
  
  limit_notes %<>% stringr::str_c(dplyr::if_else(is.na(limits$LimitNotes[limits$PC]), ".",
                                 stringr::str_c(" (", limits$LimitNotes[limits$PC], ").")))
  
  limits$LimitNotes[limits$PC] <- limit_notes
  limits <- dplyr::select(limits, -.data$PC)
  
  #### end convert background percent
  
  ### filter if all missing values
  limits <- limits %>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.)))
  
  limits
}

wqg_filter <- function(variable, use, media, x = limits) {
  x <- x %>%
    dplyr::filter(.data$Variable == variable, .data$Use %in% use, .data$Media %in% media)
  # remove duplicates caused by multiple EMS_Codes
  if(all(is.na(x$EMS_Code))) {
    return(x)
  }
  
  x %>% 
    dplyr::group_by(.data$Variable, .data$Component) %>%
    dplyr::arrange(.data$EMS_Code) %>%
    dplyr::filter(.data$EMS_Code == dplyr::first(.data$EMS_Code))
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
    dplyr::filter(.data$ConditionPass)
  
  data$Guideline <- sapply(1:nrow(data), function(x) {
    format_guideline(data$Guideline[x],
                     data$Direction[x],
                     data$Units[x],
                     data$LimitNotes[x],
                     sigfig)
  })
  
  data %>%
    dplyr::mutate(Notes = gsub("NA", "", paste(.data$ConditionNotes, .data$MethodNotes))) %>%
    dplyr::mutate(Notes = dplyr::if_else(.data$Notes == " ", NA_character_, .data$Notes)) %>%
    dplyr::select(.data$Variable, .data$Component, Value = .data$Use, 
                  .data$Media, .data$Type, `Predicted Effect Level` = .data$PredictedEffectLevel,
      .data$Status, `WQG Narrative` = .data$NarrativeWQG, .data$Notes,
      .data$Guideline, .data$Reference, .data$`Reference Link`, 
      .data$`Overview Report Link`, .data$`Technical Document Link`)
}

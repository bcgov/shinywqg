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
    dplyr::filter(.data$EMS_Code == dplyr::first(.data$EMS_Code) | is.na(.data$EMS_Code))
}

wqg_evaluate <- function(x, cvalues) {
  x$ConditionPass <- vapply(x$Condition, test_condition, cvalues, FUN.VALUE = logical(1) , USE.NAMES = FALSE)
  ### assumes that never a LimitNote AND Limit
  x$Guideline <- unlist(lapply(1:nrow(x), function(y) {
    evaluate_guideline(x$Limit[y], x$lookup[y],
      cvalues)
  }))
  x
}

wqg_clean <- function(data, sigfig) {
  
  data <- data %>%
    dplyr::filter(.data$ConditionPass)
  
  if (nrow(data) == 0) return()
    
  data$Guideline <- unlist(lapply(1:nrow(data), function(x) {
    format_guideline(data$Guideline[x],
                     data$Direction[x],
                     data$Units[x],
                     data$LimitNotes[x],
                     data$LookupNotes[x],
                     sigfig)
  }))
  
  data %>%
    dplyr::mutate(Notes = gsub("NA", "", paste(.data$ConditionNotes, .data$MethodNotes))) %>%
    dplyr::mutate(Notes = dplyr::if_else(.data$Notes == " ", NA_character_, .data$Notes)) %>%
    dplyr::select(.data$Variable, .data$Component, Value = .data$Use, 
                  .data$Media, .data$Type, `Predicted Effect Level` = .data$PredictedEffectLevel,
      .data$Status, `WQG Narrative` = .data$NarrativeWQG, .data$Notes,
      .data$Guideline, .data$Reference, .data$`Reference Link`, 
      .data$`Overview Report Link`, .data$`Technical Document Link`)
}

add_lookup <- function(x) {
  x$lookup <- lapply(x$Limit, get_data)
  x
}

add_lookup_condition <- function(x){
  x$Condition <- mapply(get_lookup_codes, x$Limit, x$lookup, x$Condition)
  return(x)
}

get_lookup_codes <- function(Limit, lookup, Condition) {
  if(!is.null(lookup)){
      col_names <-  paste0(colnames(lookup), collapse = " ")
      lookup_parameters <- stringr::str_match_all(col_names, "EMS_.{4}")
      Condition <- paste0(lookup_parameters[[1]], sep = " ", collapse = "")
      return(Condition)
  }
  return(Condition)
}

add_lookupnotes <- function(x){
  x$LookupNotes <- c(rep(NA, nrow(x)))
  x$LookupNotes <- unlist(lapply(x$lookup, get_note))
  x
}

get_note <- function(lookup){
  if (!is.null(lookup[[1]])){
    LimitNote <- "Guideline not available for this pH, Hardness and 
    Dissolved Organic Carbon combination"
  } else {
    return(NA)
  }
}

process_lookups <- function(limits){
  # Example of how to access: limits$lookup[280][[1]]["EMS_0004"]
  limits$lookup <- list(rep(NULL, nrow(limits)))
  limits[!is.na(limits$Limit) & stringr::str_detect(limits$Limit, "[.]csv"),] %<>% add_lookup()
  limits <- add_lookup_condition(limits)
  limits <- add_lookupnotes(limits)
  limits
}


lookup_variables <- function(limits){
  variables <- unlist(lapply(1:nrow(limits), function(i){
  row <- limits[i,]
  if (!is.na(row$LookupNotes)){
    return(row$Variable)
  } else{
    return(NA_character_)
  }
}))
  variables_clean <- unique(variables)
  variables_clean[!is.na(variables_clean)]
}

wqg_filter_variable <- function(variable, x = limits) {
    dplyr::filter(x, .data$Variable == variable)
}

lookup_choices <- function(data, cvalue_codes){
  drop_choices <- dplyr::tibble()
  for (i in data$lookup){
    drop_choices <- dplyr::bind_rows(drop_choices, i)
  }
  drop_choices %<>%
    dplyr::select(dplyr::contains("EMS_")) %>% 
    dplyr::distinct() 
  cvals_active <- colnames(drop_choices)
  cvals_inactive <- setdiff(cvalue_codes, cvals_active)
  
  for (codes in cvals_inactive){
    drop_choices[codes] <- NA
  }
  drop_choices
}

get_data <- function(file_name){
  data <- try(bcdata::bcdc_get_data(record = file_name), silent = TRUE)
  if (is_try_error(data)){
    i <- file_name
    internal_data <- internal_tables[[i]]
    data <- internal_data
  } else {
    data <- data
  }
  data
}

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
  x$ConditionPass <- sapply(x$Condition, test_condition, cvalues, USE.NAMES = FALSE)
  ### assumes that never a LimitNote AND Limit
  x$Guideline <- sapply(1:nrow(x), function(y) {
    evaluate_guideline(x$Limit[y], x$lookup[y],
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


add_lookup_table <- function(x) {
  
  x$lookup <- list(rep(NULL, nrow(x)))
  x$lookup <- sapply(x$Limit, function(y){
    if(!is.na(y)) {
      if(stringr::str_detect(y, "[.]csv")){
        file_path <- paste0("../../data-raw/", y)
        lookup_table <- readr::read_csv(file_path)
        return(lookup_table) 
      }
    }
    return(NULL)
  })
  x
}

get_lookup <- function(file_name) {
  file_path <- paste0("../../data-raw/", file_name)
  lookup_table <- readr::read_csv(file_path)
  lookup_table
}

add_lookup <- function(x) {
  x$lookup <- lapply(x$Limit, get_lookup)
  x
}

add_lookup_condition <- function(x){
  x$Condition <- mapply(get_lookup_codes,x$Limit, x$lookup, x$Condition)
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

process_lookups <- function(limits){
  # Example of how to access: limits$lookup[280][[1]]["EMS_0004"]
  limits$lookup <- list(rep(NULL, nrow(limits)))
  limits[!is.na(limits$Limit) & stringr::str_detect(limits$Limit, "[.]csv"),] %<>% add_lookup()
  limits <- add_lookup_condition(limits)
}

# limits <- readr::read_csv("~/Poisson/Data/shinywqg/all_wqgs.csv")
wqg_filter <- function(variable, use, media, type, effect, statistic, x = limits){
  x <- x %>%
   dplyr::filter(Variable == variable,
                  Use %in% use,
                  Media %in% media,
                  Type %in% type,
                  PredictedEffectLevel %in% effect,
                 Statistic %in% statistic) 
  # remove duplicates caused by multiple EMS_Codes
  ems <- sort(unique(x$EMS_Code))
  x %>%
    dplyr::filter(EMS_Code == ems[1])
}

get_combinations <- function(variable, use, data = limits){
  x <- dplyr::filter(data, Variable == variable,
                     Use %in% use)
  l <- list(media = sort(unique(x$Media)),
            type = sort(unique(x$Type)),
            effect = sort(unique(x$PredictedEffectLevel)),
            statistic = sort(unique(x$Statistic)))
  l
}

extract_codes1 <- function(x){
  if(is.na(x)){
    return(NULL)
  }
  reg <- gregexpr("EMS_", x)[[1]]
  if(length(reg) < 2){
    if(reg < 0){
      return(NULL)
    }
  }
  sapply(reg, function(y){
    substr(x, y, y+7)
  })  %>% unique()
}

extract_codes2 <- function(x){
  unique(unlist(lapply(x, extract_codes)))
}

get_use <- function(variable, x = limits){
  unique(x[["Use"]][x[["Variable"]] %in% variable])
}

wqg_evaluate <- function(x, cvalues, sigfig){
  x$ConditionPass <- sapply(x$Condition, test_condition, cvalues, USE.NAMES = FALSE)
  ### assumes that never a LimitNote AND Limit 
  x$Guideline <- sapply(1:nrow(x), function(y){
    note <- x[y, "LimitNotes"]
    direction <- x[y, "Direction"]
    units <- x[y, "Units"]
    if(!is.na(note))
      return(note)
    limit <- signif(calc_limit(x[y, "Limit"], cvalues), sigfig)
    prefix <- ""
    if(direction == "Upper Limit"){
      prefix <- "<= "
    }
    if(direction == "Lower Limit"){
      prefix <- ">= "
    }
    paste0(prefix, limit, " (", units, ")")
  })
  x
}

wqg_clean <- function(data){
  data %>%
    dplyr::filter(ConditionPass) %>%
    dplyr::select(Variable, Use, Media, Type, Statistic, 
                  `Predicted Effect Level` = PredictedEffectLevel,
                  Status, Guideline, Reference:`Technical Document Link`) %>%
    dplyr::select_if(function(x) !all(is.na(x))) 
}

wqg_table <- function(variable, use, term, cvalues){
  
  x <- filter_limits(variable, use, term) %>%
    dplyr::group_by(Variable, Use, Term) %>%
    dplyr::mutate(Equation = lookup_equation(Condition, UpperLimit, cvalues)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() 
  
  x %>% 
    dplyr::mutate(Equation = replace_codes(Equation)) %>%
    dplyr::select(Variable, Use, Term, Guideline,
                  Units, Reference, Equation)
  
}


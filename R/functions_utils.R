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
  unique(unlist(lapply(x, extract_codes1)))
}

variable_use <- function(variable, x = limits){
  unique(x[["Use"]][x[["Variable"]] %in% variable])
}

code_to_variable <- function(code, units = TRUE){
  x <- unique(code)
  variable <- unique(codes$Variable[which(codes$EMS_Code == x)])
  unit <- unique(codes$Units[which(codes$EMS_Code == x)])
  paste0(variable, " (", unit, ")")
}

# get_refs <- function(x){
#   setdiff(unique(x$Reference), NA_character_)
# }

as_math <- function(x){
  paste0("$", x, "$")
}

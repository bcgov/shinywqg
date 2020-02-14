code_to_variable <- function(code, units = TRUE){
  x <- unique(code)
  variable <- unique(codes$Variable[which(codes$EMS_Code == x)])
  unit <- unique(codes$Units[which(codes$EMS_Code == x)])
  paste0(variable, " (", unit, ")")
}

get_refs <- function(x){
  setdiff(unique(x$Reference), NA_character_)
}

as_math <- function(x){
  paste0("$", x, "$")
}

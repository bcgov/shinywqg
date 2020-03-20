get_combinations <- function(variable, use, data = limits) {
  x <- dplyr::filter(data, 
                     Variable == variable,
                     Use %in% use)
  l <- list(
    media = sort(unique(x$Media)),
    type = sort(unique(x$Type)),
    effect = sort(unique(x$PredictedEffectLevel)))
  l
}


extract_codes <- function(x) {
  setdiff(unique(unlist(lapply(x, function(y){
    stringr::str_extract_all(y, "EMS_[[:alnum:]][[:alnum:]_]{3,3}")
  }))), NA)
}

variable_use <- function(variable, x = limits) {
  x <- x[x$Variable == variable,]
  unique(x[["Use"]][x[["Variable"]] == variable])
}

# variable_component <- function(variable, x = limits) {
#   unique(x[["Component"]][x[["Variable"]] == variable])
# }

code_to_variable <- function(code, units = TRUE) {
  x <- unique(code)
  variable <- unique(codes$Variable[which(codes$EMS_Code == x)])
  unit <- unique(codes$Units[which(codes$EMS_Code == x)])
  # replace Hardness Total with Hardness
  variable[variable == "Hardness Total"] <- "Hardness"
  paste0(variable, " (", unit, ")")
}

# recreates html link without shiny.tag class so can be easily pasted
tag_a <- function(x, href) {
  paste0("<a href='", href, "'>", x, "</a>")
}

## x is clean data
get_links <- function(x){
  links <- lapply(1:nrow(x), function(y) {
    df <- x[y, ]
    links <- list(
      "Reference" = df[["Reference Link"]],
      "Overview Report" = df[["Overview Report Link"]],
      "Technical Document" = df[["Technical Document Link"]]
    ) %>%
      remove_nulls() %>%
      remove_nas()
    
    links <- lapply(names(links), function(z) {
      tag_a(z, href = links[z])
    })
    
    links <- paste(links, collapse = "; ")
    # if(links == ""){
    #   links <- NA
    # }
    links
  })
}

get_references <- function(x) {
  y <- as.list(x$Reference)
  names(y) <- 1:nrow(x)
  y %>%
    remove_nulls() %>%
    remove_nas()
}

as_math <- function(x) {
  paste0("$", x, "$")
}

report_cvalues <- function(x, active, output = "html") {
  x <- x[active]
  names(x) <- sapply(names(x), code_to_variable)
  # x <- remove_nulls(x)
  cvalues <- ""
  if(length(x))
    if(output == "html") {
      cvalues <- paste0(names(x), ": ", x, collapse = "<br>")
    } else {
      cvalues <- paste0(names(x), ": ", x, collapse = "  \n")
    }
  paste0(cvalues, "<br><br>")
}

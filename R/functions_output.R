
# wqg_report <- function(variable, use, term, cvalues, rm_equation, rm_condition){
#   x <- wqg_table(variable, use, term, cvalues)
#   cvalues <- clean_cvalues(cvalues, variable, use)
#   x <- x %>%
#     filter_missing(input$rm_missing, input$variable, input$term, input$use) %>%
#     clean_table()
# }

gt_table <- function(x, use, cvalues){
  cvalues <- paste0(names(cvalues), ': ', cvalues, collapse = "<br>") 
  x %>%
    dplyr::group_by(Variable) %>% 
    gt::gt(rowname_col = "Term") %>%
    gt::tab_header(
      title = use,
      subtitle = gt::html(cvalues)
    ) %>%
    gt::fmt_missing(columns = gt::everything()) %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(13)),
      locations = list(
        gt::cells_data(
          columns = gt::vars(Guideline, Equation, Reference)),
        gt::cells_stub(rows = TRUE)
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_group(
        groups = TRUE
      )
    ) %>%
    gt::tab_footnote(
      "Canada 2014 <this is incomplete>",
      locations = gt::cells_data(
        columns = gt::vars(Reference),
        rows = Reference == "CANADA_2014"
      )
    ) %>%
    gt::tab_footnote(
      "British Columbia 2015 <this is incomplete>",
      locations = gt::cells_data(
        columns = gt::vars(Reference),
        rows = Reference == "BC_2015"
      )
    ) %>%
    gt::tab_options(footnotes.font.size = gt::px(11),
                    table.width  = gt::px(600),
                    row_group.padding = gt::px(15),  
                    heading.title.font.size = gt::px(18),
                    heading.title.font.weight = "bold")
}

wqg_table <- function(variable, use, term, cvalues){
  
  x <- filter_limits(variable, use, term) %>%
    dplyr::group_by(Variable, Use, Term) %>%
    dplyr::mutate(Equation = lookup_equation(Condition, UpperLimit, cvalues)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() 
  
  x$Guideline <- sapply(x$Equation, calc_limit, cvalues)

  x %>% 
    dplyr::mutate(Equation = replace_codes(Equation)) %>%
    dplyr::select(Variable, Use, Term, Guideline,
                  Units, Reference, Equation)
    
}

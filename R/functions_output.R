# df <- data.frame(key = 1, 
#                  equation = c("$$exp(1.6 - 3.327 \\times x^2)$$"))
# gt::gt(df)
gt_table <- function(x, cvalues) {
  variable <- unique(x$Variable)
  refs <- get_references(x)
  x$Reference <- NA
  if(length(refs)){
    x[names(refs), "Reference"] <- "See Footnotes"
  }
  
  # has_notes <- any(!is.na(x$Notes))
  
  links <- get_links(x)
  x$Links <- NA
  has_links <- any(unlist(links) != "")
  if(has_links){
    x$Links <- links
  }
  
  x <- x %>% mutate(VariableComponent = paste0("(", Variable, " ", Component, ")"))
  
  ### remove cols if all NA or links but always keep Guideline 
  keep <- unique(c(names(x)[sapply(x, function(y) !all(is.na(y)))], "Guideline"))
  keep <- setdiff(keep, c("Reference Link", "Overview Report Link", 
                          "Technical Document Link", "Reference",
                          "Variable", "Component"))
  x <- x[keep]
  # all_na <- setdiff(, "Guideline")
  # print(all_na)

  gt <- x %>%
    dplyr::group_by(Value, VariableComponent) %>%
    gt::gt()
  
  gt <- gt %>%
    gt::tab_header(
      title = variable,
      subtitle = gt::html(cvalues)
    ) %>%
    gt::fmt_missing(columns = gt::everything()) %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(15), weight = "bold"),
      locations = list(
        gt::cells_data(
          columns = gt::vars(Guideline))
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(12)),
      locations = list(
        gt::cells_data(
          columns = gt::matches("Notes|Links|Narrative"))
      )
    ) %>%
    gt::fmt_markdown(columns = dplyr::contains("Links")) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = list(
        gt::cells_title(groups = "title"))
    ) %>%
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = list(
        gt::cells_group(groups = TRUE))
    ) %>%
    gt::tab_options(table.font.size = gt::px(14),
                    heading.subtitle.font.size = gt::px(16),
                    column_labels.font.size = gt::px(15),
                    footnotes.font.size = gt::px(13),
                    footnotes.padding = gt::px(10),
                    table.width = gt::pct(90),
                    row_group.padding = gt::px(18),
                    row_group.font.size = gt::px(16),
                    heading.title.font.size = gt::px(21),
                    column_labels.font.weight = "bold",
                    heading.title.font.weight = "bold")
  
  #### need to make it so only says 'see footnotes' when there is a note
  if(length(refs)){
    for(i in names(refs)) {
      gt <- gt::tab_footnote(gt,
                             footnote = refs[[i]][1],
                             locations = gt::cells_data(
                               columns = gt::vars(Guideline),
                               rows = as.numeric(i)
                             ))
    }
  }
  gt
}

gt_data <- function(x){
  gt <- x %>%
    gt::gt() %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::tab_options(table.font.size = gt::px(12),
                    table.width = gt::pct(100),
                    column_labels.font.weight = "bold")
  gt
  
}

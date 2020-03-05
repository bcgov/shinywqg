# df <- data.frame(key = 1, 
#                  equation = c("$$exp(1.6 - 3.327 \\times x^2)$$"))
# gt::gt(df)
gt_table <- function(x, cvalues) {
  variable <- unique(x$Variable)
  note <- get_footnotes(x)
  print(note)
  x$Notes <- NA
  if(length(note)){
    x[names(note), "Notes"] <- "See Footnotes"
  }
  
  links <- get_links(x)

  x$Links <- links

  gt <- x %>%
    dplyr::select(Use:Status, Reference, Links, Guideline, Notes) %>%
    dplyr::select(-Statistic) %>%
    dplyr::select_if(function(x) !all(is.na(x))) %>%
    dplyr::group_by(Use) %>%
    gt::gt()
  
  gt <- gt %>%
    gt::tab_header(
      title = variable,
      subtitle = gt::html(cvalues)
    ) %>%
    gt::fmt_markdown(columns = gt::vars(Links)) %>%
    gt::fmt_missing(columns = gt::everything()) %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(17)),
      locations = list(
        gt::cells_data(
          columns = gt::vars(Guideline))
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(12)),
      locations = list(
        gt::cells_data(
          columns = gt::vars(Links, Reference, Notes))
      )
    ) %>%
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
    gt::tab_options(table.font.size = gt::px(15),
                    heading.subtitle.font.size = gt::px(16),
                    footnotes.font.size = gt::px(15),
                    table.width = gt::pct(90),
                    row_group.padding = gt::px(15),
                    heading.title.font.size = gt::px(23),
                    column_labels.font.weight = "bold",
                    heading.title.font.weight = "bold")
  
  #### need to make it so only says 'see footnotes' when there is a note
  if(length(note)){
    for(i in names(note)) {
      gt <- gt::tab_footnote(gt,
                             footnote = note[[i]][1],
                             locations = gt::cells_data(
                               columns = gt::vars(Notes),
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

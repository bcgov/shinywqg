
gt_table <- function(x, cvalues, note, output = "html") {
  variable <- unique(x$Variable)
  gt <- x %>%
    dplyr::group_by(Use) %>%
    dplyr::select(Use:Guideline) %>%
    dplyr::rename(`Effect Level` = `Predicted Effect Level`) %>%
    gt::gt()

  if(output == "html") {
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
        style = gt::cell_text(size = gt::px(17)),
        locations = list(
          gt::cells_data(
            columns = gt::vars(Guideline))
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
        footnotes.font.size = gt::px(13),
        table.width = gt::pct(90),
        row_group.padding = gt::px(15),
        heading.title.font.size = gt::px(23),
        column_labels.font.weight = "bold",
        heading.title.font.weight = "bold")
  }

  for(i in 1:nrow(x)) {
    gt <- gt::tab_footnote(gt,
      footnote = gt::html(note[[i]]),
      locations = gt::cells_data(
        columns = gt::vars(Guideline),
        rows = i
    ))
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

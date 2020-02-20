#' Create gt table
#'
#' @export
gt_table <- function(x, cvalues, output = "html"){
  variable <- unique(x$Variable)
  note <- get_footnotes(x, output)
  gt <- x %>%
    dplyr::group_by(Use) %>%
    dplyr::select(Use:Guideline) %>%
    dplyr::rename(`Effect Level` = `Predicted Effect Level`) %>%
    gt::gt()
  
  if(output == "html"){
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
        style = gt::cell_text(size = gt::px(13)),
        locations = list(
          gt::cells_data(
            columns = gt::vars(Media, Type, Statistic, `Effect Level`, Status))
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = list(
          gt::cells_group(groups = TRUE),
          gt::cells_title(groups = "title"))
      ) %>%
      gt::tab_options(footnotes.font.size = gt::px(11),
                      table.width  = gt::px(600),
                      row_group.padding = gt::px(15),  
                      heading.title.font.size = gt::px(18),
                      heading.title.font.weight = "bold")
  }

  for(i in 1:nrow(x)){
    gt <- gt::tab_footnote(gt, 
                           footnote = gt::html(note[[i]]),
                           locations = gt::cells_data(
                             columns = gt::vars(Guideline),
                             rows = i
                           ))
  }
  gt
}

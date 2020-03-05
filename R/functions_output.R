# df <- data.frame(key = 1, 
#                  equation = c("$$exp(1.6 - 3.327 \\times x^2)$$"))
# gt::gt(df)
gt_table <- function(x, cvalues) {
  variable <- unique(x$Variable)
  note <- get_footnotes(x)
  x$Reference <- NA
  if(length(note)){
    x[names(note), "Reference"] <- "See Footnotes"
  }
  
  has_notes <- any(!is.na(x$Notes))
  
  links <- get_links(x)
  x$Links <- NA
  has_links <- any(unlist(links) != "")
  if(has_links){
    x$Links <- links
  }

  gt <- x %>%
    dplyr::select(Use:Status, Guideline, Notes, Links) %>%
    dplyr::select(-Statistic) %>%
    dplyr::select_if(function(x) !all(is.na(x))) %>%
    dplyr::group_by(Use) %>%
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
    # gt::tab_style(
    #   style = gt::cell_text(size = gt::px(12)),
    #   locations = list(
    #     gt::cells_data(
    #       columns = gt::vars(Reference))
    #   )
    # ) %>%
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
                    footnotes.padding = gt::px(10),
                    table.width = gt::pct(90),
                    row_group.padding = gt::px(18),
                    heading.title.font.size = gt::px(23),
                    column_labels.font.weight = "bold",
                    heading.title.font.weight = "bold")
  
  if(has_notes){
    gt <- gt %>% 
      gt::tab_style(
      style = gt::cell_text(size = gt::px(12)),
      locations = list(
        gt::cells_data(
          columns = gt::vars(Notes))
      )
    ) 
  }
  
  if(has_links){
    gt <- gt %>% 
      gt::tab_style(
        style = gt::cell_text(size = gt::px(12)),
        locations = list(
          gt::cells_data(
            columns = gt::vars(Links))
        )
      ) %>%
      gt::fmt_markdown(columns = gt::vars(Links)) 
  }
  
  #### need to make it so only says 'see footnotes' when there is a note
  if(length(note)){
    for(i in names(note)) {
      gt <- gt::tab_footnote(gt,
                             footnote = note[[i]][1],
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

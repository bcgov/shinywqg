app_ui <- function() {
  tagList(
    css_hide_errors(),
    css_navbar(background_color = bcgov_pal$blue, 
                           text_color = bcgov_pal$white, 
                           text_selected_color = bcgov_pal$yellow),
    css_button(), 
    css_body(text_color = bcgov_pal$black),
    waiter::use_butler(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = waiter_html("Fetching limits spreadsheet from BC Data Catalogue")),
    shinyjs::useShinyjs(),
    navbarPage(title =  "ShinyWqg",
      selected = "WQG",
      tabPanel(title = "WQG",
        br(),
        mod_data_ui("data_ui_1")))
  )
}

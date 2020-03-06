app_ui <- function() {
  tagList(
    css_hide_errors(),
    css_navbar(background_color = bcgov_pal$blue, 
                           text_color = bcgov_pal$white, 
                           text_selected_color = bcgov_pal$yellow),
    css_button(), 
    css_body(text_color = bcgov_pal$black),
    waiter::use_butler(),
    shinyjs::useShinyjs(),
    navbarPage(title =  "ShinyWqg",
      selected = "WQG",
      tabPanel(title = "WQG",
        br(),
        mod_data_ui("data_ui_1")),
      tabPanel(title = "About",
        br(),
        mod_about_ui("about_ui_1")))
  )
}

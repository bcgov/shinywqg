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
    waiter::show_waiter_on_load(html = waiter_html("Fetching guideline spreadsheet from BC Data Catalogue")),
    shinyjs::useShinyjs(),
    navbarPage(title =  "B.C. Ambient Water Quality Guidelines",
      selected = "WQG",
      tabPanel(title = "WQG",
        br(),
        mod_data_ui("data_ui_1")),
      tabPanel(title = "User Guide",
               br(),
               mod_guide_ui("guide_ui_1")),
      tabPanel(title = "About",
               br(),
               mod_about_ui("about_ui_1")))
  )
}

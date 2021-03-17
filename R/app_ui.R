app_ui <- function() {
  tagList(
    css_hide_errors(),
    add_external_resources(),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = waiter_html("Fetching guideline spreadsheet from BC Data Catalogue")),
    shinyjs::useShinyjs(),
    navbarPage(title =  "B.C. Ambient Water Quality Guidelines",
      selected = "WQG",
      tabPanel(title = "WQG",
        mod_data_ui("data_ui_1")),
      tabPanel(title = "User Guide",
               mod_guide_ui("guide_ui_1")),
      tabPanel(title = "About",
               mod_about_ui("about_ui_1"))),
    div(class = "footer",
        includeHTML("www/footer.html")
    )
  )
}

add_external_resources <- function() {
  addResourcePath("www", system.file("app/www", package = "shinywqg"))
  tagList(tags$link(rel = "stylesheet", type = "text/css", href = "www/bcgov.css"))
}

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source('global.R', local = TRUE)

ui <- function() {
  tagList(
    css_hide_errors(),
    tagList(tags$link(rel = "stylesheet", type = "text/css", href = "bcgov.css")),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = waiter_html("Fetching guideline spreadsheet from BC Data Catalogue")),
    shinyjs::useShinyjs(),
    navbarPage(title =  "B.C. Ambient Water Quality Guidelines",
               selected = "WQG",
               tabPanel(title = "WQG",
                        mod_data_ui("data_ui_1")),
               tabPanel(title = "User Guide",
                        mod_guide_ui("mod_guide")),
               tabPanel(title = "About",
                        mod_about_ui("mod_about"))),
    div(class = "footer",
        includeHTML("www/footer.html")
    )
  )
}



server <- function(input, output, session) {
  
  callModule(mod_data_server, "data_ui_1")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
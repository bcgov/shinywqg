# Module UI

#' @title   mod_data_ui and mod_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data
#' @keywords internal
mod_data_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      selectInput(ns("variable"), label = "Variable",
                  choices = c(limits$Variable, ""),
                  selected = ""),
      uiOutput(ns("ui_guideline")),
      uiOutput(ns("ui_dependent"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Table",
                 br(),
                 dl_button(ns("dl_csv"), "Download csv"),
                 dl_button(ns("dl_excel"), "Download excel"),
                 br2(),
                 tableOutput(ns("table_guideline"))),
        tabPanel(title = "Report"
                 )
      )
      
    )
  )
}

# Module Server

#' @rdname mod_data
#' @keywords internal

mod_data_server <- function(input, output, session) {
  ns <- session$ns
  
  get_limit <- reactive({
    req(input$variable)
    waiter::show_butler()
    x <- wqbc::lookup_limits(
      ph = input$EMS_0004, 
      hardness = input$EMS_0107, 
      methyl_mercury = input$EMS_HGME, 
      chloride = input$EMS_0104)
    
    x <- x[x$Variable == input$variable,]
    waiter::hide_butler()
    x
  })
  
  output$ui_guideline <- renderUI({
    req(input$variable)
    selectInput(ns("guideline"), label = "Guideline",
                choices = c(get_guidelines(input$variable), ""),
                selected = '')
  })
  
  output$ui_dependent <- renderUI({
    numeric_inputs(extract_codes(input$variable, input$guideline), ns)
  })
  
  output$table_guideline <- renderTable({
    get_limit()
  })

}

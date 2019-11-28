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
      select_input_x(ns("variable"), label = "Variable",
                  choices = c(limits$Variable, ""),
                  selected = ""),
      uiOutput(ns("ui_guideline")),
      uiOutput(ns("ui_dependent")),
      uiOutput(ns("ui_term"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Table",
                 br(),
                 uiOutput(ns("ui_dl_table")),
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
    req(input$guideline)
    req(input$term)
    waiter::show_butler()
    x <- wqg_table(variable = input$variable,
                   guideline = input$guideline,
                   term = input$term,
                   ph = input$EMS_0004, 
                   hardness = input$EMS_0107, 
                   methyl_mercury = input$EMS_HGME, 
                   chloride = input$EMS_0104)
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
  
  output$ui_term <- renderUI({
    req(input$variable)
    req(input$guideline)
    checkboxGroupInput(ns("term"), "Term", 
                       choices = c("short", "long"),
                       selected = c("short", "long"),
                       inline = TRUE)
  })
  output$table_guideline <- renderTable({
    get_limit()
  })
  
  output$ui_dl_table <- renderUI({
    req(get_limit())
    tagList(
      dl_button(ns("dl_csv"), "Download csv"),
      dl_button(ns("dl_excel"), "Download excel") 
    )
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() "wqg_table.csv",
    content = function(file) {
      readr::write_csv(get_limit(), file)
    })
  
  output$dl_excel <- downloadHandler(
    filename = function() "wqg_table.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(get_limit(), file)
    })

}

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
      select_input_x(ns("variable"), 
                     label = "Select Variable(s)", 
                     choices = c(limits$Variable, ""), 
                     selected = ""),
      uiOutput(ns("ui_use")),
      uiOutput(ns("ui_dependent")),
      uiOutput(ns("ui_term")),
      uiOutput(ns("ui_rm_missing"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Table",
                 br(),
                 uiOutput(ns("ui_dl_table")),
                 br2(),
                 tableOutput(ns("table"))),
        tabPanel(title = "Report",
                 br(),
                 uiOutput(ns("ui_dl_report")),
                 br2(),
                 uiOutput(ns("report")))
      )
    )
  )
}

# Module Server

#' @rdname mod_data
#' @keywords internal

mod_data_server <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$info_missing, {
    shinyjs::toggle("div_info_missing", anim = TRUE)
  })
  
  get_limit <- reactive({
    req(input$variable)
    req(input$use)
    req(input$term)
    waiter::show_butler()
    x <- wqg_table(variable = input$variable,
                   use = input$use,
                   term = input$term,
                   ph = input$EMS_0004, 
                   hardness = input$EMS_0107, 
                   methyl_mercury = input$EMS_HGME, 
                   chloride = input$EMS_0104,
                   total_mercury = input$EMS_HG_T)
    waiter::hide_butler()
    x
  })
  
  get_limit2 <- reactive({
    req(get_limit())
   get_limit()
   
  })
  
  output$ui_use <- renderUI({
    req(input$variable)
    select_input_x(ns("use"), label = "Select Use(s)",
                choices = c(get_uses(input$variable), ""),
                selected = '')
  })
  
  output$ui_dependent <- renderUI({
    numeric_inputs(extract_codes(input$variable, input$use), ns)
  })
  
  output$ui_term <- renderUI({
    req(input$variable)
    req(input$use)
    checkboxGroupInput(ns("term"), "Term", 
                       choices = c("short", "long"),
                       selected = c("short", "long"),
                       inline = TRUE)
  })
  
  output$ui_rm_missing <- renderUI({
    req(input$variable)
    req(input$use)
    checkboxGroupInput(ns("rm_missing"), label = "Remove missing data",
                       choices = c("if no equation is available" = "equation", 
                                   "if a condition has failed" = "condition"),
                       selected = NULL) %>% 
      embed_help("info_missing", ns, missing_help)
  })
  
  output$table <- renderTable(digits = -1, {
    get_limit2()
  })
  
  output$ui_dl_table <- renderUI({
    req(get_limit2())
    tagList(
      dl_button(ns("dl_csv"), "csv table"),
      dl_button(ns("dl_excel"), "excel table")
    )
  })
  
  output$ui_dl_report <- renderUI({
    req(get_limit2())
    tagList(
      dl_button(ns("dl_html"), "html report"),
      dl_button(ns("dl_pdf"), "pdf report"),
      dl_button(ns("dl_rmd"), "Rmarkdown file")
    )
  })
  
  output$report <- renderUI({
    temp_report <- file.path(tempdir(), paste0(session$token, ".Rmd"))
    params <- list(table = get_limit2())
    rmarkdown::render(system.file("extdata", package = "shinywqg", 
                                  "report_html.Rmd"),
                      output_file = temp_report,
                      params = params,
                      envir = new.env(parent = globalenv()))
    tags$div(
      class = "rmd-class",
      includeHTML(temp_report)
    )
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() "wqg_table.csv",
    content = function(file) {
      readr::write_csv(get_limit2(), file)
    })
  
  output$dl_excel <- downloadHandler(
    filename = function() "wqg_table.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(get_limit2(), file)
    })
  
  output$dl_html <- downloadHandler(
    filename = "wqg_report.html",
    content = function(file) {
      temp_report <- file.path(tempdir(), "report_html.Rmd")
      file.copy(system.file("extdata", package = "shinywqg", "report_html.Rmd"),
                temp_report, overwrite = TRUE)
      params <- list(table = get_limit2())
      rmarkdown::render(temp_report, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$dl_pdf <- downloadHandler(
    filename = "wqg_report.pdf",
    content = function(file) {
      temp_report <- file.path(tempdir(), "report_pdf.Rmd")
      file.copy(system.file("extdata", package = "shinywqg", "report_pdf.Rmd"),
                temp_report, overwrite = TRUE)
      params <- list(table = get_limit2())
      rmarkdown::render(temp_report, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$dl_rmd <- downloadHandler(
    filename = "wqg_report.Rmd",
    content = function(file) {
      file.copy(system.file("extdata", package = "shinywqg", "report_html.Rmd"), file)
    }
  )

}

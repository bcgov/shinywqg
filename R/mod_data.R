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
      selectizeInput(ns("variable"), 
                     label = "Select Variable", 
                     choices = c(limits$Variable, ""), 
                     selected = "",
                     multiple = FALSE),
      uiOutput(ns("ui_use")),
      uiOutput(ns("ui_media")),
      uiOutput(ns("ui_type")),
      uiOutput(ns("ui_effect")),
      uiOutput(ns("ui_statistic")),
      shinyjs::hidden(numeric_inputs(cvalue_codes, ns)),
      uiOutput(ns("ui_sigfig"))
    ),
    mainPanel(
      tagList(
        shinyWidgets::dropdownButton(status = "primary",
                                     label = "Report",
                                     size = "sm",
                                     inline = TRUE,
                                     circle = FALSE,
                                     icon = icon("download"),
                                     dl_button(ns("dl_html"), "HTML"),
                                     dl_button(ns("dl_pdf"), "PDF"),
                                     dl_button(ns("dl_rmd"), "Rmarkdown")
        ),
        shinyWidgets::dropdownButton(status = "primary",
                                     label = "Raw Data",
                                     size = "sm",
                                     inline = TRUE,
                                     circle = FALSE,
                                     icon = icon("download"),
                                     dl_button(ns("dl_csv_raw"), "CSV"),
                                     dl_button(ns("dl_excel_raw"), "Excel")
        ),
        shinyWidgets::dropdownButton(status = "primary",
                                     label = "Report Data",
                                     size = "sm",
                                     inline = TRUE,
                                     circle = FALSE,
                                     icon = icon("download"),
                                     dl_button(ns("dl_csv_report"), "CSV"),
                                     dl_button(ns("dl_excel_report"), "Excel")
        )
      ),
      br2(),
      tabsetPanel(
        tabPanel(title = "Report",
                 br(),
                 gt::gt_output(ns("table"))),
        tabPanel(title = "Raw Data",
                 br(),
                 uiOutput(ns("ui_data_raw"))),
        tabPanel(title = "Report Data",
                 br(),
                 uiOutput(ns("ui_data_report")))
      )
    )
  )
}

# Module Server

#' @rdname mod_data
#' @keywords internal

mod_data_server <- function(input, output, session) {
  ns <- session$ns
  
  observe({
    if(!is.null(input$variable) & input$variable == ""){
      return({
        for(i in cvalue_codes){
          shinyjs::hide(i)
        }
      })
    }
    for(i in rv$cvalue_inactive){
      shinyjs::hide(i)
    }
    for(i in rv$cvalue_active){
      shinyjs::show(i)
    }
  })
  
  output$ui_sigfig <- renderUI({
    numericInput(ns("sigfig"), label = "Guideline Significant Figures", value = 2)
  })
  
  cvalues <- reactive({
    x <- cvalue_codes
    set_names(lapply(x, function(y){input[[y]]}), x)
  })
  
  wqg_data_raw <- reactive({
    req(input$sigfig)
    req(input$variable)
    req(input$use)
    req(input$media)
    req(input$type)
    req(input$effect)
    req(input$statistic)
    wqg_filter(input$variable, input$use, input$media, 
               input$type, input$effect, input$statistic) 
  })
  
  wqg_data_evaluate <- reactive({
    wqg_data_raw() %>%
      wqg_evaluate(cvalues = cvalues(), sigfig = input$sigfig)
  })
  
  wqg_data_report <- reactive({
    wqg_data_evaluate() %>%
      wqg_clean()
  })
  
  combinations <- reactive({
    req(input$variable)
    req(input$use)
    get_combinations(input$variable, input$use)
  })
  
  rv <- reactiveValues(
    cvalue_active = NULL,
    cvalue_inactive = NULL
  )
  
  observe({
    data <- wqg_data_raw()
    cval <- extract_codes2(data$Condition)
    rv$cvalue_active <- cval
    rv$cvalue_inactive <- setdiff(cvalue_codes, cval)
  })
  
  output$ui_use <- renderUI({
    req(input$variable)
    select_input_x(ns("use"), label = "Select Use(s)",
                   choices = c(variable_use(input$variable), ""),
                   selected = "")
  })
  
  output$ui_media <- renderUI({
    x <- combinations()$media
    checkboxGroupInput(ns("media"), "Select Media", 
                       choices = x,
                       selected = x,
                       inline = TRUE)
  })
  
  output$ui_type <- renderUI({
    x <- combinations()$type
    checkboxGroupInput(ns("type"), "Select Type(s)", 
                       choices = x,
                       selected = x,
                       inline = TRUE)
  })
  
  output$ui_effect <- renderUI({
    x <- combinations()$effect
    checkboxGroupInput(ns("effect"), "Select Effect(s)", 
                       choices = x,
                       selected = x,
                       inline = TRUE)
  })
  
  output$ui_statistic <- renderUI({
    x <- combinations()$statistic
    checkboxGroupInput(ns("statistic"), "Select Statistic(s)", 
                       choices = x,
                       selected = x,
                       inline = TRUE)
  })
  
  output$table <- gt::render_gt({
    # if(nrow(rv$report) == 0) return()
    gt_table(wqg_data_report(), cvalues(), rv$cvalue_active)
  })
  
  output$ui_data_raw <- renderUI({
    req(wqg_data_evaluate())
    table_output(ns("data_raw"))
  })
  
  output$data_raw <- DT::renderDT({
    data_table(wqg_data_evaluate())
  })
  
  output$ui_data_report <- renderUI({
    req(wqg_data_report())
    table_output(ns("data_report"))
  })
  
  output$data_report <- DT::renderDT({
    data_table(wqg_data_report())
  })
  
  raw_dl <- reactive({
    x <- empty_evaluate
    if(input$variable != ""){
      if(!is.null(input$use)){
        x <- wqg_data_evaluate()
      }
    }
    x
  })
  
  report_dl <- reactive({
    x <- empty_report
    if(input$variable != ""){
      if(!is.null(input$use)){
        x <- wqg_data_report()
      }
    }
    x
  })
  
  output$dl_csv_raw <- downloadHandler(
    filename = function() "wqg_data_raw.csv",
    content = function(file) {
      readr::write_csv(raw_dl(), file)
    })
  
  output$dl_excel_raw <- downloadHandler(
    filename = function() "wqg_data_raw.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(raw_dl(), file)
    })
  
  output$dl_csv_report <- downloadHandler(
    filename = function() "wqg_data_report.csv",
    content = function(file) {
      readr::write_csv(report_dl(), file)
    })
  
  output$dl_excel_report <- downloadHandler(
    filename = function() "wqg_data_report.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(report_dl(), file)
    })
  
  output$dl_html <- downloadHandler(
    filename = "wqg_report.html",
    content = function(file) {
      temp_report <- file.path(tempdir(), "report_html.Rmd")
      file.copy(system.file("extdata", package = "shinywqg", "report_html.Rmd"),
                temp_report, overwrite = TRUE)
      params <- list(use = params_rv$use,
                     table = params_rv$table,
                     cvalues = params_rv$cvalues)
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
      params <- list(use = params_rv$use,
                     table = params_rv$table,
                     cvalues = params_rv$cvalues)
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

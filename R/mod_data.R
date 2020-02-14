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
      selectInput(ns("variable"), 
                     label = "Select Variable", 
                     choices = c(limits$Variable, ""), 
                     selected = "",
                  multiple = FALSE),
      uiOutput(ns("ui_use")),
      uiOutput(ns("ui_media")),
      uiOutput(ns("ui_type")),
      uiOutput(ns("ui_effect")),
      uiOutput(ns("ui_statistic")),
      uiOutput(ns("ui_cvalues")),
      uiOutput(ns("ui_cvaluenote")),
      uiOutput(ns("ui_sigfig"))
      # uiOutput(ns("ui_rm_missing")),
      # shinyjs::hidden(button(ns("get"), "Get/Update Guidelines"))
    ),
    mainPanel(
      uiOutput(ns("ui_dl")),
      br(),
      tabsetPanel(
        tabPanel(title = "Report",
                 gt::gt_output(ns("table"))),
        tabPanel(title = "Raw Data",
                 table_output(ns("data_raw"))),
        tabPanel(title = "Report Data",
                 table_output(ns("data_report")))
      )
    )
  )
}

# Module Server

#' @rdname mod_data
#' @keywords internal

mod_data_server <- function(input, output, session) {
  ns <- session$ns
  
  output$ui_cvalues <- renderUI({
    req(wqg_data_raw())
    data <- wqg_data_raw()
    cval <- extract_codes2(data$Condition)
    numeric_inputs(cval, ns)
  })
  
  output$ui_sigfig <- renderUI({
    # if(nrow(wqg_data_raw()) < 1) return()
    numericInput(ns("sigfig"), label = "Guideline Significant Figures", value = 2)
  })
  
  # output$ui_cvaluenote <- renderUI({
  #   if(nrow(wqg_data()) == 0) return()
  #   
  #   notes <- wqg_data()$ConditionNotes
  #   x <- lapply(seq_along(notes), function(x){
  #     paste0(x, ". ", notes[x])
  #   })
  #   tagList(
  #     tags$label("Condition Notes"),
  #     HTML(paste(x, collapse = "<br>"))
  #   )
  # })
  
  cvalues <- reactive({
    x <- cvalue_codes
    set_names(lapply(x, function(y){input[[y]]}), x)
  })
  

  # observe({
  #   use <- input$use
  #   if(!is.null(use)){
  #     if(use != ""){
  #       shinyjs::show("get")
  #     } else {
  #       shinyjs::hide("get")
  #     }
  #   } else {
  #     shinyjs::hide("get")
  #   }
  # })
  # 
  # observeEvent(input$info_missing, {
  #   shinyjs::toggle("div_info_missing", anim = TRUE)
  # })
  # 

  # 
  # params_rv <- reactiveValues(data = NULL,
  #                             table = NULL,
  #                             refs = NULL,
  #                             cvalues = NULL,
  #                             use = NULL)
  # 
  # observeEvent(input$get, {
  #   suppressWarnings(waiter::show_butler())
  #   x <- wqg_table(variable = input$variable,
  #                  use = input$use,
  #                  term = input$term,
  #                  cvalues = cvalues())
  #   
  #   cvalues <- clean_cvalues(cvalues(), input$variable, input$use)
  # 
  #   params_rv$use <- input$use
  #   # params_rv$data <- x
  #   params_rv$refs <- get_refs(x)
  #   params_rv$cvalues <- cvalues
  #   
  #   y <- x %>%
  #     filter_missing(input$rm_missing, input$variable, input$term, input$use) %>%
  #     clean_table()
  #   
  #   params_rv$table <- y
  #   suppressWarnings(waiter::hide_butler())
  # })
  # 
  # output$table <- gt::render_gt({
  #   req(params_rv$table)
  #   gt_table(params_rv$table, params_rv$use, params_rv$cvalues)
  # })
  # 
  output$ui_use <- renderUI({
    req(input$variable)
    select_input_x(ns("use"), label = "Select Use(s)",
                choices = c(variable_use(input$variable), ""),
                selected = "")
  })
  
  combinations <- reactive({
    req(input$variable)
    req(input$use)
    get_combinations(input$variable, input$use)
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
  
  wqg_data_raw <- reactive({
    req(input$sigfig)
    req(input$variable)
    req(input$use)
    req(input$media)
    req(input$type)
    req(input$effect)
    req(input$statistic)
    wqg_filter(input$variable, input$use, input$media, 
               input$type, input$effect, input$statistic) %>%
      wqg_evaluate(cvalues = cvalues(), sigfig = input$sigfig)
  })
  
  wqg_data_report <- reactive({
    wqg_data_raw() %>%
      wqg_clean()
  })
  
  output$data_raw <- DT::renderDT({
    data_table(wqg_data_raw())
  })
  
  output$data_report <- DT::renderDT({
    data_table(wqg_data_report() )
  })
  
  output$ui_dl <- renderUI({
    req(wqg_data_report())
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
      # dl_button(ns("dl_refs"), "References")
    )
  })
  # 
  output$dl_csv_raw <- downloadHandler(
    filename = function() "wqg_data_raw.csv",
    content = function(file) {
      readr::write_csv(wqg_data_raw(), file)
    })

  output$dl_excel_raw <- downloadHandler(
    filename = function() "wqg_data_raw.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(wqg_data_raw(), file)
    })
  
  output$dl_csv_report <- downloadHandler(
    filename = function() "wqg_data_report.csv",
    content = function(file) {
      readr::write_csv(wqg_data_report(), file)
    })
  
  output$dl_excel_report <- downloadHandler(
    filename = function() "wqg_data_report.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(wqg_data_report(), file)
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

  # output$dl_refs <- downloadHandler(
  #     filename = "refs.zip",
  #     content = function(fname) {
  #       tmpdir <- tempdir()
  #       setwd(tempdir())
  #       files <- paste0(params_rv$refs, ".pdf")
  #       for(i in files){
  #         file.copy(system.file(package = "shinywqg", file.path("extdata/", i)), i)
  #       }
  #       utils::zip(zipfile = fname, files = files)
  #     },
  #     contentType = "application/zip"
  # )
}

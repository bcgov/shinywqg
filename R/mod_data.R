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
    sidebarPanel(width = 3,
      selectizeInput(ns("variable"),
        label = "Select Variable",
        choices = c(limits$Variable, ""),
        selected = "",
        multiple = FALSE),
      uiOutput(ns("ui_component")),
      uiOutput(ns("ui_use")),
      uiOutput(ns("ui_media")),
      uiOutput(ns("ui_type")),
      uiOutput(ns("ui_effect")),
      # uiOutput(ns("ui_statistic")),
      shinyjs::hidden(numeric_inputs(cvalue_codes, ns)),
      uiOutput(ns("ui_sigfig"))
    ),
    mainPanel(width = 9,
      tagList(
        shinyWidgets::dropdownButton(status = "primary",
          label = "Report",
          size = "sm",
          inline = TRUE,
          circle = FALSE,
          icon = icon("download"),
          dl_button(ns("dl_html"), "HTML"),
          dl_button(ns("dl_pdf"), "PDF")
          # dl_button(ns("dl_rmd"), "Rmarkdown")
        ),

        shinyWidgets::dropdownButton(status = "primary",
          label = "Report Data",
          size = "sm",
          inline = TRUE,
          circle = FALSE,
          icon = icon("download"),
          dl_button(ns("dl_csv_report"), "CSV"),
          dl_button(ns("dl_excel_report"), "Excel")),
        shinyWidgets::dropdownButton(status = "primary",
          label = "Raw Data",
          size = "sm",
          inline = TRUE,
          circle = FALSE,
          icon = icon("download"),
          dl_button(ns("dl_csv_raw"), "CSV"),
          dl_button(ns("dl_excel_raw"), "Excel"))
      ),
      br2(),
      tabsetPanel(
        tabPanel(title = "Report",
          br(),
          gt::gt_output(ns("table"))),
        tabPanel(title = "Report Data",
          table_output(ns("data_report"))),
        tabPanel(title = "Raw Data",
          table_output(ns("data_raw")))
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
    if(input$variable == "" | is.null(input$use)) {
      return({
        for(i in cvalue_codes) {
          shinyjs::hide(i)
        }
      })
    }

    for(i in rv$cvalue_inactive) {
      shinyjs::hide(i)
    }
    for(i in rv$cvalue_active) {
      shinyjs::show(i)
    }
  })

  output$ui_sigfig <- renderUI({
    x <- wqg_data_evaluate()
    x <- x[x$ConditionPass,]
    if(any(is.na(as.numeric(x$Limit)))){
      return(numericInput(ns("sigfig"), label = "Guideline Significant Figures", value = 2))
    }
  })

  cvalues <- reactive({
    x <- cvalue_codes
    x <- set_names(lapply(x, function(y) {
      input[[y]]
    }), x)
    x
  })
  
  clean_cvalues <- reactive({
    x <- cvalues()
    x$EMS_1107 <- NA
    x
  })

  wqg_data_evaluate <- reactive({
    req(input$variable)
    req(input$component)
    req(input$use)
    req(input$media)
    req(input$type)
    req(input$effect)
    x <-  wqg_filter(input$variable, input$component,
                     input$use, input$media, input$type, 
                     input$effect)
    if(nrow(x) == 0) return()
    x %>%
      wqg_evaluate(cvalues = clean_cvalues())
  })

  wqg_data_report <- reactive({
    x <-  wqg_data_evaluate()
    if(is.null(x)) return()
    if(nrow(x) == 0) return()
    sigfig <- 2
    if(!is.null(input$sigfig)){
      sigfig <- input$sigfig
    }
    x %>%
      wqg_clean(sigfig)
  })

  combinations <- reactive({
    req(input$variable)
    req(input$component)
    req(input$use)
    get_combinations(input$variable, input$component, input$use)
  })

  rv <- reactiveValues(
    path = "inst/extdata/",
    cvalue_active = NULL,
    cvalue_inactive = NULL,
    raw = empty_evaluate,
    report = empty_report
  )

  observe({
    if(is.null(input$use) | input$variable == "") {
      rv$raw <- empty_evaluate
      rv$report <- empty_report
    }
    data <- wqg_data_evaluate()
    rv$raw <- data
    if(any(data$ConditionPass)) {
      rv$report <- wqg_data_report()
    }
  })

  observe({
    data <- wqg_data_evaluate()
    cval <- extract_codes(data$Condition)
    cval <- setdiff(cval, "EMS_1107")
    rv$cvalue_active <- cval
    rv$cvalue_inactive <- setdiff(cvalue_codes, cval)
  })

  output$ui_use <- renderUI({
    uses <- variable_use(input$variable, input$component)
    select_input_x(ns("use"),
      label = "Select Value(s)",
      choices = uses,
      selected = "")
  })
  
  output$ui_component <- renderUI({
    components <- variable_component(input$variable)
    selectizeInput(ns("component"),
                   label = "Select Component",
                   choices = components,
                   selected = "",
                   multiple = FALSE)
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

  # output$ui_statistic <- renderUI({
  #   x <- combinations()$statistic
  #   checkboxGroupInput(ns("statistic"), "Select Statistic(s)",
  #     choices = x,
  #     selected = x,
  #     inline = TRUE)
  # })

  output$table <- gt::render_gt({
    x <- wqg_data_report()
    if(is.null(x)) return()
    if(nrow(x) == 0) return()
    cvalues <- report_cvalues(cvalues(), rv$cvalue_active)
    gt_table(x, cvalues)
  })

  output$data_raw <- gt::render_gt({
    gt_data(rv$raw)
  })

  output$data_report <- gt::render_gt({
    gt_data(rv$report)
  })

  output$dl_csv_raw <- downloadHandler(
    filename = function() "wqg_data_raw.csv",
    content = function(file) {
      readr::write_csv(rv$raw, file)
    })

  output$dl_excel_raw <- downloadHandler(
    filename = function() "wqg_data_raw.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(rv$raw, file)
    })

  output$dl_csv_report <- downloadHandler(
    filename = function() "wqg_data_report.csv",
    content = function(file) {
      readr::write_csv(rv$report, file)
    })

  output$dl_excel_report <- downloadHandler(
    filename = function() "wqg_data_report.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(rv$report, file)
    })

  output$dl_html <- downloadHandler(
    filename = "wqg_report.html",
    content = function(file) {
      x <- wqg_data_report()
      cvalues <- report_cvalues(cvalues(), rv$cvalue_active)
      gt <- gt_table(x, cvalues)
      gt::gtsave(gt, file)
      # path <- system.file(package = "shinywqg", "extdata/report_html.Rmd")
      # temp_report <- file.path(tempdir(), "report_html.Rmd")
      # file.copy(path, temp_report, overwrite = TRUE)
      #
      # cvalues <- report_cvalues(cvalues(), rv$cvalue_active)
      # data <- rv$report
      # notes <- get_footnotes(data)
      # params <- list(data = data,
      #                cvalues = cvalues,
      #                notes = notes)
      # rmarkdown::render(temp_report,
      #                   output_file = file,
      #                   params = params,
      #                   envir = new.env(parent = globalenv()))
    }
  )

  output$dl_pdf <- downloadHandler(
    filename = "wqg_report.pdf",
    content = function(file) {
      x <- wqg_data_report()
      cvalues <- report_cvalues(cvalues(), rv$cvalue_active)
      gt <- gt_table(x, cvalues)
      gt::gtsave(gt, file, zoom = 1.3, expand = 5)
      #
      # path <- system.file(package = "shinywqg", "extdata/report_pdf.Rmd")
      # temp_report <- file.path(tempdir(), "report_pdf.Rmd")
      # file.copy(path, temp_report, overwrite = TRUE)
      #
      # cvalues <- report_cvalues(cvalues(), rv$cvalue_active, "pdf")
      # data <- rv$report
      # notes <- get_footnotes(data, "pdf")
      # params <- list(data = data,
      #                cvalues = cvalues,
      #                notes = notes)
      # rmarkdown::render(temp_report,
      #                   output_file = file,
      #                   params = params,
      #                   envir = new.env(parent = globalenv()))
    }
  )

  output$dl_rmd <- downloadHandler(
    filename = "wqg_report.Rmd",
    content = function(file) {
      file.copy(system.file(package = "shinywqg", "extdata/report_html.Rmd"), file)
    }
  )
}

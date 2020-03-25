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
        choices = c(unique(limits$Variable), ""),
        selected = "",
        multiple = FALSE),
      uiOutput(ns("ui_use")),
      uiOutput(ns("ui_media")),
      shinyjs::hidden(numeric_inputs(cvalue_codes, ns)),
      uiOutput(ns("ui_sigfig"))
    ),
    mainPanel(width = 9,
      tagList(
        shinyWidgets::dropdownButton(status = "primary",
          label = "Download",
          # size = "sm",
          inline = TRUE,
          circle = FALSE,
          icon = icon("download"),
          dl_button(ns("dl_html"), "Report HTML"),
          dl_button(ns("dl_pdf"), "Report PDF"),
          dl_button(ns("dl_csv"), "Report CSV"),
          dl_button(ns("dl_xlsx"), "Report XLSX"),
          dl_button(ns("dl_raw_csv"), "Raw Data CSV"),
          dl_button(ns("dl_raw_xlsx"), "Raw Data XLSX"),
          dl_button(ns("dl_all_xlsx"), "All Limits XLSX"),
          dl_button(ns("dl_all_csv"), "All Limits CSV")
        ),
      br3(),
      gt::gt_output(ns("table"))
      )
  ))
}

# Module Server

#' @rdname mod_data
#' @keywords internal

mod_data_server <- function(input, output, session) {
  ns <- session$ns
  observe({
    limits_bcdc <-  bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")
    
    # if guidelines aren't valid, fall back on internal data
    limits_bcdc <- try(check_guidelines(limits_bcdc), silent = TRUE)
    if(is_try_error(limits_bcdc)){
      waiter::waiter_update(html = waiter_html("Guidelines on BC Data Catalogue are not valid. 
                                               Using guidelines from March 24, 2020."))
      Sys.sleep(3)
      limits <- limits
    } else {
      limits <- limits_bcdc
    }
    rv$limits <- process_limits(limits)
    waiter::waiter_hide()
  })
  
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
    req(wqg_data_evaluate())
    x <- wqg_data_evaluate()
    x <- x[x$ConditionPass,]
    if(any(is.na(suppressWarnings(as.numeric(x$Limit)))) & all(!is.na(x$Limit))){
      return(
        tagList(
          tags$label("Guideline Significant Figures"),
          help_text("Only applies to guidelines calculated from equations"),
          numericInput(ns("sigfig"), label = NULL, value = 2)
        ))
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
    x <- x[rv$cvalue_active]
    x
  })
  
  wqg_data_raw <- reactive({
    req(input$variable)
    req(input$use)
    wqg_filter(input$variable, 
               input$use, input$media, rv$limits)
  })

  wqg_data_evaluate <- reactive({
    req(wqg_data_raw())
    if(any(is.na(clean_cvalues()))) return()
    x <-  wqg_data_raw()
    if(nrow(x) == 0) return()
    x %>%
      wqg_evaluate(cvalues = clean_cvalues())
  })

  wqg_data_report <- reactive({
    req(wqg_data_evaluate())
    x <-  wqg_data_evaluate()
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
    req(input$use)
    get_combinations(input$variable, input$use, rv$limits)
  })

  rv <- reactiveValues(
    path = "inst/extdata/",
    cvalue_active = NULL,
    cvalue_inactive = NULL,
    raw = empty_evaluate,
    report = empty_report,
    limits = NULL
  )

  observe({
    if(is.null(input$use) | input$variable == "") {
      rv$raw <- empty_evaluate
      rv$report <- empty_report
    }
    req(wqg_data_evaluate())
    data <- wqg_data_evaluate()
    rv$raw <- data
    if(any(data$ConditionPass)) {
      rv$report <- wqg_data_report()
    }
  })

  observe({
    req(wqg_data_raw())
    data <- wqg_data_raw()
    cval <- unique(c(extract_codes(data$Condition), extract_codes(data$Limit)))
    rv$cvalue_active <- cval
    rv$cvalue_inactive <- setdiff(cvalue_codes, cval)
  })

  output$ui_use <- renderUI({
    uses <- variable_use(input$variable, rv$limits)
    select_input_x(ns("use"),
      label = "Select Value(s)",
      choices = uses,
      selected = "")
  })

  output$ui_media <- renderUI({
    x <- combinations()$media
    checkboxGroupInput(ns("media"), "Select Media",
      choices = x,
      selected = x,
      inline = TRUE)
  })

  output$table <- gt::render_gt({
    req(wqg_data_report())
    x <- wqg_data_report()
    if(is.null(x)) return()
    if(nrow(x) == 0) return()
    cvalues <- report_cvalues(cvalues(), rv$cvalue_active)
    gt_table(x, cvalues)
  })

  output$dl_raw_csv <- downloadHandler(
    filename = function() "wqg_data_raw.csv",
    content = function(file) {
      readr::write_csv(rv$raw, file)
    })

  output$dl_raw_xlsx <- downloadHandler(
    filename = function() "wqg_data_raw.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(rv$raw, file)
    })

  output$dl_csv <- downloadHandler(
    filename = function() "wqg_data_report.csv",
    content = function(file) {
      readr::write_csv(rv$report, file)
    })

  output$dl_xlsx <- downloadHandler(
    filename = function() "wqg_data_report.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(rv$report, file)
    })
  
  output$dl_all_xlsx <- downloadHandler(
    filename = function() "all_wqgs.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(rv$iimits, file)
    })
  
  output$dl_all_csv <- downloadHandler(
    filename = function() "all_wqgs.csv",
    content = function(file) {
      readr::write_csv(rv$limits, file)
    })

  output$dl_html <- downloadHandler(
    filename = "wqg_report.html",
    content = function(file) {
      x <- wqg_data_report()
      cvalues <- report_cvalues(cvalues(), rv$cvalue_active)
      gt <- gt_table(x, cvalues)
      gt::gtsave(gt, file)
    }
  )

  output$dl_pdf <- downloadHandler(
    filename = "wqg_report.pdf",
    content = function(file) {
      x <- wqg_data_report()
      cvalues <- report_cvalues(cvalues(), rv$cvalue_active)
      gt <- gt_table(x, cvalues)
      gt::gtsave(gt, file, zoom = 1.3, expand = 5)
    }
  )

  output$dl_rmd <- downloadHandler(
    filename = "wqg_report.Rmd",
    content = function(file) {
      file.copy(system.file(package = "shinywqg", "extdata/report_html.Rmd"), file)
    }
  )
}

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
                 uiOutput(ns("ui_variable")),
                 uiOutput(ns("ui_use")),
                 uiOutput(ns("ui_media")),
                 uiOutput(ns("ui_cvalue")),
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
    
    file_name <- "85d3990a-ec0a-4436-8ebd-150de3ba0747"
    
    #### To test with new data before uploading to BCDC: 
    ## - save the new `all-wqgs.csv` in inst/app
    ## - comment out the next two lines
    ## - uncomment the third line (limits <- readr::read_csv(...))
    ## - run pkgload::load_all(); run_wqg_app()
    limit_resource <- "6f32a85b-a3d9-44c3-9a14-15175eba25b6"
    limits <- get_data(file_name, resource = limit_resource)
    # limits <- readr::read_csv("all-wqgs-2.csv", show_col_types = FALSE)
    
    limits <- try(process_limits(limits))
    if (is_try_error(limits)) {
      waiter::waiter_update(
        html = waiter_html(
          "Issue with Guidelines from BC Data Catalogue.
          Using internal guidelines which may not be most recent version."
        )
      )
      Sys.sleep(5)
      internal_data <- internal_tables[[file_name]]
    } else {
      limits <- limits
    }
    limits <- process_lookups(limits)
    cvalue_codes <- unique(c(extract_codes(limits$Limit),
                             extract_codes(limits$Condition))) %>%
      setdiff("EMS_1107")
    
    rv$cvalue_codes <- cvalue_codes
    
    rv$limits <- limits
    waiter::waiter_hide()
  })
  
  observe({
    req(input$variable)
    if(input$variable == "" | is.null(input$use)) {
      return({
        for(i in rv$cvalue_codes) {
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
      if(any(stringr::str_detect(x$Limit, "\\.csv$"))) {
        return()
      }
      return(
        tagList(
          tags$label("Guideline Significant Figures"),
          help_text("Only applies to guidelines calculated from equations"),
          numericInput(ns("sigfig"), label = NULL, value = 3)
        ))
    }
  })
  
  cvalues <- reactive({
    x <- rv$cvalue_codes
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
    x <- wqg_filter(input$variable, 
                    input$use, input$media, rv$limits)
    x
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
    sigfig <- 3
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
    limits = NULL,
    cvalue_codes = NULL,
    filtered = NULL,
    lookup_vars = NULL
  )
  
  observe({
    req(input$variable)
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
    rv$cvalue_inactive <- setdiff(rv$cvalue_codes, cval)
  })
  
  output$ui_variable <- renderUI({
    selectizeInput(ns("variable"),
                   label = "Select Variable",
                   choices = c(sort(unique(rv$limits$Variable)), ""),
                   selected = "",
                   multiple = FALSE)
  })
  
  observe({
    rv$lookup_vars <- lookup_variables(rv$limits)
  })
  
  wqg_data_variable <- reactive({
    req(input$variable)
    x <- wqg_filter_variable(input$variable, rv$limits)
    x
  })
  
  observe({
    req(input$variable)
    filtered_data <-   wqg_data_variable()
    rv$filtered <- lookup_choices(filtered_data, rv$cvalue_codes)
  })
  
  output$ui_cvalue <- renderUI({
    req(input$variable)
    if(input$variable %in% rv$lookup_vars){
      shinyjs::hidden(dropdown_inputs(rv$cvalue_codes, ns, rv$filtered))
    } else {
      shinyjs::hidden(numeric_inputs(rv$cvalue_codes, ns))
    }
  })
  
  output$ui_use <- renderUI({
    req(input$variable)
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
      readr::write_csv(
        remove_list_columns(rv$raw),
        file
      )
    })
  
  output$dl_raw_xlsx <- downloadHandler(
    filename = function() "wqg_data_raw.xlsx",
    content = function(file) {
      openxlsx2::write_xlsx(
        remove_list_columns(rv$raw),
        file
      )
    })
  
  output$dl_csv <- downloadHandler(
    filename = function() "wqg_data_report.csv",
    content = function(file) {
      readr::write_csv(rv$report, file)
    })
  
  output$dl_xlsx <- downloadHandler(
    filename = function() "wqg_data_report.xlsx",
    content = function(file) {
      openxlsx2::write_xlsx(rv$report, file)
    })
  
  output$dl_all_xlsx <- downloadHandler(
    filename = function() "all_wqgs.xlsx",
    content = function(file) {
      openxlsx2::write_xlsx(
        remove_list_columns(rv$limits),
        file
      )
    })
  
  output$dl_all_csv <- downloadHandler(
    filename = function() "all_wqgs.csv",
    content = function(file) {
      readr::write_csv(
        remove_list_columns(rv$limits),
        file
      )
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
      file.copy(system.file(package = "shinywqg", "extdata/report_html.Rmd"), 
                file)
    }
  )
}


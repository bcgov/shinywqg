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
      uiOutput(ns("ui_rm_missing")),
      shinyjs::hidden(button(ns("get"), "Get/Update Guidelines"))
    ),
    mainPanel(
      uiOutput(ns("ui_dl")),
      br2(),
      gt::gt_output(ns("table"))
    )
  )
}

# Module Server

#' @rdname mod_data
#' @keywords internal

mod_data_server <- function(input, output, session) {
  ns <- session$ns
  
  observe({
    use <- input$use
    if(!is.null(use)){
      if(use != ""){
        shinyjs::show("get")
      } else {
        shinyjs::hide("get")
      }
    } else {
      shinyjs::hide("get")
    }
  })
  
  observeEvent(input$info_missing, {
    shinyjs::toggle("div_info_missing", anim = TRUE)
  })
  
  cvalues <- reactive({
    code_values(ph = input$EMS_0004, 
                hardness = input$EMS_0107, 
                methyl_mercury = input$EMS_HGME, 
                chloride = input$EMS_0104,
                total_mercury = input$EMS_HG_T)
  })
  
  params_rv <- reactiveValues(data = NULL,
                              table = NULL,
                              refs = NULL,
                              cvalues = NULL,
                              use = NULL)
  
  observeEvent(input$get, {
    suppressWarnings(waiter::show_butler())
    x <- wqg_table(variable = input$variable,
                   use = input$use,
                   term = input$term,
                   cvalues = cvalues())
    
    cvalues <- clean_cvalues(cvalues(), input$variable, input$use)

    params_rv$use <- input$use
    params_rv$data <- x
    params_rv$refs <- get_refs(x)
    params_rv$cvalues <- cvalues
    
    y <- x %>%
      filter_missing(input$rm_missing, input$variable, input$term, input$use) %>%
      clean_table()
    
    params_rv$table <- y
    suppressWarnings(waiter::hide_butler())
  })
  
  output$table <- gt::render_gt({
    req(params_rv$table)
    gt_table(params_rv$table, params_rv$use, params_rv$cvalues)
  })
  
  output$ui_use <- renderUI({
    req(input$variable)
    selectInput(ns("use"), label = "Select Use(s)",
                choices = c(get_use(input$variable), ""),
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
  
  output$ui_dl <- renderUI({
    req(params_rv$table)
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
                                   label = "Data",
                                   size = "sm",
                                   inline = TRUE, 
                                   circle = FALSE,
                                   icon = icon("download"),
                                   dl_button(ns("dl_csv"), "CSV"),
                                   dl_button(ns("dl_excel"), "Excel")
      ),
      dl_button(ns("dl_refs"), "References")
    )
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() "wqg_table.csv",
    content = function(file) {
      readr::write_csv(params_rv$data, file)
    })
  
  output$dl_excel <- downloadHandler(
    filename = function() "wqg_table.xlsx",
    content = function(file) {
      openxlsx::write.xlsx(params_rv$data, file)
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
  
  output$dl_refs <- downloadHandler(
      filename = "refs.zip",
      content = function(fname) {
        tmpdir <- tempdir()
        setwd(tempdir())
        files <- paste0(params_rv$refs, ".pdf")
        for(i in files){
          file.copy(system.file(package = "shinywqg", file.path("extdata/", i)), i)
        }
        utils::zip(zipfile = fname, files = files)
      },
      contentType = "application/zip"
  )
}

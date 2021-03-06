#' @title   mod_about_ui and mod_about_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about
#'
#' @keywords internal

mod_about_ui <- function(id, label = "Mod_about"){
  ns <- NS(id)
  tagList(
    wellPanel(
      includeMarkdown(system.file(package = "shinywqg", "extdata/about.md"))
    )
  )
}
    
#' @rdname mod_about
#' @keywords internal
mod_about_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_about_ui("about_ui_1")
    
## To be copied in the server
# callModule(mod_about_server, "about_ui_1")
 

# Module UI
  
#' @title   mod_guide_ui and mod_guide_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_guide
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_guide_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      includeMarkdown(system.file(package = "shinywqg", "extdata/guide.md"))
    )
  )
}
    
# Module Server
    
#' @rdname mod_guide
#' @export
#' @keywords internal
    
mod_guide_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_guide_ui("guide_ui_1")
    
## To be copied in the server
# callModule(mod_guide_server, "guide_ui_1")
 

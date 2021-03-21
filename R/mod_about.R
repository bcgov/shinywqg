# Module UI

mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      includeMarkdown(system.file(package = "shinywqg", "extdata/about.md"))
    )
  )
}
    
# Module Server

mod_about_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_about_ui("about_ui_1")
    
## To be copied in the server
# callModule(mod_about_server, "about_ui_1")
 

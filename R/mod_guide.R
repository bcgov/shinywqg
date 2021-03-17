# Module UI
  
mod_guide_ui <- function(id, label = "mod_guide"){
  ns <- NS(id)
  tagList(
    wellPanel(
      includeMarkdown("extdata/guide.md")
    )
  )
}
    
# Module Server
    
mod_guide_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_guide_ui("guide_ui_1")
    
## To be copied in the server
# callModule(mod_guide_server, "guide_ui_1")
 

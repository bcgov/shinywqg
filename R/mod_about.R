# Module UI

mod_about_ui <- function(id, label = "Mod_about"){
  ns <- NS(id)
  tagList(
    wellPanel(
      includeMarkdown("extdata/about.md")
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
 

app_server <- function(input, output, session) {

  callModule(mod_data_server, "data_ui_1")
}

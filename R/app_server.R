app_server <- function(input, output, session) {
  setup_chromote()
  callModule(mod_data_server, "data_ui_1")
}

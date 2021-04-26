#' Run Shiny Application
#'
#' @export
run_wqg_app <- function() {
  
  shiny::shinyAppDir(system.file("app", package = "shinywqg"))
  
}

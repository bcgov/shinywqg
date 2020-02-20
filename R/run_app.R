#' Run Shiny Application
#'
#' @param deploy A flag indicating whether function is being used to deploy app to shinyapps.io.
#' @export
run_wqg_app <- function(deploy = FALSE) {
  chk::chk_flag(deploy)
  shinyOptions(deploy = deploy)
  
  shiny::shinyAppDir(system.file("app", package = "shinywqg"), 
                     options = c("launch.browser" = TRUE))
  
}
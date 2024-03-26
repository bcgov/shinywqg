#' @import chk lifecycle magrittr
#' @importFrom dplyr %>%
#' @importFrom utils zip
#' @importFrom rlang .data
#' @rawNamespace import(shiny, except = p)
NULL

ignore_unused_imports <- function() {
  # webshot to capture gt table for pdf output
  webshot2::webshot
  # we don't actually use pagedown, but include it to force install chrome on shinyapps.io
  # so that webshot2 works.
  # see also R/utils_shiny.R; setup_chromote()
  # https://forum.posit.co/t/how-to-properly-configure-google-chrome-on-shinyapps-io-because-of-webshot2/109020/5
  pagedown::find_chrome
}

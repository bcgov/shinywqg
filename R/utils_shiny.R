txt_input <- function(..., width = "100%") shiny::textInput(..., width = width)

br2 <- function() tagList(br(), br())
br3 <- function() tagList(br(), br(), br())

help_text <- function(x) {
  tags$p(x, style = "font-size: 11px; color: grey;")
}

dl_button <- function(..., icon = "download", class = "small-dl") {
  downloadButton(..., icon = icon(icon), class = class)
}

button <- function(..., icon = NULL, class = "btn-primary") {
  actionButton(..., icon = icon(icon), class = class)
}

select_input_x <- function(..., label = "Select sites:", choices, selected = choices[1]) {
  selectizeInput(..., multiple = TRUE, label = label,
    choices = choices,
    selected = selected,
    options = list(
      "plugins" = list("remove_button"),
      "create" = TRUE,
      "persist" = FALSE))
}

table_output <- function(...) {
  wellPanel(gt::gt_output(...), style = "font-size:87%", class = "wellpanel")
}

hide <- function(id, anim = TRUE) {
  shinyjs::hide(id, anim = anim)
}

show <- function(id, anim = TRUE) {
  shinyjs::show(id, anim = anim)
}

embed_help <- function(tag, id, ns, help) {
  element <- div(shiny::actionLink(ns(id), shiny::icon("info-circle")),
    class = "pull-right")
  tag$children[[1]] <- tag$children[[1]] %>%
    htmltools::tagAppendChild(element) %>%
    htmltools::tagAppendAttributes(style = "width:100%;")
  tagList(tag,
    shinyjs::hidden(div(id = ns(paste0("div_", id)),
      help_text(help))))
}

css_add <- function(x) {
  shiny::tags$head(shiny::tags$style(shiny::HTML(x)))
}

css_hide_errors <- function() {
  css_add("
.shiny-output-error {visibility: hidden;}
.shiny-output-error:before {visibility: hidden;}
")
}

css_button <- function() {
  css_add(".small-dl{
  padding:4px; font-size:85%;
}")
}

css_navbar <- function(text_color = "#E0E0E0",
                       text_selected_color = "#5bc0de",
                       text_size = "15px",
                       background_color = "#010101",
                       float = "right") {
  css_add(paste0("
.navbar .navbar-nav {float: ", float, ";}

.navbar-header {padding: 0px 20px 0px 30px;}

.navbar.navbar-default.navbar-static-top{
  padding:0;
  margin-bottom: 0px;
  background: ", background_color, "2;
}

.navbar-default {
    box-shadow: 0 4px 4px -2px rgba(0,0,0,.2);
}

.navbar-default {
  background-color: ", background_color, ";
  border-color: transparent;
  font-size: ", text_size, ";
}
.navbar-default .navbar-brand {
  color: ", text_color, ";
}
.navbar-default .navbar-brand:hover,
.navbar-default .navbar-brand:focus {
  color: ", text_color, ";
  background-color: transparent;
}
.navbar-default .navbar-text {
  color: ", text_color, ";
  font-size: 20px !important;
}
.navbar-default .navbar-nav > li > a {
  color:  ", text_color, ";
}
.navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > li > a:focus {
  color: ", text_selected_color, ";
  background-color: transparent;
}
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus {
  color: ", text_selected_color, ";
  background-color: transparent;
}
.navbar-default .navbar-nav > .disabled > a,
.navbar-default .navbar-nav > .disabled > a:hover,
.navbar-default .navbar-nav > .disabled > a:focus {
  color: ", background_color, ";
  background-color: transparent;
}
.navbar-default .navbar-toggle {
  border-color: ", background_color, ";
}
.navbar-default .navbar-toggle:hover,
.navbar-default .navbar-toggle:focus {
  background-color: ", text_selected_color, ";
}
.navbar-default .navbar-toggle .icon-bar {
  background-color: ", text_color, ";
}
.navbar-default .navbar-collapse,
.navbar-default .navbar-form {
  border-color: transparent;
}
.navbar-default .navbar-nav > .open > a,
.navbar-default .navbar-nav > .open > a:hover,
.navbar-default .navbar-nav > .open > a:focus {
  background-color: ", background_color, ";
  color:", text_selected_color, ";
}
@media (max-width: 767px) {
  .navbar-default .navbar-nav .open .dropdown-menu > li > a {
    color: ", text_color, ";
  }
  .navbar-default .navbar-nav .open .dropdown-menu > li > a:hover,
  .navbar-default .navbar-nav .open .dropdown-menu > li > a:focus {
    color: ", background_color, ";
    background-color: transparent;
  }
  .navbar-default .navbar-nav .open .dropdown-menu > .active > a,
  .navbar-default .navbar-nav .open .dropdown-menu > .active > a:hover,
  .navbar-default .navbar-nav .open .dropdown-menu > .active > a:focus {
    color: ", text_color, ";
    background-color: ", background_color, ";
  }
  .navbar-default .navbar-nav .open .dropdown-menu > .disabled > a,
  .navbar-default .navbar-nav .open .dropdown-menu > .disabled > a:hover,
  .navbar-default .navbar-nav .open .dropdown-menu > .disabled > a:focus {
    color: #cccccc;
    background-color: transparent;
  }
}
.navbar-default .btn-link {
  color: ", text_color, ";
}"))
}

css_body <- function (font_family = "Myriad-Pro, Calibri, Arial, 'sans serif'", 
                      font_weight = "100", font_size_body = "10px", font_size_label = "13px", 
                      text_color = "#494949") {
  css_add(paste0("\nbody, label, input, button, select {{ \n  font-family: ", font_family, ";\n  font-weight: ", font_weight, ";\n  color: ", text_color, ";\n    font-size: ", font_size_body, ";\n}}\nlabel {{\n    font-size: ", font_size_label, ";\n}}\nh1, h2, h3, h4 {{font-weight: ", font_weight, ";}}\nbutton {{background-color: #999999;}}\n"))
}

setup_chromote <- function() {
  # This is a workaround to ensure that the chromote package is working on shinyapps.io
  # It is needed to enable webshot2, the package that produces the pdf output 
  # of the guideline report
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c("--disable-gpu", 
                 "--no-sandbox", 
                 "--disable-dev-shm-usage", # required bc the target easily crashes
                 c("--force-color-profile", "srgb"))
      ))
    )
  }
}

pkgload::load_all(".")
if (is.null(suppressMessages(webshot:::find_phantom()))){
  webshot::install_phantomjs()
}
shinywqg::run_wqg_app()


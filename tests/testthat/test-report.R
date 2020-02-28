test_that("report renders", {

  teardown(unlink(file.path(tempdir(), "shinywqg")))
  path <- file.path(tempdir(), "shinywqg")
  unlink(path, recursive = TRUE)
  dir.create(path)

  ### test LimitNotes
  x <- wqg_filter("Chloride Dissolved", "Aquatic Life - Marine", "Water",
    "Long-term chronic", "No Effect", "mean") %>%
    wqg_evaluate(cvalues, 2) %>%
    wqg_clean()

  cvalues <- report_cvalues(list(EMS_0107 = 9, EMS_1107 = 5), c("EMS_0107", "EMS_1107"))
  note <- get_footnotes(x)
  
  # params <- list(data = x,
  #   cvalues = cvalues)
  
  # y <- rmarkdown::render(system.file("extdata", package = "shinywqg", "report_html.Rmd"),
  #   output_file = file.path(path, "report.html"),
  #   params = params, output_format = rmarkdown::html_document(),
  #   envir = new.env(parent = globalenv()))
  
  y <- gt_table(x, cvalues, note)
  expect_is(y, "gt_tbl")
  y <- gt::gtsave(y, "report.html", path)
  system2("open", args = file.path(path, "report.html"), wait = FALSE)

  cvalues <- report_cvalues(list(EMS_0107 = 9, EMS_1107 = 5),
    c("EMS_0107", "EMS_1107"), "pdf")

  y <- gt_table(x, cvalues, note)
  expect_is(y, "gt_tbl")
  y <- gt::gtsave(y, "report.pdf", path, zoom = 1.3, expand = 5)
  system2("open", args = file.path(path, "report.pdf"), wait = FALSE)
})

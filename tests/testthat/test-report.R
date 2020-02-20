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
    
  params <- list(data = x,
                 cvalues = cvalues,
                 output = "html")
  
  options(tinytex.verbose = TRUE)
  
  y <- rmarkdown::render(system.file("extdata", package = "shinywqg", "report_html.Rmd"),
                         output_file = file.path(path, "report.html"),
                    params = params, output_format = rmarkdown::html_document(),
                    envir = new.env(parent = globalenv()))
  
  expect_is(y, "character")
  system2("open", args = file.path(path, "report.html"), wait = FALSE)
  
  # params <- list(data = mtcars)
  params$output <- "pdf"
  params$cvalues <- report_cvalues(list(EMS_0107 = 9, EMS_1107 = 5), 
                                   c("EMS_0107", "EMS_1107"), "pdf")
  
  y <- rmarkdown::render(system.file("extdata", package = "shinywqg", "report_pdf.Rmd"),
                         output_file = file.path(path, "report.pdf"),
                         params = params, output_format = rmarkdown::pdf_document(),
                         envir = new.env(parent = globalenv()))
  
  system2("open", args = file.path(path, "report.pdf"), wait = FALSE)
  
  expect_is(y, "character")
  
})

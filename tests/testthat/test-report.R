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
  
  cvalues <- function(){
    list(EMS_0107 = 9, EMS_1107 = 5)
  }
  
  params <- list(data = x,
                 cvalue_active = names(cvalues),
                 cvalues = cvalues)
  
  options(tinytex.verbose = TRUE)
  
  y <- rmarkdown::render(system.file("extdata", package = "shinywqg", "report.Rmd"),
                         output_file = file.path(path, "report.html"),
                    params = params, output_format = rmarkdown::html_document(),
                    envir = new.env(parent = globalenv()))
  
  expect_is(y, "character")
  
  y <- rmarkdown::render(system.file("extdata", package = "shinywqg", "report.Rmd"),
                         output_file = file.path(path, "report.pdf"),
                         params = params, output_format = rmarkdown::pdf_document(),
                         envir = new.env(parent = globalenv()))
  
  expect_is(y, "character")
  
})

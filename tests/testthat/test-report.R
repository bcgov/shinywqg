test_that("report renders", {

  teardown(unlink(file.path(tempdir(), "shinywqg")))
  path <- file.path(tempdir(), "shinywqg")
  unlink(path, recursive = TRUE)
  dir.create(path)

  cvalues <- list(EMS_0107 = 9, EMS_1104 = 10)
  ### test LimitNotes
  x <- wqg_filter("Chloride", "Aquatic Life - Marine", "Water") %>%
    wqg_evaluate(cvalues) %>%
    wqg_clean(2)

  cvalues <- report_cvalues(cvalues, c("EMS_0107", "EMS_1104"))
  
  y <- gt_table(x, cvalues)
  expect_is(y, "gt_tbl")
  y <- gt::gtsave(y, "report.html", path)
  system2("open", args = file.path(path, "report.html"), wait = FALSE)

  y <- gt_table(x, cvalues)
  expect_is(y, "gt_tbl")
  y <- gt::gtsave(y, "report.pdf", path, zoom = 1.3, expand = 5)
  system2("open", args = file.path(path, "report.pdf"), wait = FALSE)
})

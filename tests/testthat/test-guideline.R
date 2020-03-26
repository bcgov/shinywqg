test_that("guidelines on DataBC are valid", {
  x <-  bcdata::bcdc_get_data(record = "85d3990a-ec0a-4436-8ebd-150de3ba0747")
  x <- process_limits(x)
  y <- check_guidelines(x)
  expect_identical(x, y)
})

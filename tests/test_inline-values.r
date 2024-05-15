test_that("Inline Values are as exepected", {

  inline_values <- readRDS(here::here("./data/paper-figures/inline-values/inline-values.rds"))

  expect_equal(inline_values$vfciUnemploymentMaxShareCorrelation, 0.75)
  expect_equal(inline_values$vfciOutputMaxShareCorrelation, 0.57)
  expect_equal(inline_values$vfciLaborShareMaxShareCorrelation, 0.2)

  expect_equal(inline_values$dataStartDate, zoo::as.yearqtr("1962 Q1"))
  expect_equal(inline_values$dataEndDate, zoo::as.yearqtr("2017 Q1"))

  expect_equal(inline_values$vfciFEVDFD, 83)

})

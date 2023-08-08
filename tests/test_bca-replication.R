test_that("Replicated Classical VAR IRFs", {
  require(data.table)
  require(dplyr)

  df <-
    fread(here::here("./data/replicated_bca_classical_VAR_IRF.csv")) |>
    mutate(
        value = -1 * value,
        median = -1 * median
    ) |>
    mutate(
        variable = factor(variable, levels = c("unemployment", "output", "hours_worked", "investment", "consumption", "TFP", "productivity", "labor_share", "inflation", "interest"), ordered = T)
    ) |>
    dplyr::select(h, variable, value, model) |>
    tidyr::pivot_wider(names_from = model, values_from = value)

  expect_equal(df$Replication, df$classical_fd, tolerance = 0.05)

})
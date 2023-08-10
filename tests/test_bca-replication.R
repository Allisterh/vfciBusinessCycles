test_that("Replicated Classical VAR IRFs", {
  require(data.table)
  require(dplyr)

  df <-
    fread(here::here("./data/replicated_bca_classical_VAR_IRF.csv")) |>
    dplyr::select(h, variable, value, model) |>
    tidyr::pivot_wider(names_from = model, values_from = value)

  expect_equal(df$Replication, df$classical_fd, tolerance = 0.075)

})

test_that("Replicated Bayesian VAR IRFs", {
  require(data.table)
  require(dplyr)

  df <-
    fread(here::here("./data/replicated_bca_bayesian_VAR_IRF_bvartools.csv")) |>
    dplyr::filter(
      (model == "bayesian_fd" & version == "original") |
        (model == "bayesian_fd_exact")
      ) |>
    dplyr::select(h, variable, median, version) |>
    tidyr::pivot_wider(names_from = version, values_from = median)

  expect_equal(df$replication, df$original, tolerance = 0.075)

})
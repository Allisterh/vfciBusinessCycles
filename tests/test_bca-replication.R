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
    dplyr::select(model, version, h, variable, median)
  setorder(df, model, version, h, variable)

  for (var in unique(df$variable)) {

    expect_equal(
      df[variable == var & model == "bayesian_fd" & version == "replication", median],
      df[variable == var & model == "bayesian_fd" & version == "original", median],
      label = paste("Bayesian FD", var),
      tolerance = 0.1
    ) # Diff 0.0355

    expect_equal(
      df[variable == var & model == "bayesian_td4" & version == "replication", median],
      df[variable == var & model == "bayesian_td4" & version == "original", median],
      label = paste("Bayesian TD4", var),
      tolerance = 0.125
    ) # Diff 0.044

    expect_equal(
      df[variable == var & model == "bayesian_td32" & version == "replication", median],
      df[variable == var & model == "bayesian_td632" & version == "original", median],
      label = paste("Bayesian TD32", var),
      tolerance = 0.1
    ) # Diff 0.0427

    expect_equal(
        df[variable == var & model == "bayesian_td632" & version == "replication", median],
        df[variable == var & model == "bayesian_td632" & version == "original", median],
        label = paste("Bayesian TD632", var),
        tolerance = 0.1
    ) # Diff 0.0462

  }


})


test_that("Replicated BCA Minnesota Priors", {
  require(data.table)
  source(here::here("./code/helpers/bca_mn_priors.R"))

  bcadata <- fread(here::here("./data/bca_replication_data.csv"))[
    date <= as.Date("2017-10-01"), ]
  data <- ts(bcadata[, -"date"], start = year(bcadata[[1, 1]]), frequency = 4)

  priors <- bca_mn_priors(data, lags = 2, 0.2, 0.5, 2, 10^5)

  ## Values to expect, from BCA code
  se <- c(0.856, 3.165, 0.520, 0.764, 0.350, 0.967, 0.217, 0.287, 0.805, 0.768)
  v_i <- c(25.000, 7.318, 270.483, 125.450, 595.203, 78.291, 1547.684, 889.827,
  113.125, 124.150, 1366.560, 25.000, 3696.312, 1714.354, 8133.803, 1069.889,
  21150.018, 12160.021, 1545.924, 1696.581, 36.971, 2.705, 25.000, 46.380,
  220.052, 28.945, 572.192, 328.977, 41.823, 45.899, 79.713, 5.833,
  215.610, 25.000, 474.453, 62.408, 1233.702, 709.306, 90.175, 98.963,
  16.801, 1.229, 45.444, 21.077, 25.000, 13.154, 260.026, 149.500,
  19.006, 20.858, 127.729, 9.347, 345.486, 160.237, 760.248, 25.000,
  1976.843, 1136.569, 144.494, 158.575, 6.461, 0.473, 17.477, 8.106,
  38.458, 5.059, 25.000, 57.494, 7.309, 8.022, 11.238, 0.822,
  30.397, 14.098, 66.890, 8.798, 173.931, 25.000, 12.713, 13.952,
  88.398, 6.469, 239.101, 110.895, 526.145, 69.207, 1368.115, 786.586,
  25.000, 109.745, 80.548, 5.894, 217.868, 101.048, 479.423, 63.061,
  1246.626, 716.737, 91.120, 25.000, 400.000, 117.082, 4327.729, 2007.206,
  9523.248, 1252.651, 24762.938, 14237.238, 1810.004, 1986.397, 21864.953,
  400.000, 59140.998, 27429.665, 130140.853, 17118.217, 338400.296,
  194560.333, 24734.777, 27145.292,
  591.534, 43.286, 400.000, 742.082, 3520.829, 463.116, 9155.078, 5263.633,
  669.174, 734.388, 1275.405, 93.330, 3449.754, 400.000, 7591.247, 998.523,
  19739.230, 11348.900, 1442.804, 1583.412, 268.816, 19.671, 727.101, 337.230,
  400.000, 210.458, 4160.419, 2391.997, 304.099, 333.734, 2043.666, 149.548,
  5527.772, 2563.787, 12163.963, 400.000, 31629.489, 18185.102, 2311.902,
  2537.207, 103.380, 7.565, 279.626, 129.691, 615.323, 80.937, 400.000, 919.906,
  116.949, 128.346, 179.810, 13.158, 486.356, 225.573, 1070.235, 140.775,
  2782.892, 400.000, 203.411, 223.234, 1414.362, 103.498, 3825.609, 1774.322,
  8418.324, 1107.313, 21889.847, 12585.379, 400.000, 1755.927, 1288.766, 94.307,
  3485.893, 1616.762, 7670.773, 1008.983, 19946.018, 11467.791, 1457.919,
  400.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000)

  expect_equal(priors$mu[seq(1,100,11)], rep(1,10))

  expect_equal(diag(priors$sigma_i), se, tolerance = 0.001)

  expect_equal(diag(priors$v_i), v_i, tolerance = 0.0001)

})

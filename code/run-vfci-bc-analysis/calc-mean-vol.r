library(data.table)
library(vfciBCHelpers)

## Settings
lags <- 2

## Fit the VAR
data <- get_var_data()
var <- fit_var(data, lags = lags)

data[, t := .I - lags]

het_reg <- fit_het_reg_from_var(var, lags = 0)

## Save out to Disk
het_reg$dt |>
  merge(data[, .(date, t)], by = "t") |>
  _[, .(date, variable, fitted, log_var_fitted)] |>
  fwrite("./data/paper-figures/charts/mean-vol-estimate-var.csv")

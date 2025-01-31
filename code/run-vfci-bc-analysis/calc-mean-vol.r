library(data.table)
library(vfciBCHelpers)


## Settings
lags <- 2
end_date <- as.Date("2024-07-01")
make_stationary <- TRUE
cumsum <- FALSE
hetreg_lags <- 0
hetreg_horizon <- 12

## Macro VFCI
data <- get_var_data(make_stationary = make_stationary, vfci = NULL, end_date = end_date)

var <- fit_var(data, lags = lags)

data[, t := .I - lags]

het_reg <- fit_het_reg_from_var(var, hetreg_horizon = hetreg_horizon, cumsum = cumsum, lags = hetreg_lags)

## Save out to Disk
het_reg$dt |>
  merge(data[, .(date, t)], by = "t") |>
  _[, .(date, variable, fitted, log_var_fitted)] |>
  fwrite("./data/paper-figures/charts/mean-vol-estimate-macro-var.csv")


## Financial VFCI
fin_cols <- c("pc1", "pc2", "pc3", "pc4")

data <- get_var_data(
  make_stationary = make_stationary,
  vfci = NULL,
  end_date = end_date,
  add_cols = fin_cols
)

var <- fit_var(data[, -fin_cols, with = FALSE], lags = lags)

data[, t := .I - lags]

het_reg <-
  fit_het_reg_from_var(
    var,
    hetreg_horizon = hetreg_horizon,
    cumsum = cumsum,
    x2 = fin_cols,
    extra_data = data[, ..fin_cols]
  )


## Save out to Disk
het_reg$dt |>
  merge(data[, .(date, t)], by = "t") |>
  _[, .(date, variable, fitted, log_var_fitted)] |>
  fwrite("./data/paper-figures/charts/mean-vol-estimate-fin-var.csv")

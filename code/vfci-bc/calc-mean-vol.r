library(data.table)
library(purrr)
library(vars)
library(svars)
library(fevdid)

data <-
  fread(here::here("./data/vfciBC_data.csv")) |>
  _[date <= as.Date("2017-01-01")]

data <-
  data[, .(
    date,
    output,
    investment,
    consumption,
    hours_worked,
    unemployment,
    labor_share,
    interest,
    inflation,
    productivity,
    TFP,
    vfci = vfci_fgr10gdpc1
  )]

lags <- 2

## Fit the VAR
v <- VAR(data[, -"date"], p = lags, type = "const")

## Mean-Vol

data[, t := seq_len(nrow(data)) - lags]

data <- data |>
  dt_pivot_longer(-c(date, t), names_to = "variable", values_to = "original")

resid_data <-
  residuals(v) |>
  as.data.table() |>
  _[, t := .I] |>
  dt_pivot_longer(-t, names_to = "variable", values_to = "residuals")

fitted_data <-
  fitted(v) |>
  as.data.table() |>
  _[, t := .I] |>
  dt_pivot_longer(-t, names_to = "variable", values_to = "fitted")

data <- data |>
  merge(resid_data, by = c("t", "variable")) |>
  merge(fitted_data, by = c("t", "variable"))

data[, log_var := log(residuals^2)]

reg_data <- data |>
  merge(
    dt_pivot_wider(data[,.(t, variable, original)], names_from = variable, values_from = original),
    by = c("t"),
    allow.cartesian = TRUE
  )

fitted_log_var_data <-
  rbindlist(lapply(unique(reg_data$variable), function(n) {
    het_model <- lm(formula = "log_var ~ 
    lag(output) + lag(investment) + lag(consumption) + lag(hours_worked) + lag(unemployment) + 
    lag(labor_share) + lag(interest) + lag(inflation) + lag(productivity) + lag(TFP) + lag(vfci) +
    lag(output, 2) + lag(investment, 2) + lag(consumption, 2) + lag(hours_worked, 2) + lag(unemployment, 2) + 
    lag(labor_share, 2) + lag(interest, 2) + lag(inflation, 2) + lag(productivity, 2) + lag(TFP, 2) + lag(vfci, 2)
    ", data = reg_data[variable == n])
    fitted(het_model) |>
      as.data.table() |>
      setnames("V1", "fitted_log_var") |>
      _[, variable := n] |>
      _[, t := .I] |>
      _[, resid_log_var := residuals(het_model)]
  }))

data <- data |>
  merge(fitted_log_var_data, by = c("t", "variable"))

diff_vars <- c("output", "consumption", "investment", "productivity", "TFP")
data[, fitted_adj := fitted]
data[variable %in% diff_vars, fitted_adj := fitted - shift(fitted, n = 1, type = "lag"), by = "variable"]


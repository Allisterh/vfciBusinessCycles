library(data.table)
library(vfciBCHelpers)

## Settings
lags <- 2

## Fit the VAR
data <- get_var_data(make_stationary = TRUE, vfci = NULL, end_date = as.Date("2022-07-01"))

VARselect(data[, -"date"])$selection

var <- fit_var(data, lags = lags)

data[, t := .I - lags]

het_reg <- fit_het_reg_from_var(var, lags = 0:lags, hetreg_horizon = 10)

## Save out to Disk
het_reg$dt |>
  merge(data[, .(date, t)], by = "t") |>
  _[, .(date, variable, fitted, log_var_fitted)] |>
  fwrite("./data/paper-figures/charts/mean-vol-estimate-var.csv")

##### Figures

data <- het_reg$dt |>
  merge(data[, .(date, t)], by = "t") |>
  _[, .(date, variable, fitted, log_var_fitted)]

p <-
  data |>
  _[variable == "output"] |>
  ggplot(aes(
    x = log_var_fitted,
    y = fitted
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, ) +
  labs(
    x = "Log Var Fitted",
    y = "Mean Fitted",
    caption = "Note: VAR estimated in differences for non stationary series."
  ) +
  theme_paper

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/mean-vol-int-macro-vfci-output.pdf",
  p, width = 5.5, height = 4, units = "in"
)

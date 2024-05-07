library(purrr)
library(data.table)
library(tidyfast)
library(vars)
library(fevdid)
library(vfciBCHelpers)

## Settings
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
lags <- 2

## Make VAR
data <- get_var_data()
var <- fit_var(data, lags = lags)

all_variables <- data[, -"date"] |> colnames() |> set_names()
data[, t := .I - lags]

## Use max share identification method
fevdfd_vars <- map(all_variables, id_fevdfd, x = var, freqs = bc_freqs)


## Construct VAR IRFs, HDs, etc.

fevdfd_irfs <- fevdfd_vars |>
  map(~ irf(.x, n.ahead = 40)$irf) |>
  list_rbind(names_to = "target") |>
  setDT()

fevdfd_hds <- fevdfd_vars |>
  map(~ hd(.x)$hd) |>
  list_rbind(names_to = "target") |>
  setDT() |>
  merge(data[, .(t, date)], by = "t")

fevdfd_hss <- fevdfd_vars |>
  map(~ hs(.x)$hs) |>
  list_rbind(names_to = "target") |>
  setDT() |>
  merge(data[, .(t, date)], by = "t")

fevdfd_hss_cross <- merge(
  fevdfd_hss,
  fevdfd_hss,
  by = c("t", "date", "impulse"),
  allow.cartesian = TRUE,
  suffixes = c("_x", "_y")
)

fevdfd_corrs <- fevdfd_vars |> hs_corr()

fevdfd_fevdfd <- fevdfd_vars |>
  map(~ fevdfd(.x)$fevdfd) |>
  list_rbind(names_to = "target") |>
  setDT()

fevdfd_fevdfd_bc_avg <- fevdfd_fevdfd |>
  _[f %between% bc_freqs] |>
  _[, .(avg_fevdfd = mean(fevdfd)), by = .(impulse, response, target)]


## Save VAR Coefficients
coefficients(var) |>
  map(~ as.data.table(.x, keep.rownames = "independent")) |>
  list_rbind(names_to = "dependent") |>
  setnames(old = c("Estimate", "Std. Error"), c("estimate", "sd")) |>
  _[, .(dependent, independent, estimate, sd)] |>
  fwrite("./data/paper-figures/tables/var_estimated_coefficients.csv")


## Save Figure Data

fevdfd_irfs |>
  _[impulse == "Main"] |>
  _[target == "vfci"] |>
  _[, .(h, response, irf)] |>
  fwrite("./data/paper-figures/charts/irf-max-share-vfci.csv")

fevdfd_irfs |>
  _[impulse == "Main"] |>
  _[target %in% c("vfci", "unemployment")] |>
  _[, .(h, target, response, irf)] |>
  fwrite("./data/paper-figures/charts/irf-max-share-vfci-unemployment.csv")

fevdfd_hds |>
  _[impulse == "Main"] |>
  _[response == "unemployment"] |>
  _[target == "vfci"] |>
  _[, .(date, hd)] |>
  fwrite("./data/paper-figures/charts/hd-max-share-vfci-response-unemployment.csv")

fevdfd_hds |>
  _[impulse == "Main"] |>
  _[response == "unemployment"] |>
  _[target %in% c("vfci", "unemployment")] |>
  _[, .(date, target, hd)] |>
  fwrite("./data/paper-figures/charts/hd-max-share-vfci-unemployment-response-unemployment.csv")

fevdfd_hss |>
  _[impulse == "Main"] |>
  _[target %in% c("vfci", "unemployment")] |>
  _[, .(date, target, hs)] |>
  dt_pivot_wider(names_from = target, values_from = hs) |>
  fwrite("./data/paper-figures/charts/hs-max-share-vfci-unemployment.csv")

fevdfd_hss_cross |>
  _[impulse == "Main"] |>
  _[target_x == "vfci"] |>
  _[target_y != "vfci"] |>
  _[, .(date, vfci = hs_x, target_y, hs_y)] |>
  fwrite("./data/paper-figures/charts/hs-cross-max-share.csv")

fevdfd_corrs |>
  _[impulse == "Main"] |>
  fwrite("./data/paper-figures/tables/hs-cross-correlations.csv")

fevdfd_fevdfd_bc_avg |>
  _[impulse == "Main"] |>
  _[, .(target, response, avg_fevdfd)] |>
  fwrite("./data/paper-figures/tables/avg-fevdfd-bc-freqs-max-share.csv")

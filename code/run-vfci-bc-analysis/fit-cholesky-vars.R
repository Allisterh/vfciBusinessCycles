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

data[, t := .I - lags]

## Use max share identification method
fevdfd_vars <- map(c(fevdfd = "vfci"), id_fevdfd, x = var, freqs = bc_freqs)

## Use Cholesky identification
reordered_vars <- map(c(chol = "vfci"), reorder_var, var = var)
chol_vars <- map(reordered_vars, id_ordered_chol)

all_vars <- c(fevdfd_vars, chol_vars)

## Construct VAR IRFs, HDs, etc.

all_irfs <- all_vars |>
  map(~ irf(.x, n.ahead = 40)$irf) |>
  list_rbind(names_to = "identification") |>
  setDT()


## Save Figure Data

all_irfs |>
  _[impulse %in% c("Main", "Chol_1")] |>
  _[, .(identification, h, response, irf)] |>
  fwrite("./data/paper-figures/charts/irf-chol-vfci-max-share-vfci.csv")

library(purrr)
library(data.table)
library(tidyfast)
library(vars)
library(fevdid)
library(vfciBCHelpers)

## Settings
lags <- 2
end_date <- as.Date("2024-07-01")
make_stationary <- TRUE

## Make VAR

fit_cholesky_var <- function(data, lags, chol_col) {
  var <- fit_var(data, lags = lags)
  reordered_var <- reorder_var(chol_col, var = var)
  chol_var <- id_ordered_chol(reordered_var)
  data[, t := .I - lags]
  chol_var$dates <- data[, .(t, date)]
  return(chol_var)
}

chol_vars <- list(
  chol_vfci_f12 =
    est_vfci("output", c("pc1", "pc2", "pc3", "pc4"), forward = 12) |>
    get_var_data(
      vfci_dt = _,
      end_date = end_date,
      make_stationary = make_stationary
    ) |>
    fit_cholesky_var(lags, "vfci"),
  chol_vfci_f1 =
    est_vfci("output", c("pc1", "pc2", "pc3", "pc4"), forward = 1) |>
    get_var_data(
      vfci_dt = _,
      end_date = end_date,
      make_stationary = make_stationary
    ) |>
    fit_cholesky_var(lags, "vfci"),
  chol_fcig =
    get_var_data(
      vfci = NULL,
      end_date = end_date,
      add_cols = c("fci_g"),
      make_stationary = make_stationary
    ) |>
    fit_cholesky_var(lags, "fci_g"),
  chol_epu =
    get_var_data(
      vfci = NULL,
      end_date = end_date,
      add_cols = c("epu"),
      make_stationary = make_stationary
    ) |>
    fit_cholesky_var(lags, "epu")
)

## Construct VAR IRFs, HDs, etc.
all_irfs <- chol_vars |>
  map(~ irf(.x, n.ahead = 40)$irf) |>
  list_rbind(names_to = "identification") |>
  setDT()

all_hs <- chol_vars |>
  map(~ {
    hs(.x)$hs |>
      merge(.x$dates, by = "t")
  }) |>
  list_rbind(names_to = "identification") |>
  setDT()

all_hd <- chol_vars |>
  map(~ {
    hd(.x)$hd |>
      merge(.x$dates, by = "t")
  }) |>
  list_rbind(names_to = "identification") |>
  setDT()

all_fevfd <- chol_vars |>
  map(~ fevfd(.x)$fevfd) |>
  list_rbind(names_to = "identification") |>
  setDT()
all_fevfd[, total := sum(fevfd), by = .(identification, f, response)]
all_fevfd[, p := 1 / f * 2 * pi]

all_q <- chol_vars |>
  map(~ data.table(
    variable = colnames(.x$y),
    weight = .x$Q[, 1]
  )) |>
  list_rbind(names_to = "identification") |>
  setDT()

all_b <- chol_vars |>
  map(~ data.table(
    variable = colnames(.x$y),
    weight = solve(.x$B)[1, ]
  )) |>
  list_rbind(names_to = "identification") |>
  setDT()

## Save Figure Data

all_irfs |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, h, response, irf)] |>
  fwrite("./data/paper-figures/charts/irf-chol.csv")

all_hs |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, date, hs)] |>
  fwrite("./data/paper-figures/charts/hs-chol.csv")

all_hd |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, date, response, hd, total)] |>
  fwrite("./data/paper-figures/charts/hd-chol.csv")


all_fevfd |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, f, p, response, fevfd, total)] |>
  fwrite("./data/paper-figures/charts/fevfd-chol.csv")

all_q |>
  fwrite("./data/paper-figures/charts/q-chol.csv")

all_b |>
  fwrite("./data/paper-figures/charts/b-chol.csv")

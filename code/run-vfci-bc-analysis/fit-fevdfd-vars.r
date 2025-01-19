library(purrr)
library(data.table)
library(tidyfast)
library(vars)
library(fevdid)
library(vfciBCHelpers)

## Settings
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
lags <- 2
end_date <- as.Date("2022-07-01")
make_stationary <- TRUE

data <- get_var_data(vfci = NULL, end_date = end_date, make_stationary = make_stationary)
data[, t := .I - lags]

## Make VARs
fevdfd_vars <- list(
  fevdfd_vfci_f12 =
    est_vfci("output", c("pc1", "pc2", "pc3", "pc4"), forward = 12) |>
    get_var_data(vfci_dt = _, end_date = end_date, make_stationary = make_stationary) |>
    fit_var(lags = lags) |>
    id_fevdfd("vfci", bc_freqs, sign = "pos"),
  fevdfd_vfci_f1 =
    est_vfci("output", c("pc1", "pc2", "pc3", "pc4"), forward = 1) |>
    get_var_data(vfci_dt = _, end_date = end_date, make_stationary = make_stationary) |>
    fit_var(lags = lags) |>
    id_fevdfd("vfci", bc_freqs, sign = "neg"),
  fevdfd_unem =
    get_var_data(vfci = NULL, end_date = end_date, make_stationary = make_stationary) |>
    fit_var(lags = lags) |>
    id_fevdfd("unemployment", bc_freqs, sign = "neg")
)

## Construct VAR IRFs, HDs, etc.
all_irfs <- fevdfd_vars |>
  map(~ irf(.x, n.ahead = 40)$irf) |>
  list_rbind(names_to = "identification") |>
  setDT()

all_hs <- fevdfd_vars |>
  map(~ hs(.x)$hs) |>
  list_rbind(names_to = "identification") |>
  setDT() |>
  merge(data[, .(t, date)], by = "t")

all_hd <- fevdfd_vars |>
  map(~ hd(.x)$hd) |>
  list_rbind(names_to = "identification") |>
  setDT() |>
  merge(data[, .(t, date)], by = "t")

all_fevfd <- fevdfd_vars |>
  map(~ fevfd(.x)$fevfd) |>
  list_rbind(names_to = "identification") |>
  setDT()
all_fevfd[, total := sum(fevfd), by = .(identification, f, response)]
all_fevfd[, p := 1 / f * 2 * pi]

all_q <- fevdfd_vars |>
  map(~ data.table(
    variable = colnames(.x$y),
    weight = .x$Q[, 1]
  )) |>
  list_rbind(names_to = "identification") |>
  setDT()

all_b <- fevdfd_vars |>
  map(~ data.table(
    variable = colnames(.x$y),
    weight = solve(.x$B)[1, ]
  )) |>
  list_rbind(names_to = "identification") |>
  setDT()

## Save Figure Data
all_irfs |>
  _[impulse %in% c("Main")] |>
  _[, .(identification, h, response, irf)] |>
  fwrite("./data/paper-figures/charts/irf-fevdfd.csv")

all_hs |>
  _[impulse %in% c("Main")] |>
  _[, .(identification, date, hs)] |>
  fwrite("./data/paper-figures/charts/hs-fevdfd.csv")

all_hd |>
  _[impulse %in% c("Main")] |>
  _[, .(identification, date, response, hd, total)] |>
  fwrite("./data/paper-figures/charts/hd-fevdfd.csv")

all_fevfd |>
  _[impulse %in% c("Main")] |>
  _[, .(identification, f, p, response, fevfd, total)] |>
  fwrite("./data/paper-figures/charts/fevfd-fevdfd.csv")

all_q |>
  fwrite("./data/paper-figures/charts/q-fevdfd.csv")

all_b |>
  fwrite("./data/paper-figures/charts/b-fevdfd.csv")

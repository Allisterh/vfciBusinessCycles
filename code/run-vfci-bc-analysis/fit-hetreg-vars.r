library(purrr)
library(data.table)
library(tidyfast)
library(vars)
library(fevdid)
library(vfciBCHelpers)

## Settings
lags <- 2
fin_cols <- c("pc1", "pc2", "pc3", "pc4")
end_date <- as.Date("2024-07-01")
make_stationary <- TRUE
cumsum <- FALSE
hetreg_lags <- 0
hetreg_horizon <- 12

## Make VARs
data <- get_var_data(vfci = NULL, end_date = end_date, make_stationary = make_stationary)
data[, t := .I - lags]

fin_data <-
  get_var_data(
    vfci = NULL,
    add_cols = fin_cols,
    end_date = end_date,
    make_stationary = make_stationary
  )

hr_vars <- list(
  hr_macro =
    get_var_data(
      vfci = NULL,
      end_date = end_date,
      make_stationary = make_stationary
    ) |>
    fit_var(lags = lags) |>
    id_linear_het_reg(
      "output",
      hetreg_horizon = hetreg_horizon,
      cumsum = cumsum,
      het_reg_lags = hetreg_lags,
      sign = "pos"
    ),
  hr_fin =
    fin_data[, -c(fin_cols), with = FALSE] |>
    fit_var(lags = lags) |>
    id_linear_het_reg(
      "output",
      hetreg_horizon = hetreg_horizon,
      cumsum = cumsum,
      x2 = fin_cols,
      extra_data = fin_data[, fin_cols, with = FALSE],
      method = "mriv",
      sign = "pos"
    )
)

## Construct VAR IRFs, HDs, etc.
all_irfs <- hr_vars |>
  map(~ irf(.x, n.ahead = 40)$irf) |>
  list_rbind(names_to = "identification") |>
  setDT()

all_hs <- hr_vars |>
  map(~ hs(.x)$hs) |>
  list_rbind(names_to = "identification") |>
  setDT() |>
  merge(data[, .(t, date)], by = "t")

all_hd <- hr_vars |>
  map(~ hd(.x)$hd) |>
  list_rbind(names_to = "identification") |>
  setDT() |>
  merge(data[, .(t, date)], by = "t")

all_fevfd <- hr_vars |>
  map(~ fevfd(.x)$fevfd) |>
  list_rbind(names_to = "identification") |>
  setDT()
all_fevfd[, total := sum(fevfd), by = .(identification, f, response)]
all_fevfd[, p := 1 / f * 2 * pi]

all_q <- hr_vars |>
  map(~ data.table(
    variable = colnames(.x$y),
    weight = .x$Q[, 1]
  )) |>
  list_rbind(names_to = "identification") |>
  setDT()

all_b <- hr_vars |>
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
  fwrite("./data/paper-figures/charts/irf-hetreg.csv")

all_hs |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, date, hs)] |>
  fwrite("./data/paper-figures/charts/hs-hetreg.csv")

all_hd |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, date, response, hd, total)] |>
  fwrite("./data/paper-figures/charts/hd-hetreg.csv")


all_fevfd |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, f, p, response, fevfd, total)] |>
  fwrite("./data/paper-figures/charts/fevfd-hetreg.csv")

all_q |>
  fwrite("./data/paper-figures/charts/q-hetreg.csv")

all_b |>
  fwrite("./data/paper-figures/charts/b-hetreg.csv")

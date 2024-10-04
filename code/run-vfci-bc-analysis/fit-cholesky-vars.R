library(purrr)
library(data.table)
library(tidyfast)
library(vars)
library(fevdid)
library(vfciBCHelpers)

## Settings
lags <- 2

## Make VAR
get_var_data(add_cols = c("fci_g"))[, -"vfci"]

fit_cholesky_var <- function(data, lags, chol_col) {
  var <- fit_var(data, lags = lags)
  reordered_var <- reorder_var(chol_col, var = var)
  chol_var <- id_ordered_chol(reordered_var)
  return(chol_var)
}

small_data <- get_var_data(cols = c(
  output = "output",
  consumption = "consumption",
  unemployment = "unemployment",
  investment = "investment",
  hours_worked = "hours_worked",
  labor_share = "labor_share",
  productivity = "productivity",
  TFP = "TFP",
  inflation = "inflation",
  interest = "interest",
  vfci = "vfci_fgr1gdpc1"
))

chol_vars <- list(
  vfci_chol = fit_cholesky_var(get_var_data(vfci = "vfci_fgr1gdpc1"), lags, "vfci"),
  vfci_last_chol = id_ordered_chol(fit_var(small_data, lags)),
  # vfci_first_chol = fit_cholesky_var(small_data, lags, "vfci"),
  fcig_chol = fit_cholesky_var(get_var_data(add_cols = c("fci_g"))[, -"vfci"], lags, "fci_g"),
  epu_chol = fit_cholesky_var(get_var_data(add_cols = c("epu"))[, -"vfci"], lags, "epu")
)

## Construct VAR IRFs, HDs, etc.
all_irfs <- chol_vars |>
  map(~ irf(.x, n.ahead = 40)$irf) |>
  list_rbind(names_to = "identification") |>
  setDT()


## Check IRFS
chol_vars$vfci_chol
irf(chol_vars$vfci_chol, n.ahead = 1)$irf |> dplyr::filter(impulse == "Chol_11")
irf(chol_vars$vfci_last_chol, n.ahead = 1)$irf |> dplyr::filter(impulse == "Chol_11")

## Save Figure Data

all_irfs |>
  _[(identification != "vfci_last_chol" & impulse %in% c("Chol_1")) |
      (identification == "vfci_last_chol" & impulse == paste0("Chol_", ncol(small_data) - 1))] |>
  _[, .(identification, h, response, irf)] |>
  fwrite("./data/paper-figures/charts/irf-chol.csv")


## Make Figures
library(ggplot2)

data <- all_irfs |>
  _[(identification != "vfci_last_chol" & impulse %in% c("Chol_1")) |
      (identification == "vfci_last_chol" & impulse ==  paste0("Chol_", ncol(small_data) - 1))] |>
  _[, .(identification, h, response, irf)]

p <-
  data |>
  _[identification %in% c("vfci_chol", "vfci_last_chol", "vfci_first_chol")] |>
  _[, response :=
  factor(
    response,
    levels = variable_labels,
    labels = labels(variable_labels),
    ordered = TRUE
  )] |>
  ggplot(aes(
    x = h,
    y = irf,
    color = identification
  )) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line() +
  facet_wrap(
    vars(response),
    scales = "free_y",
    nrow = 3
  ) +
  labs(
    x = "Horizon (quarters)",
    y = "Impulse Response Function"
  ) +
  theme_paper +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.125)
  )

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/irf-chol-ext-vfci.pdf",
  p, width = 5.5, height = 4, units = "in"
)

######

p <-
  data |>
  _[identification %in% c("vfci_chol", "fcig_chol", "epu_chol")] |>
  ggplot(aes(
    x = h,
    y = irf,
    color = identification
  )) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line() +
  facet_wrap(
    vars(response),
    scales = "free_y",
    ncol = 4
  ) +
  labs(
    x = "Horizon (quarters)",
    y = "Impulse Response Function"
  ) +
  theme_paper +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.125)
  )

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/irf-all-cholesky.pdf",
  p, width = 5.5, height = 6, units = "in"
)

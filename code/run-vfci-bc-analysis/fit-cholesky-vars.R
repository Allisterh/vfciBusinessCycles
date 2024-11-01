library(purrr)
library(data.table)
library(tidyfast)
library(vars)
library(fevdid)
library(vfciBCHelpers)

## Settings
lags <- 2

## Make VAR

fit_cholesky_var <- function(data, lags, chol_col) {
  var <- fit_var(data, lags = lags)
  reordered_var <- reorder_var(chol_col, var = var)
  chol_var <- id_ordered_chol(reordered_var)
  return(chol_var)
}

chol_vars <- list(
  vfci_chol =
    est_vfci("output", c("pc1", "pc2", "pc3", "pc4"), forward = 10) |>
    get_var_data(vfci_dt = _, end_date = as.Date("2022-07-01")) |>
    fit_cholesky_var(lags, "vfci"),
  fcig_chol =
    get_var_data(vfci = NULL, end_date = as.Date("2022-07-01"), add_cols = c("fci_g")) |>
    fit_cholesky_var(lags, "fci_g"),
  epu_chol =
    get_var_data(vfci = NULL, end_date = as.Date("2022-07-01"), add_cols = c("epu")) |>
    fit_cholesky_var(lags, "epu")
)

## Construct VAR IRFs, HDs, etc.
all_irfs <- chol_vars |>
  map(~ irf(.x, n.ahead = 40)$irf) |>
  list_rbind(names_to = "identification") |>
  setDT()

## Save Figure Data

all_irfs |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, h, response, irf)] |>
  fwrite("./data/paper-figures/charts/irf-chol.csv")


## Make Figures
library(ggplot2)

data <- all_irfs |>
  _[impulse %in% c("Chol_1")] |>
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
  _[, response := factor(response, levels = c(
    `FCI Growth` = "fci_g",
    `EPU` = "epu",
    variable_labels
  ), ordered = TRUE)] |>
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

library(purrr)
library(data.table)
library(tidyfast)
library(vars)
library(fevdid)
library(vfciBCHelpers)

## Settings
lags <- 2
end_date <- as.Date("2022-07-01")
make_stationary <- FALSE

## Make VAR

fit_cholesky_var <- function(data, lags, chol_col) {
  var <- fit_var(data, lags = lags)
  reordered_var <- reorder_var(chol_col, var = var)
  chol_var <- id_ordered_chol(reordered_var)
  return(chol_var)
}

chol_vars <- list(
  chol_vfci_f12 =
    est_vfci("output", c("pc1", "pc2", "pc3", "pc4"), forward = 12) |>
    get_var_data(vfci_dt = _, end_date = end_date, make_stationary = make_stationary) |>
    fit_cholesky_var(lags, "vfci"),
  chol_vfci_f1 =
    est_vfci("output", c("pc1", "pc2", "pc3", "pc4"), forward = 1) |>
    get_var_data(vfci_dt = _, end_date = end_date, make_stationary = make_stationary) |>
    fit_cholesky_var(lags, "vfci"),
  chol_fcig =
    get_var_data(vfci = NULL, end_date = end_date, add_cols = c("fci_g"), make_stationary = make_stationary) |>
    fit_cholesky_var(lags, "fci_g"),
  chol_epu =
    get_var_data(vfci = NULL, end_date = end_date, add_cols = c("epu"), make_stationary = make_stationary) |>
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

fevdfd_irf <- fread("./data/paper-figures/charts/irf-fevdfd.csv")

p <-
  data |>
  rbind(fevdfd_irf) |>
  _[identification %in% c("chol_vfci_f1", "chol_vfci_f12", "fevdfd_unem")] |>
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
  "./paper-figures/charts/irf-chol-ext-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)

######

p <-
  data |>
  rbind(fevdfd_irf) |>
  _[identification %in% c("fevdfd_unem", "chol_fcig", "chol_epu")] |>
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
  "./paper-figures/charts/irf-chol-epu-fcig-fevdfd-unem.pdf",
  p, width = 5.5, height = 6, units = "in"
)

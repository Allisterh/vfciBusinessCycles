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
make_stationary <- FALSE


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

## Save Figure Data

all_irfs |>
  _[impulse %in% c("Main")] |>
  _[, .(identification, h, response, irf)] |>
  fwrite("./data/paper-figures/charts/irf-fevdfd.csv")

#####

## Make Figures
library(ggplot2)

fevdfd_data <- all_irfs |>
  _[impulse %in% c("Main")] |>
  _[, .(identification, h, response, irf)]


p <-
  fevdfd_data |>
  _[identification %in% c("fevdfd_vfci_f1", "fevdfd_vfci_f12", "fevdfd_unem")] |>
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

p

ggsave(
  "./paper-figures/charts/irf-fevdfd-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)

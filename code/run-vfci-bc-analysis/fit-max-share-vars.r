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
data <- get_var_data(vfci = "vfci_fgr10output", end_date = as.Date("2022-07-01"))
var <- fit_var(data, lags = lags)

all_variables <- data[, -"date"] |> colnames() |> set_names()
data[, t := .I - lags]

fevdfd_vars <- list(
  fevdfd_vfci = id_fevdfd(var, "vfci", bc_freqs, sign = "pos"),
  fevdfd_unem = id_fevdfd(var, "unemployment", bc_freqs, sign = "neg")
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

chol_data <- fread("./data/paper-figures/charts/irf-chol.csv")

data <- list_rbind(list(
  chol_data[identification == "vfci_chol"],
  fevdfd_data[identification == "fevdfd_vfci"]
))

p <-
  data |>
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
  "./paper-figures/charts/irf-chol-ext-vfci-fevdfd-vfci.pdf",
  p, width = 5.5, height = 4, units = "in"
)

##

data <- list_rbind(list(
  chol_data[identification == "vfci_chol"],
  fevdfd_data[identification == "fevdfd_unem"]
))

p <-
  data |>
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
  "./paper-figures/charts/irf-chol-ext-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)

data <- list_rbind(list(
  fevdfd_data[identification == "fevdfd_vfci"],
  fevdfd_data[identification == "fevdfd_unem"]
))

p <-
  data |>
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
  "./paper-figures/charts/irf-chol-ext-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)

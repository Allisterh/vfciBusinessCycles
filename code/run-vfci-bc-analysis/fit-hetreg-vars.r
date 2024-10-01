library(purrr)
library(data.table)
library(tidyfast)
library(vars)
library(fevdid)
library(vfciBCHelpers)

## Settings
lags <- 2
fin_cols <- c("pc1", "pc2", "pc3", "pc4")

## Make VAR
data <- get_var_data(vfci = NULL)
fin_data <- get_var_data(add_cols = fin_cols)[, fin_cols, with = FALSE]
var <- fit_var(data, lags = lags)

all_variables <- data[, -"date"] |> colnames() |> set_names()
data[, t := .I - lags]

hr_vars <- list(
  hr_macro = id_linear_het_reg(var, "consumption", sign = "neg"),
  hr_fin = id_linear_het_reg(var, "consumption", x2 = fin_cols, extra_data = fin_data, method = "mriv", sign = "neg")
)

## Construct VAR IRFs, HDs, etc.
all_irfs <- hr_vars |>
  map(~ irf(.x, n.ahead = 40)$irf) |>
  list_rbind(names_to = "identification") |>
  setDT()

## Save Figure Data

all_irfs |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, h, response, irf)] |>
  fwrite("./data/paper-figures/charts/irf-hetreg.csv")

#####

## Make Figures
library(ggplot2)

hr_data <- all_irfs |>
  _[impulse %in% c("Chol_1")] |>
  _[, .(identification, h, response, irf)]

chol_data <- fread("./data/paper-figures/charts/irf-chol.csv")

data <- list_rbind(list(
  chol_data[identification == "vfci_chol"],
  hr_data[identification == "hr_fin"]
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
  "./paper-figures/charts/irf-chol-ext-vfci-hr-int-fin.pdf",
  p, width = 5.5, height = 4, units = "in"
)

##

data <- list_rbind(list(
  chol_data[identification == "vfci_chol"],
  hr_data[identification == "hr_macro"]
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
  "./paper-figures/charts/irf-chol-ext-vfci-hr-int-macro.pdf",
  p, width = 5.5, height = 4, units = "in"
)

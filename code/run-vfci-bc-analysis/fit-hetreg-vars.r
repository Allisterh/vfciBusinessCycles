library(purrr)
library(data.table)
library(tidyfast)
library(vars)
library(fevdid)
library(vfciBCHelpers)

## Settings
lags <- 2
fin_cols <- c("pc1", "pc2", "pc3", "pc4")
end_date <- as.Date("2022-07-01")
make_stationary <- FALSE
cumsum <- FALSE

## Make VARs
fin_data <-
  get_var_data(
    vfci = NULL,
    add_cols = fin_cols,
    end_date = as.Date("2022-07-01"),
    make_stationary = make_stationary
  )

hr_vars <- list(
  hr_macro =
    get_var_data(vfci = NULL, end_date = as.Date("2022-07-01"), make_stationary = make_stationary) |>
    fit_var(lags = lags) |>
    id_linear_het_reg(
      "output",
      hetreg_horizon = 12,
      cumsum = cumsum,
      het_reg_lags = 0,
      sign = "pos"
    ),
  hr_fin =
    fin_data[, -c(fin_cols), with = FALSE] |>
    fit_var(lags = lags) |>
    id_linear_het_reg(
      "output",
      hetreg_horizon = 12,
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

fevdfd_data <- fread("./data/paper-figures/charts/irf-fevdfd.csv")

data <- list_rbind(list(
  fevdfd_data,
  hr_data
))

p <-
  data |>
  _[identification %in% c("fevdfd_unem", "hr_macro")] |>
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
  "./paper-figures/charts/irf-hr-macro-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)

##


p <-
  data |>
  _[identification %in% c("fevdfd_unem", "hr_fin")] |>
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
  "./paper-figures/charts/irf-hr-fin-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)

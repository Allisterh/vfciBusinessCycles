library(data.table)
library(tidyfast)
library(stringr)
library(vfciBCHelpers)

## Settings
v_lags <- 2
make_stationary <- TRUE
cumsum <- FALSE
hetreg_lags <- 0
end_date <- as.Date("2022-07-01")
hetreg_horizon <- 12

## Estimate External VFCIs
ext_vfci_f1 <-
  est_vfci(
    y = "output",
    x = c("pc1", "pc2", "pc3", "pc4"),
    forward = 1
  )

ext_vfci_f12 <-
  est_vfci(
    y = "output",
    x = c("pc1", "pc2", "pc3", "pc4"),
    forward = 12
  )

## Internal Macro VFCI
data <-
  get_var_data(
    vfci = NULL,
    end_date = end_date,
    make_stationary = make_stationary
  )

v <- fit_var(data, lags = v_lags)

hr <- fit_het_reg_from_var(v, hetreg_horizon = hetreg_horizon, cumsum = cumsum, lags = hetreg_lags)

int_macro_vfci <- hr$dt |>
  merge(copy(data)[, t := .I - v_lags][, .(t, date)], by = "t")


## Internal Financial VFCI
fin_cols <- c("pc1", "pc2", "pc3", "pc4")
data <-
  get_var_data(
    vfci = NULL,
    add_cols = fin_cols,
    end_date = end_date,
    make_stationary = make_stationary
  )

v <- fit_var(data[, !c(fin_cols), with = FALSE], lags = v_lags)

hr <- fit_het_reg_from_var(v, hetreg_horizon = hetreg_horizon, cumsum = cumsum, x2 = fin_cols, extra_data = data[, ..fin_cols])

int_fin_vfci <- hr$dt |>
  merge(copy(data)[, t := .I - v_lags][, .(t, date)], by = "t")


## Combine data to compare
comp_data <-
  ext_vfci_f1[, .(date, ext_vfci_f1 = vfci)] |>
  merge(ext_vfci_f12[, .(date, ext_vfci_f12 = vfci)]) |>
  merge(int_macro_vfci[variable == "output", .(date, int_macro_vfci = log_var_fitted)]) |>
  merge(int_fin_vfci[variable == "output", .(date, int_fin_vfci = log_var_fitted)])

## Write data to file
comp_data |>
  fwrite("./data/paper-figures/charts/compare-int-ext-vfci.csv")

## Calculate Correlations
corrs <- cor(na.omit(comp_data[, -"date"]))

corrs_list <- list(
  ext_vfci_f1_f12_corr = corrs["ext_vfci_f1", "ext_vfci_f12"],
  int_fin_ext_vfci_f1_corr = corrs["int_fin_vfci", "ext_vfci_f1"],
  int_macro_ext_vfci_f1_corr = corrs["int_macro_vfci", "ext_vfci_f1"],
  int_fin_ext_vfci_f12_corr = corrs["int_fin_vfci", "ext_vfci_f12"],
  int_macro_ext_vfci_f12_corr = corrs["int_macro_vfci", "ext_vfci_f12"],
  int_macro_fin_vfci_corr = corrs["int_fin_vfci", "int_macro_vfci"]
)

corrs_list |>
  saveRDS("./data/paper-figures/inline-values/compare-int-ext-vfci-corrs.rds")


## Figures
library(ggplot2)


p <-
  comp_data |>
  tidyfast::dt_pivot_longer(-date) |>
  _[name %in% c("ext_vfci_f12", "int_fin_vfci")] |>
  _[, value := scale(value), by = name] |>
  ggplot(aes(
    x = date,
    y = value,
    color = name
  )) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line() +
  scale_color_manual(
    values = c("steelblue", "orange"),
  ) +
  scale_x_date(
    breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
    labels = seq(1960, 2020, by = 10)
  ) +
  labs(
    x = NULL,
    y = "Normalized VFCI"
  ) +
  theme_paper +
  theme(
    legend.position = "top"
  )

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/compare-int-fin-ext-vfci-f12.pdf",
  p, width = 5, height = 3, units = "in"
)

#####

p <-
  comp_data |>
  tidyfast::dt_pivot_longer(-date) |>
  _[name %in% c("ext_vfci_f12", "int_macro_vfci")] |>
  _[, value := scale(value), by = name] |>
  ggplot(aes(
    x = date,
    y = value,
    color = name
  )) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line() +
  scale_color_manual(
    values = c("steelblue", "firebrick"),
  ) +
  scale_x_date(
    breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
    labels = seq(1960, 2020, by = 10)
  ) +
  labs(
    x = NULL,
    y = "Normalized VFCI"
  ) +
  theme_paper +
  theme(
    legend.position = "top"
  )

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/compare-int-macro-ext-vfci-f12.pdf",
  p, width = 5, height = 3, units = "in"
)

#####

p <-
  comp_data |>
  tidyfast::dt_pivot_longer(-date) |>
  _[name %in% c("ext_vfci_f1", "int_fin_vfci")] |>
  _[, value := scale(value), by = name] |>
  ggplot(aes(
    x = date,
    y = value,
    color = name
  )) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line() +
  scale_color_manual(
    values = c("lightblue", "orange"),
  ) +
  scale_x_date(
    breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
    labels = seq(1960, 2020, by = 10)
  ) +
  labs(
    x = NULL,
    y = "Normalized VFCI"
  ) +
  theme_paper +
  theme(
    legend.position = "top"
  )

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/compare-int-fin-ext-vfci-f1.pdf",
  p, width = 5, height = 3, units = "in"
)

#####

p <-
  comp_data |>
  tidyfast::dt_pivot_longer(-date) |>
  _[name %in% c("ext_vfci_f1", "int_macro_vfci")] |>
  _[, value := scale(value), by = name] |>
  ggplot(aes(
    x = date,
    y = value,
    color = name
  )) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line() +
  scale_color_manual(
    values = c("lightblue", "firebrick"),
  ) +
  scale_x_date(
    breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
    labels = seq(1960, 2020, by = 10)
  ) +
  labs(
    x = NULL,
    y = "Normalized VFCI"
  ) +
  theme_paper +
  theme(
    legend.position = "top"
  )

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/compare-int-macro-ext-vfci-f1.pdf",
  p, width = 5, height = 3, units = "in"
)

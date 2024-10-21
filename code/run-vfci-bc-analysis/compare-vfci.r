library(data.table)
library(tidyfast)
library(stringr)
library(vfciBCHelpers)

data <- readRDS("./data/all_analysis_data.rds")

vfci_cols <- colnames(data)[str_detect(colnames(data), "vfci")]

vfci_data <- data[, c("date", vfci_cols), with = FALSE]

ext_vfci_dt <- vfci_data |>
  dt_pivot_longer(-date) |>
  _[!is.na(value)] |>
  _[, forward := as.numeric(str_extract(name, "(?<=fgr)\\d*"))] |>
  _[, shift := as.numeric(str_extract(name, "(?<=s)1"))][is.na(shift), shift := 0] |>
  _[, trimmed := str_remove(str_remove(name, "vfci_fgr\\d*"), "s1")] |>
  _[, variable := str_extract(trimmed, "^[^_]*")] |>
  _[, type := str_extract(trimmed, "(?<=_)[^_]*")][is.na(type), type := "baseline"] |>
  _[, c("trimmed", "name") := NULL]


## Internal Macro VFCI
data <- get_var_data(vfci = NULL, end_date = as.Date("2022-07-01"))
v_lags <- 2

v <- fit_var(data, lags = v_lags)

hr <- fit_het_reg_from_var(v, hetreg_horizon = 10, lags = 0:v_lags)

int_macro_vfci <- hr$dt |>
  merge(copy(data)[, t := .I - v_lags][, .(t, date)], by = "t")


## Internal Financial VFCI
fin_cols <- c("pc1", "pc2", "pc3", "pc4")
data <- get_var_data(vfci = NULL, add_cols = fin_cols, end_date = as.Date("2022-07-01"))
v_lags <- 2

v <- fit_var(data[, !c(fin_cols), with = FALSE], lags = v_lags)

hr <- fit_het_reg_from_var(v, hetreg_horizon = 10, x2 = fin_cols, extra_data = data[, ..fin_cols])

int_fin_vfci <- hr$dt |>
  merge(copy(data)[, t := .I - v_lags][, .(t, date)], by = "t")


## Combine data to compare
comp_data <-
  ext_vfci_dt[forward == 10 & shift == 0 & type == "baseline" & variable == "gdpc1", .(date, ext_vfci_gdp = value)] |>
  merge(int_macro_vfci[variable == "output", .(date, int_macro_vfci = log_var_fitted)]) |>
  merge(int_fin_vfci[variable == "output", .(date, int_fin_vfci = log_var_fitted)])

## Write data to file
comp_data |>
  fwrite("./data/paper-figures/charts/compare-int-ext-vfci.csv")

## Calculate Correlations
corrs <- cor(na.omit(comp_data[, -"date"]))

corrs_list <- list(
  int_ext_fin_vfci_corr = corrs["int_fin_vfci", "ext_vfci_gdp"],
  int_ext_macro_vfci_corr = corrs["int_macro_vfci", "ext_vfci_gdp"],
  int_macro_fin_vfci_corr = corrs["int_fin_vfci", "int_macro_vfci"]
)

corrs_list |>
  saveRDS("./data/paper-figures/inline-values/compare-int-ext-vfci-corrs.rds")


## Figures
library(ggplot2)


p <-
  comp_data |>
  tidyfast::dt_pivot_longer(-date) |>
  _[name %in% c("ext_vfci_gdp", "int_fin_vfci")] |>
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
  "./paper-figures/charts/compare-int-ext-financial-vfci.pdf",
  p, width = 5, height = 3, units = "in"
)

#####

p <-
  comp_data |>
  tidyfast::dt_pivot_longer(-date) |>
  _[name %in% c("ext_vfci_gdp", "int_macro_vfci")] |>
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
  "./paper-figures/charts/compare-int-ext-macro-vfci.pdf",
  p, width = 5, height = 3, units = "in"
)

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


## Internal VFCI
fin_cols <- c("pc1", "pc2", "pc3", "pc4")
data <- get_var_data(add_cols = fin_cols)
data <- data[, vfci := NULL]
v_lags <- 2

v <- fit_var(data[, !c(fin_cols), with = FALSE], lags = v_lags)

hr <- fit_het_reg_from_var(v, x2 = fin_cols, extra_data = data[, ..fin_cols])

int_vfci <- hr$dt |>
  merge(copy(data)[, t := .I - v_lags][, .(t, date)], by = "t")


## Combine data to compare
comp_data <-
  merge(
    ext_vfci_dt[forward == 1 & shift == 0 & type == "baseline" & variable == "gdpc1", .(date, ext_vfci = value)],
    int_vfci[variable == "output", .(date, int_vfci = log_var_fitted)],
  )


## Write data to file
# data |>
#   _[, .(date, vfci = vfci_fgr10gdpc1)] |>
#   na.omit() |>
#   fwrite("./data/paper-figures/charts/vfci.csv")

## Figures
library(ggplot2)


p <-
  comp_data |>
  tidyfast::dt_pivot_longer(-date) |>
  _[, value := scale(value), by = name] |>
  ggplot(aes(
    x = date,
    y = value,
    color = name
  )) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line() +
  scale_color_manual(
    values = c("steelblue", "goldenrod")
  ) +
  labs(
    x = NULL,
    y = "Normalized VFCI"
  ) +
  theme_paper +
  theme(
    legend.position = "right"
  )

p

ggsave(
  "./paper-figures/charts/compare-int-ext-vfci.pdf",
  p, width = 5, height = 2, units = "in"
)

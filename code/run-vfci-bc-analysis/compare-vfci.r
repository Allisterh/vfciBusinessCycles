library(data.table)
library(tidyfast)
library(stringr)

data <- readRDS("./data/all_analysis_data.rds")

vfci_cols <- colnames(data)[str_detect(colnames(data), "vfci")]

vfci_data <- data[, c("date", vfci_cols), with = FALSE]

vfci_data <- vfci_data |>
  dt_pivot_longer(-date) |>
  _[name != "vfci"] |>
  _[name != "vfci_fgr1gdpc1_l1"] |>
  _[, forward := as.numeric(str_extract(name, "(?<=fgr)\\d*"))] |>
  _[, variable := str_remove_all(str_remove(name, "vfci_fgr"), "\\d*")] |>
  na.omit()

data |>
  _[, .(date, vfci = vfci_fgr10gdpc1)] |>
  na.omit() |>
  fwrite("./data/paper-figures/charts/vfci.csv")

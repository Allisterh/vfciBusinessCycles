## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(gt)
library(tidyfast)
library(vfciBCHelpers)

data <- get_var_data()

summ <-
  data |>
  dt_pivot_longer(-date) |>
  _[, .(
    Minimum = min(value),
    Maximum = max(value),
    Observations = .N,
    Mean = mean(value),
    Median = median(value),
    `Standard Deviation` = sd(value)
  ),
  by = .(name)
  ] |>
  _[, name :=
    factor(name, levels = variable_labels, labels = labels(variable_labels), ordered = TRUE)]

t <-
  summ |>
  gt(
    rowname_col = "name"
  ) |>
  cols_move(
    c("Minimum", "Mean", "Median", "Maximum", "Standard Deviation"),
    "Observations"
  ) |>
  cols_label(
    "Minimum" = "Min",
    "Maximum" = "Max",
    "Standard Deviation" = "SD"
  ) |>
  fmt_number(
    columns = c("Minimum", "Mean", "Median", "Maximum", "Standard Deviation"),
    decimals = 2,
    drop_trailing_zeros = TRUE
  ) |>
  fmt_number(
    columns = c("Minimum", "Mean", "Median", "Maximum", "Standard Deviation"),
    rows = c(1, 2, 3, 4, 6, 9, 10),
    decimals = 0
  )

t |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-figures/tables/summary-stats.tex")

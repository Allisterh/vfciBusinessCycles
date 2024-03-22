## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(gt)
library(tidyfast)

source("./code/vfci-bc/target-all-var-bc-freqs.R")

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
  ]

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
    rows = c("output", "investment", "consumption", "hours_worked", "labor_share", "productivity", "TFP"),
    decimals = 0
  ) |>
  tab_header(
    title = "Summary Statistics for VAR Data"
  ) |>
  tab_footnote(paste0(
    "All data series are measured quarterly from ",
    zoo::as.yearqtr(min(data$date)),
    " to ",
    zoo::as.yearqtr(max(data$date)),
    "."
  ))

t |>
  as_latex() |>
  stringr::str_replace("\\\\caption\\*", "\\\\caption") |>
  stringr::str_replace("\\} \\\\\\\\", "\\}\\\\label\\{tab\\:summary-stats\\} \\\\\\\\") |>
  write(file = "./paper-Overleaf/tables/data-summary-stats.tex")

## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(gt)
library(xtable)
library(tidyfast)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/paper-figures/theme-paper.r")

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
  _[, name := factor(name, levels = var_labels, labels = labels(var_labels), ordered = T)]

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
    rows = factor(c("Output", "Investment", "Consumption", "Hours Worked", "Labor Share", "Labor Prod.", "TFP")),
    decimals = 0
  ) |>
  tab_footnote(paste0(
    "All data series are measured quarterly from ",
    zoo::as.yearqtr(min(data$date)),
    " to ",
    zoo::as.yearqtr(max(data$date)),
    "."
  ))

# t |>
#   as_latex() |>
#   stringr::str_replace("\\\\caption\\*", "\\\\caption") |>
#   stringr::str_replace("\\} \\\\\\\\", "\\}\\\\label\\{tab\\:summary-stats\\} \\\\\\\\") |>
#   write(file = "./paper-Overleaf/tables/data-summary-stats.tex")

t |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |> 
  write(file = "./paper-Overleaf/tables/data-summary-stats.tex")

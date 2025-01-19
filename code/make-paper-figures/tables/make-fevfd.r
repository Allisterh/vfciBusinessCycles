library(gt)
library(data.table)
library(vfciBCHelpers)

bc_freqs <- 2 * pi / c(32, 6)

data <-
  list(
    fread("./data/paper-figures/charts/fevfd-fevdfd.csv"),
    fread("./data/paper-figures/charts/fevfd-chol.csv"),
    fread("./data/paper-figures/charts/fevfd-hetreg.csv")
  ) |>
  purrr::list_rbind() |>
  copy()

## Business Cycle Freqs
summary_data <-
  data |>
  _[, fevdfd := fevfd / total] |>
  _[f %between% bc_freqs, .(
    wavg_fevdfd = weighted.mean(fevdfd, total)
  ), by = .(
    response, identification
  )]

t <- summary_data |>
  _[, identification := stringr::str_replace_all(identification, "chol", "c")] |>
  _[, identification := stringr::str_replace_all(identification, "fevdfd", "f")] |>
  tidyfast::dt_pivot_wider(names_from = identification, values_from = wavg_fevdfd) |>
  gt(rowname_col = "response") |>
  fmt_percent(decimals = 1) |>
  sub_missing() |>
  tab_footnote(
    footnote = "c = Cholesky, f = FEVDFD, hr = Heteroskedasticity Regression",
  ) |>
  as_latex() |>
  stringr::str_remove("\\\\begin\\{table\\}\\[!t\\]") |>
  stringr::str_remove("\\\\end\\{table\\}")

t |>
  writeLines("./paper-figures/tables/fevfd-bcfreqs.tex")

## All Freqs
summary_data <-
  data |>
  _[, fevdfd := fevfd / total] |>
  _[, .(
    wavg_fevdfd = weighted.mean(fevdfd, total)
  ), by = .(
    response, identification
  )]

t <- summary_data |>
  _[, identification := stringr::str_replace_all(identification, "chol", "c")] |>
  _[, identification := stringr::str_replace_all(identification, "fevdfd", "f")] |>
  tidyfast::dt_pivot_wider(names_from = identification, values_from = wavg_fevdfd) |>
  gt(rowname_col = "response") |>
  fmt_percent(decimals = 1) |>
  sub_missing() |>
  tab_footnote(
    footnote = "c = Cholesky, f = FEVDFD, hr = Heteroskedasticity Regression",
  ) |>
  as_latex() |>
  stringr::str_remove("\\\\begin\\{table\\}\\[!t\\]") |>
  stringr::str_remove("\\\\end\\{table\\}")

t |>
  writeLines("./paper-figures/tables/fevfd-allfreqs.tex")

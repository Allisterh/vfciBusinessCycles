library(gt)
library(data.table)
library(vfciBCHelpers)

data <-
  list(
    fread("./data/paper-figures/charts/b-fevdfd.csv"),
    fread("./data/paper-figures/charts/b-chol.csv"),
    fread("./data/paper-figures/charts/b-hetreg.csv")
  ) |>
  purrr::list_rbind() |>
  copy()


t <- data |>
  _[, identification := stringr::str_replace_all(identification, "chol", "c")] |>
  _[, identification := stringr::str_replace_all(identification, "fevdfd", "f")] |>
  tidyfast::dt_pivot_wider(names_from = identification, values_from = weight) |>
  gt(rowname_col = "variable") |>
  fmt_number(decimals = 2) |>
  sub_missing() |>
  tab_footnote(
    footnote = "c = Cholesky, f = FEVDFD, hr = Heteroskedasticity Regression",
  ) |>
  as_latex() |>
  stringr::str_remove("\\\\begin\\{table\\}\\[!t\\]") |>
  stringr::str_remove("\\\\end\\{table\\}")

t |>
  writeLines("./paper-figures/tables/b.tex")

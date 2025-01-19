library(gt)
library(data.table)
library(vfciBCHelpers)

data <-
  list(
    fread("./data/paper-figures/charts/hs-fevdfd.csv"),
    fread("./data/paper-figures/charts/hs-chol.csv"),
    fread("./data/paper-figures/charts/hs-hetreg.csv")
  ) |>
  purrr::list_rbind()

t <-
  data |>
  _[, identification := stringr::str_replace_all(identification, "chol", "c")] |>
  _[, identification := stringr::str_replace_all(identification, "fevdfd", "f")] |>
  tidyfast::dt_pivot_wider(names_from = identification, values_from = hs) |>
  _[, -"date"] |>
  cor(use = "pairwise.complete.obs") |>
  as.data.table(keep.rownames = TRUE) |>
  gt(rowname_col = "rn") |>
  fmt_number(decimals = 2) |>
  tab_footnote(
    footnote = "c = Cholesky, f = FEVDFD, hr = Heteroskedasticity Regression",
  ) |>
  as_latex() |>
  stringr::str_remove("\\\\begin\\{table\\}\\[!t\\]") |>
  stringr::str_remove("\\\\end\\{table\\}") |>
  stringr::str_remove("\\\\fontsize.*\\\\selectfont")

t |>
  writeLines("./paper-figures/tables/hs-correlations.tex")

library(gt)
library(tidyfast)

vars1 <- c("vfci", "unemployment", "output", "consumption", "investment", "hours_worked")
vars2 <- c("interest", "inflation", "labor_share", "productivity", "TFP")
vars <- c(vars1, vars2)

corr_dt <- fread("./data/paper-figures/tables/hs-cross-correlations.csv")

corr_tb <-
  corr_dt[, .(var_y, var_x, corr)] |>
  dt_pivot_wider(names_from = var_x, values_from = corr)

t1 <- corr_tb |>
  _[, c("var_y", vars), with = FALSE] |>
  _[, var_y := factor(var_y, levels = vars, ordered = TRUE)] |>
  _[, 1:7] |>
  setorder(var_y) |>
  gt(
    rowname_col = "var_y"
  ) |>
  fmt_number(
    columns = 2:7,
    decimals = 2
  )

t2 <- corr_tb |>
  _[, c("var_y", vars), with = FALSE] |>
  _[, var_y := factor(var_y, levels = vars, ordered = TRUE)] |>
  _[, c(1, 8:12)] |>
  setorder(var_y) |>
  gt(
    rowname_col = "var_y"
  ) |>
  fmt_number(
    columns = 2:6,
    decimals = 2
  )

t1 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-figures/tables/hs-cross-correlations/hs-cross-correlations-1.tex")

t2 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-figures/tables/hs-cross-correlations/hs-cross-correlations-2.tex")

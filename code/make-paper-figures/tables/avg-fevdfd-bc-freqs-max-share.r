library(gt)
library(tidyfast)

vars1 <- c("vfci", "unemployment", "output", "consumption", "investment", "hours_worked")
vars2 <- c("interest", "inflation", "labor_share", "productivity", "TFP")
vars <- c(vars1, vars2)

bc_fevdfd <- fread("./data/paper-figures/tables/avg-fevdfd-bc-freqs-max-share.csv")

fevdfd_tb <-
  bc_fevdfd |>
  _[, .(response, target, avg_fevdfd)] |>
  dt_pivot_wider(names_from = response, values_from = avg_fevdfd)

t1 <-
  fevdfd_tb |>
  _[, c("target", vars), with = FALSE] |>
  _[, target := factor(target, levels = vars, ordered = TRUE)] |>
  setorder(target) |>
  _[, c(1:7)] |>
  gt() |>
  fmt_percent(
    columns = 2:7,
    decimals = 0
  )

t2 <-
  fevdfd_tb |>
  _[, c("target", vars), with = FALSE] |>
  _[, target := factor(target, levels = vars, ordered = TRUE)] |>
  setorder(target) |>
  _[, c(1, 8:12)] |>
  gt() |>
  fmt_percent(
    columns = 2:6,
    decimals = 0
  )

t1 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-figures/tables/avg-fevdfd-bc-freqs-max-share/avg-fevdfd-bc-freqs-max-share-1.tex")

t2 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-figures/tables/avg-fevdfd-bc-freqs-max-share/avg-fevdfd-bc-freqs-max-share-2.tex")

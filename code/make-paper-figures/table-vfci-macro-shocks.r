## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(gt)
library(tidyfast)

source("./code/vfci-bc/target-all-var-bc-freqs.R")

vars1 <- c("vfci", "unemployment", "output", "consumption", "investment", "hours_worked")
vars2 <- c("interest", "inflation", "labor_share", "productivity", "TFP")
vars <- c(vars1, vars2)

#####

corr_dt[target_variable.x == "vfci", .(target_variable.y, corr)]

corr_tb <-
  corr_dt[, .(target_variable.y, target_variable.x, corr)] |>
  dt_pivot_wider(names_from = target_variable.x, values_from = corr)

t1 <- corr_tb |>
  _[, c("target_variable.y", vars), with = FALSE] |>
  _[, target_variable.y := factor(target_variable.y, levels = vars, ordered = TRUE)] |>
  _[, 1:7] |>
  gt(
    rowname_col = "target_variable.y"
  ) |>
  fmt_number(
    columns = 2:7,
    decimals = 2
  )

t2 <- corr_tb |>
  _[, c("target_variable.y", vars), with = FALSE] |>
  _[, target_variable.y := factor(target_variable.y, levels = vars, ordered = TRUE)] |>
  _[, c(1, 8:12)] |>
  gt(
    rowname_col = "target_variable.y"
  ) |>
  fmt_number(
    columns = 2:6,
    decimals = 2
  )

t1 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-Overleaf/tables/all_var_corr_table1.tex")

t2 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-Overleaf/tables/all_var_corr_table2.tex")


#####

bc_fevdfd <-
  fevdfd_dt[f %between% bc_freqs, .(
    avg_bc_fevdfd = mean(fevdfd)
  ), by = .(response, target_variable, impulse)]

fevdfd_tb <-
  bc_fevdfd |>
  _[impulse == "Main"] |>
  _[, .(response, target_variable, avg_bc_fevdfd)] |>
  dt_pivot_wider(names_from = response, values_from = avg_bc_fevdfd)

t1 <-
  fevdfd_tb |>
  _[, c("target_variable", vars), with = FALSE] |>
  _[, target_variable := factor(target_variable, levels = vars, ordered = TRUE)] |>
  setorder(target_variable) |>
  _[, c(1:7)] |>
  gt(
    rowname_col = "target_variable"
  ) |>
  fmt_percent(
    columns = 2:7,
    decimals = 0
  )

t2 <-
  fevdfd_tb |>
  _[, c("target_variable", vars), with = FALSE] |>
  _[, target_variable := factor(target_variable, levels = vars, ordered = TRUE)] |>
  setorder(target_variable) |>
  _[, c(1, 8:12)] |>
  gt(
    rowname_col = "target_variable"
  ) |>
  fmt_percent(
    columns = 2:6,
    decimals = 0
  )

t1 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-Overleaf/tables/all_var_fevdfd_table1.tex")

t2 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-Overleaf/tables/all_var_fevdfd_table2.tex")

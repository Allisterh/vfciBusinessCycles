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

t <- corr_tb |>
  gt(
    rowname_col = "target_variable.y"
  ) |>
  fmt_number(
    columns = 2:12,
    decimals = 3
  ) |>
  tab_header(
    title = "Correlation Matrix of Each Variable-targeted Shocks"
  ) |>
  tab_options(table.font.size = 10)

t |>
  as_latex() |>
  stringr::str_replace("caption\\*", "caption") |>
  write(file = "./paper-Overleaf/tables/all_var_corr_table.tex")


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

t <-
  fevdfd_tb |>
  gt(
    rowname_col = "target_variable"
  ) |>
  fmt_percent(
    columns = 2:12,
    decimals = 0
  ) |>
  tab_header(
    title = "Average BC Frequency FEVD of Each Variable Targeted Shock"
  ) |>
  tab_options(table.font.size = 10)

t |>
  as_latex() |>
  stringr::str_replace("caption\\*", "caption") |> cat()
  write(file = "./paper-Overleaf/tables/all_var_fevdfd_table.tex")

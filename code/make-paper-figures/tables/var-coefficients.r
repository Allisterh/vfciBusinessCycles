## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(gt)
library(tidyfast)
library(data.table)

coef_dt <- fread("./data/paper-figures/tables/var_estimated_coefficients.csv")

vars1 <- c("vfci", "unemployment", "output")
vars2 <- c("consumption", "investment", "hours_worked")
vars3 <- c("interest", "inflation", "labor_share")
vars4 <- c("productivity", "TFP")
vars <- c(vars1, vars2, vars3, vars4)

rn_order <- c(
  "const",
  "vfci.l1",
  "vfci.l2",
  "unemployment.l1",
  "unemployment.l2",
  "interest.l1",
  "interest.l2",
  "inflation.l1",
  "inflation.l2",
  "output.l1",
  "output.l2",
  "investment.l1",
  "investment.l2",
  "consumption.l1",
  "consumption.l2",
  "hours_worked.l1",
  "hours_worked.l2",
  "labor_share.l1",
  "labor_share.l2",
  "TFP.l1",
  "TFP.l2",
  "productivity.l1",
  "productivity.l2"
)

coef_dt |>
  _[, value := paste0(round(estimate, 3), " (", round(sd, 3), ")")] |>
  _[, independent := factor(independent, rn_order, ordered = TRUE)]

setorder(coef_dt, independent)

t1 <-
  coef_dt |>
  _[dependent %in% vars1] |>
  _[, .(independent, dependent, value)] |>
  dt_pivot_wider(names_from = dependent, values_from = value) |>
  gt(
    rowname_col = "independent"
  )

t1 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-figures/tables/var-coefficients/var-coefficients-1.tex")

#####

t2 <-
  coef_dt |>
  _[dependent %in% vars2] |>
  _[, .(independent, dependent, value)] |>
  dt_pivot_wider(names_from = dependent, values_from = value) |>
  gt(
    rowname_col = "independent"
  )

t2 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-figures/tables/var-coefficients/var-coefficients-2.tex")

#####

t3 <-
  coef_dt |>
  _[dependent %in% vars3] |>
  _[, .(independent, dependent, value)] |>
  dt_pivot_wider(names_from = dependent, values_from = value) |>
  gt(
    rowname_col = "independent"
  )

t3 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-figures/tables/var-coefficients/var-coefficients-3.tex")

#####

t4 <-
  coef_dt |>
  _[dependent %in% vars4] |>
  _[, .(independent, dependent, value)] |>
  dt_pivot_wider(names_from = dependent, values_from = value) |>
  gt(
    rowname_col = "independent"
  )

t4 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-figures/tables/var-coefficients/var-coefficients-4.tex")

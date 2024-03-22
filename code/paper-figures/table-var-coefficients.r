## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(gt)
library(tidyfast)

source("./code/vfci-bc/target-all-var-bc-freqs.R")

vars1 <- c("vfci", "unemployment", "output")
vars2 <- c("consumption", "investment", "hours_worked")
vars3 <- c("interest", "inflation", "labor_share")
vars4 <- c( "productivity", "TFP")
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

coef_v <- coefficients(v)

coef_dt <-
  rbindlist(lapply(vars, function(x) {
    dt <- as.data.table(coef_v[[x]], keep.rownames = TRUE)
    dt$`dep_var` <- x
    dt
  })) |>
  _[, value := paste0(round(Estimate, 3), " (", round(`Std. Error`, 3), ")")] |>
  _[, rn := factor(rn, rn_order, ordered = TRUE)]

setorder(coef_dt, rn)

t1 <- 
  coef_dt |>
  _[dep_var %in% vars1] |>
  _[, .(rn, dep_var, value)] |>
  dt_pivot_wider(names_from = dep_var, values_from = value) |>
  gt(
    rowname_col = "rn"
  )

t1 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-Overleaf/tables/var-coefficients1.tex")

#####

t2 <- 
  coef_dt |>
  _[dep_var %in% vars2] |>
  _[, .(rn, dep_var, value)] |>
  dt_pivot_wider(names_from = dep_var, values_from = value) |>
  gt(
    rowname_col = "rn"
  )

t2 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-Overleaf/tables/var-coefficients2.tex")

#####

t3 <- 
  coef_dt |>
  _[dep_var %in% vars3] |>
  _[, .(rn, dep_var, value)] |>
  dt_pivot_wider(names_from = dep_var, values_from = value) |>
  gt(
    rowname_col = "rn"
  )

t3 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-Overleaf/tables/var-coefficients3.tex")

#####

t4 <- 
  coef_dt |>
  _[dep_var %in% vars4] |>
  _[, .(rn, dep_var, value)] |>
  dt_pivot_wider(names_from = dep_var, values_from = value) |>
  gt(
    rowname_col = "rn"
  )

t4 |>
  as_latex() |>
  stringr::str_extract("\\\\begin\\{longtable\\}[\\S\\s]*\\\\end\\{longtable\\}") |>
  write(file = "./paper-Overleaf/tables/var-coefficients4.tex")

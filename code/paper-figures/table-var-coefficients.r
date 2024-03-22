## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(gt)
library(xtable)
library(tidyfast)

source("./code/vfci-bc/target-all-var-bc-freqs.R")

vars1 <- c("vfci", "unemployment", "output", "consumption", "investment", "hours_worked")
vars2 <- c("interest", "inflation", "labor_share", "productivity", "TFP")
vars <- c(vars1, vars2)

coef_v <- coefficients(v)

coef_dt <-
  rbindlist(lapply(vars, function(x) {
    dt <- as.data.table(coef_v[[x]], keep.rownames = TRUE)
    dt$`Dependent Variable` <- x
    setnames(dt, "rn", "Independent Variable")
    dt
  }))


print(xtable(coef_dt), floating = FALSE)

t <-
  coef_dt |>
  gt(
    groupname_col = "Dependent Variable",
    rowname_col = "Independent Variable"
  ) |>
  fmt_number(
    columns = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
    decimals = 4
  ) |>
  tab_header(
    title = "Estimated Coefficients on all VAR Variables"
  ) |>
  tab_footnote(
    "''.l1' represents the first lag. '.l2' represents the second lag."
  )

t |>
  as_latex() |> cat()
  stringr::str_replace("caption\\*", "caption") |>
  write(file = "./paper-Overleaf/tables/var-coefficients.tex")

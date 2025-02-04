library(data.table)
library(ggplot2)
library(broom)
library(broom.helpers)

#####
## Macro VFCI
data <- fread("./data/paper-figures/charts/mean-vol-estimate-macro-var.csv")

p <-
  data |>
  _[variable == "output"] |>
  ggplot(aes(
    x = exp(log_var_fitted / 2),
    y = fitted
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, ) +
  labs(
    x = "Log Var Fitted",
    y = "Mean Fitted"
  ) +
  theme_paper

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/mean-vol-int-macro-vfci-output.pdf",
  p, width = 5.5, height = 4, units = "in"
)

## Mean and Vol Regression
reg_table <-
  data |>
  _[variable == "output"] |>
  lm(fitted ~ exp(log_var_fitted / 2), data = _) |>
  gtsummary::tbl_regression() |>
  gtsummary::add_glance_source_note(include = c(r.squared)) |>
  gtsummary::as_gt() |>
  gt::as_latex() |>
  stringr::str_remove("\\\\begin\\{table\\}\\[!t\\]") |>
  stringr::str_remove("\\\\end\\{table\\}")

writeLines(reg_table, "./paper-figures/tables/mean-vol-regression-output-macro.tex")


## Mean and Vol across Time
p <-
  data |>
  _[variable == "output"] |>
  ggplot(aes(
    x = date
  )) +
  geom_line(aes(y = fitted, color = "fitted mean")) +
  geom_line(aes(y = exp(log_var_fitted / 2), color = "vol fitted")) +
  theme_paper +
  theme(legend.position = c(0.2, 0.8))

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/mean-vol-across-time-macro.pdf",
  p, width = 5.5, height = 4, units = "in"
)

## At Risk and Not at Risk
p <-
  data |>
  _[variable == "output"] |>
  ggplot(aes(
    x = date
  )) +
  geom_line(aes(y = fitted - 2 * exp(log_var_fitted / 2), color = "At Risk")) +
  geom_line(aes(y = fitted + 2 * exp(log_var_fitted / 2), color = "Not at Risk")) +
  theme_paper +
  theme(legend.position = c(0.2, 0.8))

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/mean-vol-at-risk-not-at-risk-macro.pdf",
  p, width = 5.5, height = 4, units = "in"
)


#####
## Macro VFCI
data <- fread("./data/paper-figures/charts/mean-vol-estimate-fin-var.csv")

p <-
  data |>
  _[variable == "output"] |>
  ggplot(aes(
    x = exp(log_var_fitted / 2),
    y = fitted
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, ) +
  labs(
    x = "Log Var Fitted",
    y = "Mean Fitted"
  ) +
  theme_paper

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/mean-vol-int-fin-vfci-output.pdf",
  p, width = 5.5, height = 4, units = "in"
)

## Mean and Vol Regression
reg_table <-
  data |>
  _[variable == "output"] |>
  lm(fitted ~ exp(log_var_fitted / 2), data = _) |>
  gtsummary::tbl_regression() |>
  gtsummary::add_glance_source_note(include = c(r.squared)) |>
  gtsummary::as_gt() |>
  gt::as_latex() |>
  stringr::str_remove("\\\\begin\\{table\\}\\[!t\\]") |>
  stringr::str_remove("\\\\end\\{table\\}")

writeLines(reg_table, "./paper-figures/tables/mean-vol-regression-output-fin.tex")


## Mean and Vol across Time
p <-
  data |>
  _[variable == "output"] |>
  ggplot(aes(
    x = date
  )) +
  geom_line(aes(y = fitted, color = "fitted mean")) +
  geom_line(aes(y = exp(log_var_fitted / 2), color = "vol fitted")) +
  theme_paper +
  theme(legend.position = c(0.2, 0.8))

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/mean-vol-across-time-fin.pdf",
  p, width = 5.5, height = 4, units = "in"
)

## At Risk and Not at Risk
p <-
  data |>
  _[variable == "output"] |>
  ggplot(aes(
    x = date
  )) +
  geom_line(aes(y = fitted - 2 * exp(log_var_fitted / 2), color = "At Risk")) +
  geom_line(aes(y = fitted + 2 * exp(log_var_fitted / 2), color = "Not at Risk")) +
  theme_paper +
  theme(legend.position = c(0.2, 0.8))

p + theme_bw(base_size = 20)

ggsave(
  "./paper-figures/charts/mean-vol-at-risk-not-at-risk-fin.pdf",
  p, width = 5.5, height = 4, units = "in"
)

## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)
library(tidyfast)
library(data.table)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/vfci-bc/vfci-cholesky.R")
source("./code/paper-figures/theme-paper.r")

#####
p_data <-
  rbindlist(list(
    shock_dt[, method := "max_share"],
    cv_shock_dt[, method := "chol"]
  )) |>
  dt_pivot_wider(names_from = method, values_from = hs)


p <-
  p_data |>
  _[impulse == "Main"] |>
  _[, x_loc := min(chol), by = .(target_variable)] |>
  _[, y_loc := max(max_share), by = .(target_variable)] |>
  _[target_variable == "unemployment", chol := chol * -1] |>
  _[, corr := cor(max_share, chol), by = .(impulse, target_variable)] |>
  ggplot(aes(
    x = max_share,
    y = chol
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(
    vars(target_variable),
    nrow = 4,
    scales = "free"
  ) +
  geom_text(aes(
    x = x_loc,
    y = y_loc,
    label = round(corr, 3)
  ), hjust = 0) +
  theme_classic()

p

ggsave(
  "./paper-Overleaf/figs/corr-max-share-chol.pdf",
  p, width = 7, height = 7, units = "in"
)


#####
p_data <-
  rbindlist(list(
    contr_dt[, method := "max_share"][, response := as.character(response)],
    cv_contr_dt[, method := "chol"]
  ))

p_data |>
  _[impulse == "Main"] |>
  _[response == "unemployment"] |>
  ggplot(aes(
    x = date,
    y = hd,
    color = method
  )) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(
    vars(paste(
      "Target: ", target_variable,
      "Response: ", response)),
    nrow = 4,
    scales = "free"
  )


p_data |>
  _[impulse == "Main"] |>
  ggplot(aes(
    x = date,
    y = hd,
    color = method
  )) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(
    vars(paste(
      "Target: ", target_variable,
      "Response: ", response)),
    scales = "free",
    nrow = 11
  )

## as a scatter

p_data |>
  _[impulse == "Main"] |>
  _[,.(t, date, impulse, response, target_variable, method, hd)] |>
  dt_pivot_wider(names_from = method, values_from = hd) |>
  ggplot(aes(
    x = chol,
    y = max_share
  )) +
  geom_point() +
  facet_wrap(
    vars(paste(
      "Target: ", target_variable,
      "Response: ", response)),
    scales = "free",
    nrow = 11
  )

cv_fevdfd_dt |>
  _[impulse == "Main"] |>
  _[f <= pi] |>
  _[response == target_variable] |>
  ggplot(aes(
    x = f,
    y = fevdfd
  )) +
  geom_vline(xintercept = 2 * pi / 6) +
  geom_vline(xintercept = 2 * pi / 32) +
  geom_line() +
  facet_wrap(vars(response))

library(ggplot2)
library(data.table)
library(vfciBCHelpers)

data <- fread("./data/paper-figures/charts/mean-vol-estimate-var.csv")

p <-
  data |>
  _[, variable := factor(variable, levels = variable_labels, labels = labels(variable_labels), ordered = TRUE)] |>
  ggplot(aes(
    x = scale(log_var_fitted),
    y = fitted
  )) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(
    vars(variable),
    scales = "free",
    nrow = 3
  ) +
  labs(
    y = "Condtional Mean",
    x = "Conditional Volatility"
  ) +
  theme_paper

p

ggsave(
  "./paper-figures/charts/mean-vol-estimate-var.pdf",
  p, width = 5.5, height = 4, units = "in"
)

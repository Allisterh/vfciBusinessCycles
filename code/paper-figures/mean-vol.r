## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/calc-mean-vol.R")
source("./code/paper-figures/theme-paper.r")

p <-
  data |>
  _[, variable := factor(variable, levels = var_labels, labels = labels(var_labels), ordered = T)] |>
  ggplot(aes(
    x = scale(fitted_log_var),
    y = fitted_adj
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
  "./paper-Overleaf/figs/mean-vol.pdf",
  p, width = 5.5, height = 4, units = "in"
)

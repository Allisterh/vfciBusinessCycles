## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/calc-mean-vol.R")

p <-
  data |>
  ggplot(aes(
    x = scale(fitted_log_var),
    y = fitted_adj
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(
    vars(variable),
    scales = "free",
    nrow = 4
  ) +
  labs(
    y = "Condtional Mean",
    x = "Conditional Volatility"
  ) +
  theme_classic()

ggsave(
  "./paper-Overleaf/figs/mean-vol.pdf",
  p, width = 5, height = 5, units = "in"
)

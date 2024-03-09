## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")


p <-
  cross_dt |>
  _[target_variable.y == "unemployment"] |>
  _[target_variable.x == "vfci"] |>
  ggplot(aes(
    x = hs.x,
    y = hs.y
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "VFCI-targeted Shock",
    y = "Unemployment-targeted Shock"
  ) +
  theme_classic()

p + theme_classic(base_size = 30)

ggsave(
  "./paper-Overleaf/figs/corr-vfci-unemp-shocks.pdf",
  p, width = 2, height = 2, units = "in"
)

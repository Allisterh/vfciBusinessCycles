## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/paper-figures/theme-paper.r")

p <-
  cross_dt |>
  _[target_variable.y == "unemployment"] |>
  _[target_variable.x == "vfci"] |>
  ggplot(aes(
    x = hs.x,
    y = hs.y
  )) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(
    x = "VFCI-targeted",
    y = "Unemployment-targeted"
  ) +
  theme_paper

p

ggsave(
  "./paper-Overleaf/figs/corr-vfci-unemp-shocks.pdf",
  p, width = 2, height = 2, units = "in"
)

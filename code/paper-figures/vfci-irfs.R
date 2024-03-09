## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")

p <-
  irf_dt |>
  _[impulse == "Main"] |>
  _[target_variable == "vfci"] |>
  ggplot(aes(
     x = h,
    y = irf,
    color = target_variable
  )) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(
    vars(response),
    scales = "free_y",
    nrow = 4
  ) +
  theme_classic() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.875, 0.125)
  )

p

ggsave(
  "./paper-Overleaf/figs/irf-vfci-max-share.pdf",
  p, width = 5, height = 5, units = "in"
)

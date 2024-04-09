## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")

bc_targets <- c("vfci", "unemployment", "output", "investment", "consumption", "hours_worked")

p <-
  irf_dt |>
  _[impulse == "Main"] |>
  _[target_variable %in% bc_targets] |>
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
  theme(legend.position = c(0.875, 0.125))

p

ggsave(
  "./paper-Overleaf/figs/irf-vfci-macro-shocks.pdf",
  p, width = 5, height = 5, units = "in"
)

## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/paper-figures/theme-paper.r")


p <-
  irf_dt |>
  _[impulse == "Main"] |>
  _[target_variable %in% c("vfci", "unemployment")] |>
  _[, response := factor(response, levels = var_labels, labels = labels(var_labels), ordered = T)] |>
  ggplot(aes(
    x = h,
    y = irf,
    color = target_variable
  )) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line() +
  facet_wrap(
    vars(response),
    scales = "free_y",
    nrow = 3
  ) +
  labs(
    x = "Horizon (quarters)",
    y = "Impulse Response Function"
  ) +
  scale_color_manual(
    values = c(vfci = "black", unemployment = "goldenrod"),
    labels = c(vfci = "VFCI", unemployment = "Unemployment"),
    name = "Max-share Target"
  ) +
  theme_paper +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.125)
  )

p

ggsave(
  "./paper-Overleaf/figs/irf-vfci-unemp-max-share.pdf",
  p, width = 5.5, height = 4, units = "in"
)

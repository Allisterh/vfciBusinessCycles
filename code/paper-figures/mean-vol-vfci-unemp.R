## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)
library(patchwork)

source("./code/vfci-bc/calc-mean-vol.R")
source("./code/paper-figures/theme-paper.r")


make_plot <- function(x) {
  data |>
    _[variable == x] |>
    ggplot(aes(
      x = scale(fitted_log_var),
      y = fitted_adj
    )) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      y = "Condtional Mean",
      x = "Conditional Volatility",
      title = labels(var_labels[var_labels == x])
    ) +
    theme_paper +
    theme(
      plot.title = element_text(hjust = 0.5, size = 9)
    )
}

p_l <- purrr::map(c("vfci", "unemployment"), make_plot)

p <- wrap_plots(p_l)

p

ggsave(
  "./paper-Overleaf/figs/mean-vol-vfci-unemp.pdf",
  p, width = 5, height = 2.5, units = "in"
)

## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)
library(patchwork)

source("./code/vfci-bc/calc-mean-vol.R")

data

make_plot <- function(x) {
  data |>
    _[variable == x] |>
    ggplot(aes(
      x = scale(fitted_log_var),
      y = fitted_adj
    )) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(
      vars(variable),
      scales = "free",
      nrow = 1
    ) +
    labs(
      y = "Condtional Mean",
      x = "Conditional Volatility"
    ) +
    theme_classic()
}

p_l <- purrr::map(c("vfci", "unemployment"), make_plot)

p <- wrap_plots(p_l)

ggsave(
  "./paper-Overleaf/figs/mean-vol-vfci-unemp.pdf",
  p, width = 5, height = 3, units = "in"
)

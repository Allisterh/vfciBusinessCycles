## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")

other_vars <- unique(cross_dt$target_variable.x)[unique(cross_dt$target_variable.x) != "vfci"]

make_plot <- function(x) {
  cross_dt |>
  _[target_variable.y == x] |>
  _[target_variable.x == "vfci"] |>
  ggplot(aes(
    x = hs.x,
     y = hs.y
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "VFCI",
    y = x
  ) +
  theme_classic()
}

p_l <- purrr::map(other_vars, make_plot)

p <- wrap_plots(p_l, nrow = 4)

ggsave(
  "./paper-Overleaf/figs/corr-vfci-macro-shocks.pdf",
  p, width = 5, height = 5, units = "in"
)

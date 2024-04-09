## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)
library(patchwork)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/paper-figures/theme-paper.r")

other_vars <- unique(cross_dt$target_variable.x)[unique(cross_dt$target_variable.x) != "vfci"]
other_vars <- factor(other_vars, levels = var_labels, ordered = T) |> sort()

make_plot <- function(x) {
  cross_dt |>
  _[target_variable.y == x] |>
  _[target_variable.x == "vfci"] |>
  ggplot(aes(
    x = hs.x,
     y = hs.y
  )) +
  geom_point(size = 0.5) +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "VFCI",
    y = labels(var_labels[x == var_labels])
  ) +
  theme_paper
}

p_l <- purrr::map(other_vars, make_plot)

p <- wrap_plots(p_l, nrow = 3)

p

ggsave(
  "./paper-Overleaf/figs/corr-vfci-macro-shocks.pdf",
  p, width = 5.5, height = 4, units = "in"
)

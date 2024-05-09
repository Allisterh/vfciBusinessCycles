## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)
library(patchwork)
library(data.table)
library(vfciBCHelpers)

cross_dt <- fread("./data/paper-figures/charts/hs-cross-max-share.csv")

other_vars <-
  factor(variable_labels[variable_labels != "vfci"], levels = variable_labels, ordered = TRUE) |>
  sort()

make_plot <- function(x) {
  target_y <- vfci <- hs_y <- NULL

  cross_dt |>
    _[target_y == x] |>
    ggplot(aes(
      x = vfci,
      y = hs_y
    )) +
    geom_point(size = 0.5) +
    geom_vline(xintercept = 0, color = "gray50") +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "VFCI",
      y = labels(variable_labels[x == variable_labels])
    ) +
    theme_paper
}

p_l <- purrr::map(other_vars, make_plot)

p <- wrap_plots(p_l, nrow = 3)

p

ggsave(
  "./paper-figures/charts/hs-cross-max-share.pdf",
  p, width = 5.5, height = 4, units = "in"
)

library(ggplot2)
library(patchwork)
library(data.table)
library(vfciBCHelpers)

data <- fread("./data/paper-figures/charts/mean-vol-estimate-var.csv")

make_plot <- function(x) {
  variable <- log_var_fitted <- fitted <- NULL
  data |>
    _[variable == x] |>
    ggplot(aes(
      x = scale(log_var_fitted),
      y = fitted
    )) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      y = "Condtional Mean",
      x = "Conditional Volatility",
      title = labels(variable_labels[variable_labels == x])
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
  "./paper-figures/charts/mean-vol-estimate-var-vfci-unemployment.pdf",
  p, width = 5, height = 2.5, units = "in"
)

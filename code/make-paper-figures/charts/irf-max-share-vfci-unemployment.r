## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)
library(data.table)
library(vfciBCHelpers)

data <- fread("./data/paper-figures/charts/irf-max-share-vfci-unemployment.csv")

p <-
  data |>
  _[, response := factor(
    response,
    levels = variable_labels,
    labels = labels(variable_labels),
    ordered = TRUE
  )] |>
  ggplot(aes(
    x = h,
    y = irf,
    color = target
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
  "./paper-figures/charts/irf-max-share-vfci-unemployment.pdf",
  p, width = 5.5, height = 4, units = "in"
)

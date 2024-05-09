## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)
library(data.table)
library(vfciBCHelpers)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/vfci-bc/vfci-cholesky.R")
source("./code/paper-figures/theme-paper.r")

data <- fread("./data/paper-figures/charts/irf-chol-vfci-max-share-vfci.csv")

#####
data[, response :=
  factor(
    response,
    levels = variable_labels,
    labels = labels(variable_labels),
    ordered = TRUE
  )]

p <-
  ggplot() +
  geom_hline(yintercept = 0, color = "gray") +
  geom_line(
    data = data[identification == "fevdfd"],
    aes(
      x = h,
      y = irf,
      color = "max_share"
    )
  ) +
  geom_line(
    data = data[identification == "chol"],
    aes(
      x = h,
      y = irf,
      color = "chol"
    )
  ) +
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
    values = c(max_share = "black", chol = "lightblue3"),
    labels = c(max_share = "Max-share", chol = "Cholesky"),
    name = "Identification Method"
  ) +
  theme_paper +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.125)
  )

p

ggsave(
  "./paper-figures/charts/irf-chol-vfci-max-share-vfci.pdf",
  p, width = 5.5, height = 4, units = "in"
)

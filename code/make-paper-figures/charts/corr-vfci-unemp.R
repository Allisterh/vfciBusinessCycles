## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)
library(data.table)
library(vfciBCHelpers)

data <- fread("./data/paper-figures/charts/hs-max-share-vfci-unemployment.csv")

p <-
  data |>
  ggplot(aes(
    x = vfci,
    y = unemployment
  )) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "VFCI-targeted",
    y = "Unemployment-targeted"
  ) +
  theme_paper

p

ggsave(
  "./paper-figures/charts/hs-max-share-vfci-unemployment.pdf",
  p, width = 2, height = 2, units = "in"
)

library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/paper-figures/get-recession-dt.R")

recessions <- recessions |>
  _[start %between% as.Date(c("1962-01-01", "2019-01-01"))]


p <-
  ggplot() +
  geom_rect(
    data = recessions,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = +Inf
    ),
    fill = "gray70",
    color = NA,
    alpha = 0.5
  ) +
  geom_hline(yintercept = mean(data$vfci)) +
  geom_line(
    data = data,
    aes(
      x = date,
      y = vfci
    )
  ) +
  theme_classic()

ggsave(
  "./paper-Overleaf/figs/vfci-time-series.pdf",
  p, width = 5, height = 2, units = "in"
)

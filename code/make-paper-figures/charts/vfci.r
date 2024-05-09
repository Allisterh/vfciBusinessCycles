library(ggplot2)
library(data.table)
library(vfciBCHelpers)

data <- fread("./data/paper-figures/charts/vfci.csv")

recessions <- get_recession_dt() |>
  _[start %between% as.Date(c("1962-01-01", "2021-01-01"))]


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
  geom_hline(
    yintercept = mean(data$vfci),
    color = "gray50"
  ) +
  geom_line(
    data = data,
    aes(
      x = date,
      y = vfci
    )
  ) +
  scale_x_date(
    breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
    labels = seq(1960, 2020, by = 10)
  ) +
  labs(
    x = NULL,
    y = "VFCI"
  ) +
  theme_paper

ggsave(
  "./paper-figures/charts/vfci.pdf",
  p, width = 5, height = 2, units = "in"
)

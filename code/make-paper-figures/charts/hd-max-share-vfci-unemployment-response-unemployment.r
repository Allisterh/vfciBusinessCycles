library(ggplot2)
library(data.table)
library(vfciBCHelpers)

recessions <- get_recession_dt() |>
  _[start %between% as.Date(c("1962-01-01", "2019-01-01"))]

p_data <-
  fread("./data/paper-figures/charts/hd-max-share-vfci-unemployment-response-unemployment.csv")

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
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line(
    data = p_data,
    aes(
      x = date,
      y = hd,
      color = target
    )
  ) +
  scale_x_date(
    breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
    labels = seq(1960, 2020, 10)
  ) +
  labs(
    x = "",
    y = "Contriboution to\nUnemployment"
  ) +
  scale_color_manual(
    values = c(vfci = "black", unemployment = "goldenrod"),
    labels = c(vfci = "VFCI", unemployment = "Unemployment"),
    name = "Max-share Target"
  ) +
  theme_paper +
  theme(
    legend.position = "top"
  )

p

ggsave(
  "./paper-figures/charts/hd-max-share-vfci-unemployment-response-unemployment.pdf",
  p, width = 5, height = 2.5, units = "in"
)

library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/paper-figures/get-recession-dt.R")
source("./code/paper-figures/theme-paper.r")

recessions <- recessions |>
  _[start %between% as.Date(c("1962-01-01", "2019-01-01"))]

p_data <- contr_dt |>
  _[impulse == "Main"] |>
  _[response == "unemployment"] |>
  _[target_variable == "vfci"]

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
      y = hd
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
  theme_paper

p

ggsave(
  "./paper-Overleaf/figs/vfci-contribution-unemployment.pdf",
  p, width = 5, height = 2, units = "in"
)

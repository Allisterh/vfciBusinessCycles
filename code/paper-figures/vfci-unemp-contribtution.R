library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/paper-figures/get-recession-dt.R")

recessions <- recessions |>
  _[start %between% as.Date(c("1962-01-01", "2019-01-01"))]

p_data <- contr_dt |>
  _[impulse == "Main"] |>
  _[response == "unemployment"] |>
  _[target_variable %in% c("vfci", "unemployment")]

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
  geom_hline(yintercept = 0) +
  geom_line(
    data = p_data,
    aes(
      x = date,
      y = hd,
      color = target_variable
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "top"
  )

p

ggsave(
  "./paper-Overleaf/figs/vfci-unemployment-contribution-unemployment.pdf",
  p, width = 5, height = 2, units = "in"
)

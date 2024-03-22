library(ggplot2)
theme_paper <-
  theme_classic(base_size = 10) +
  theme(
    panel.background = element_rect(color = "black", linewidth = 1),
    axis.line = element_blank(),
    strip.background = element_blank(),
    legend.key = element_blank()
  )

var_labels <- c(
  VFCI = "vfci",
  Unemployment = "unemployment",
  `Fed Funds` = "interest",
  Inflation = "inflation",
  Output = "output",
  Investment = "investment",
  Consumption = "consumption",
  `Hours Worked` = "hours_worked",
  `Labor Share` = "labor_share",
  TFP = "TFP",
  `Labor Prod.` = "productivity"
)

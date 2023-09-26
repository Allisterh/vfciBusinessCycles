require(ggplot2)

theme_pres <-
    theme_bw(base_size = 10) +
    theme(
        axis.text = element_text(size = 6, margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 7),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, -10, 0),
        legend.text = element_text(size = 7),
        legend.key.size = unit(10, "pt"),
        plot.margin = margin(0, 0, 0, 0)
        )

var_order <- c(
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

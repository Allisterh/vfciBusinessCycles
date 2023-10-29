require(ggplot2)

outpath <- "./presentations/2023-11-01-Brown-Macro-Breakfast/figs/"

theme_pres <-
    theme_bw(base_size = 10) +
    theme(
        axis.text = element_text(size = 6, margin = margin(0, 0, 0, 0)),
        axis.title = element_text(size = 7),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 7),
        legend.key.size = unit(10, "pt"),
        plot.margin = margin(2, 2, 2, 2, "pt")
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

## Colors
vfci_color <- "steelblue"
u_color <- "darkorange"
inf_color <- "firebrick"
int_color <- "lightpink"

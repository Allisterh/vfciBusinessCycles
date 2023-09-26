require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

data <- fread("./data/vfciBC_data.csv")


plot <-
    data |>
    ggplot(aes(
        x = date,
        y = vfci,
        color = "vfci"
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    labs(
        x = NULL,
        y = "VFCI"
    ) +
    scale_color_manual(
        guide = "none",
        values = c(
            vfci = "steelblue"
        )
    ) +
    scale_x_date(
        breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
        labels = seq(1960, 2020, 10)
    ) +
    theme_pres

ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig2_vfci.pdf",
    plot,
    units = "in",
    width = 4,
    height = 1.5
    )

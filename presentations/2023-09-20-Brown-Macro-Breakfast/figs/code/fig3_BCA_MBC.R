require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

data <- fread("./data/classical_vfcibc_VAR_IRF.csv")


plot <-
    data[(
        target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 32
        ) | (
        target == "vfci" & sign == "neg" & period_l == 6 & period_h == 32)
        ] |>
    ggplot(aes(
        x = h,
        y = irf,
        color = target
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(
        vars(response),
        nrow = 3,
        scales = "free_y"
    ) +
    labs(
        x = NULL,
        y = "IRF"
    ) +
    scale_color_manual(
        values = c(
            vfci = "steelblue",
            unemployment = "goldenrod"
        ),
        labels = c(
            vfci = "VFCI",
            unemployment = "Unnemployment"
        )
    ) +
    theme_pres

ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig3_BCA_MBC.pdf",
    plot,
    units = "in",
    width = 4.75,
    height = 3
    )

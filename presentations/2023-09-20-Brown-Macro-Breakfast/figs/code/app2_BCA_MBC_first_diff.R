require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

data <- fread("./data/classical_vfcibc_VAR_IRF.csv")

data[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]

plot <-
    data[(
        target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 32
        ) | (
        target == "vfci" & sign == "neg" & period_l == 6 & period_h == 32),
        .(target, h, response, irf)
        ][,
            diff1 := irf - shift(irf, 1, type = "lag"), by = .(target, response)
        ] |>
    ggplot(aes(
        x = h,
        y = diff1,
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
    theme_pres +
    theme(legend.position = c(0.875, 0.15))
plot
ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/app2_BCA_MBC_first_diff.pdf",
    plot,
    units = "in",
    width = 4.75,
    height = 3
    )

require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

data <- fread("./data/residual-mbc-shock/irf.csv")

data[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]

plot <-
    data|>
    ggplot(aes(
        x = h,
        y = irf,
        color = "resid"
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
            resid = "mediumorchid"
        ),
        labels = c(
            resid = "Residual MBC"
        )
    ) +
    theme_pres +
    theme(legend.position = c(0.875, 0.15))
plot
ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig6_resid_MBC.pdf",
    plot,
    units = "in",
    width = 4.75,
    height = 3
    )

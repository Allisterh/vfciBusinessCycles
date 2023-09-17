require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

resid <- fread("./data/residual-mbc-shock/hist_shocks.csv")
data <- fread("./data/classical_vfcibc_hist_shocks.csv")

data <- rbindlist(list(
    data[target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 32, .(date, shock, model = "u0632")],
    data[target == "vfci" & sign == "neg" & period_l == 22 & period_h == 32, .(date, shock, model = "v2232")],
    resid[, .(date, shock, model = "resid")]
))

data[, model := factor(model, levels = c("u0632", "v2232", "resid"), ordered = TRUE)]

plot <-
    data|>
    ggplot(aes(
        x = date,
        y = shock,
        color = model
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    labs(
        x = NULL,
        y = "Historical Shock"
    ) +
    scale_color_manual(
        values = c(
            u0632 = "goldenrod",
            v2232 = "steelblue",
            resid = "mediumorchid"
        ),
        labels = c(
            u0632 = "MBC",
            v2232 = "VFCI 22:32",
            resid = "Residual MBC"
        )
    ) +
    theme_pres +
    theme(legend.position = c(0.75, 0.3))


ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig8_hist_shocks.pdf",
    plot,
    units = "in",
    width = 4.5,
    height = 2
    )

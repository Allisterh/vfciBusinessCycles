require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

resid_data <- fread("./data/residual-mbc-shock/weights.csv")
data <- fread("./data/classical_vfcibc_weights.csv")

data <- rbindlist(list(
    data[target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 32, .(variable, weight, model = "u0632")],
    data[target == "vfci" & sign == "neg" & period_l == 22 & period_h == 32, .(variable, weight, model = "v2232")],
    resid_data[, .(variable, weight, model = "resid")]
))

data[, model := factor(model, levels = c("u0632", "v2232", "resid"), ordered = TRUE)]
data[, variable := factor(variable, levels = arrange(data[model == "u0632"], weight)$variable, ordered = TRUE)]

plot <-
    data|>
    ggplot(aes(
        x = variable,
        y = weight,
        fill = model
    )) +
    geom_hline(yintercept = 0) +
    geom_col(
        width = 0.8,
        position = position_dodge(width = 0.8)
        ) +
    labs(
        x = NULL,
        y = "Rotation Weight"
    ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_fill_manual(
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
    theme(legend.position = "top")


ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig7_weights.pdf",
    plot,
    units = "in",
    width = 4.5,
    height = 2
    )

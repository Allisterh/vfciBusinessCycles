require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

resid_data_v2232 <- fread("./data/residual-mbc-shock/B.csv")
resid_data_v0632 <- fread("./data/residual-mbc-shock/B_v0632.csv")
resid_data_u2232 <- fread("./data/residual-mbc-shock/B_u2232.csv")
iters <- fread("./data/classical_vfcibc_B.csv")

data <- rbindlist(list(
    iters[target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 32, .(variable, weight, model = "u0632")],
    iters[target == "vfci" & sign == "neg" & period_l == 22 & period_h == 32, .(variable, weight, model = "v2232")],
    iters[target == "vfci" & sign == "neg" & period_l == 6 & period_h == 32, .(variable, weight, model = "v0632")],
    iters[target == "unemployment" & sign == "pos" & period_l == 22 & period_h == 32, .(variable, weight, model = "u2232")],
    resid_data_v0632[, .(variable, weight, model = "u0632 - v0632")],
    resid_data_v2232[, .(variable, weight, model = "u0632 - v2232")],
    resid_data_u2232[, .(variable, weight, model = "u0632 - u2232")]
))

data[, model := factor(model, levels = c("u0632", "u2232", "v0632",  "v2232", "u0632 - v0632", "u0632 - v2232", "u0632 - u2232"), ordered = TRUE)]
data[, variable := factor(variable, levels = arrange(data[model == "u0632"], weight)$variable, ordered = TRUE)]

plot <-
    data[model %in% c("u0632", "v0632", "u0632 - u2232")] |>
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
            u0632 = "gray50",
            v0632 = "steelblue",
            `u0632 - u2232` = "mediumorchid"
        )
    ) +
    theme_pres +
    theme(legend.position = "top") +
    theme(legend.key.size = unit(10, "pt")) +
    theme(legend.text = element_text(size = 6))

ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig11_emp_weights.pdf",
    plot,
    units = "in",
    width = 4.5,
    height = 2
    )

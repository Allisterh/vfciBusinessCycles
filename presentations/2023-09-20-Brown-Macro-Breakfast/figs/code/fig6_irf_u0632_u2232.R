require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

data <- fread("./data/classical_vfcibc_VAR_IRF.csv")
resid_data_v0632 <- fread("./data/residual-mbc-shock/irf_v0632.csv")
resid_data_v2232 <- fread("./data/residual-mbc-shock/irf.csv")
resid_data_u2232 <- fread("./data/residual-mbc-shock/irf_u2232.csv")

data[target == "unemployment" & sign == "neg" & period_l == 6 & period_h == 32, model := "u0632"]
data[target == "vfci" & sign == "pos" & period_l == 6 & period_h == 32, model := "v0632"]

data[target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 22, model := "u0622"]
data[target == "unemployment" & sign == "neg" & period_l == 22 & period_h == 32, model := "u2232"]

data[target == "vfci" & sign == "pos" & period_l == 6 & period_h == 22, model := "v0622"]
data[target == "vfci" & sign == "pos" & period_l == 22 & period_h == 32, model := "v2232"]
data[target == "vfci" & sign == "pos" & period_l == 4 & period_h == 22, model := "v0422"]

data <- rbindlist(list(
    data[,.(h, response, irf, model)],
    resid_data_v0632[, .(h, response, irf, model = "u0632 - v0632")],
    resid_data_v2232[, .(h, response, irf, model = "u0632 - v2232")],
    resid_data_u2232[, .(h, response, irf, model = "u0632 - u2232")]
))

data[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]


plot <-
    data[model %in% c("u0632", "u2232")] |>
    ggplot(aes(
        x = h,
        y = irf,
        color = model
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(
        vars(response),
        nrow = 3,
        scales = "free_y"
    ) +
    labs(
        x = "Horizon",
        y = "IRF",
        color = "Target:"
    ) +
    scale_color_manual(
        values = c(
            u0632 = "gray50",
            `u2232` = "orange"
        ),
        labels = c(
            u0632 = "Anatomy Shock",
            `u2232` = "Unemp. 22 to 32 q"
        )
    ) +
    theme_pres +
    theme(
        legend.position = c(0.875, 0.15),
        legend.title = element_text(size = 8)
        )

ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig6_irf_u0632_u2232.pdf",
    plot,
    units = "in",
    width = 4.5,
    height = 3
    )

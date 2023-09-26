require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

resid_data_v2232 <- fread("./data/residual-mbc-shock/fevd.csv")
resid_data_v0632 <- fread("./data/residual-mbc-shock/fevd_v0632.csv")
resid_data_u2232 <- fread("./data/residual-mbc-shock/fevd_u2232.csv")
iters <- fread("./data/classical_vfcibc_fevd.csv")

data <- rbindlist(list(
    iters[target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 32, .(h, fevd, response, model = "u0632")],
    iters[target == "vfci" & sign == "neg" & period_l == 22 & period_h == 32, .(h, fevd, response, model = "v2232")],
    iters[target == "vfci" & sign == "neg" & period_l == 6 & period_h == 22, .(h, fevd, response, model = "v0622")],
    iters[target == "vfci" & sign == "neg" & period_l == 6 & period_h == 32, .(h, fevd, response, model = "v0632")],
    iters[target == "unemployment" & sign == "pos" & period_l == 22 & period_h == 32, .(h, fevd, response, model = "u2232")],
    resid_data_v0632[, .(h, fevd, response, model = "u0632 - v0632")],
    resid_data_v2232[, .(h, fevd, response, model = "u0632 - v2232")],
    resid_data_u2232[, .(h, fevd, response, model = "u0632 - u2232")]
))

data[, model := factor(model, levels = c("u0632", "u2232", "v0632", "v0622",  "v2232",  "u0632 - v0632", "u0632 - v2232",  "u0632 - u2232"), ordered = TRUE)]
data[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]

plot <-
    data[model %in% c("u0632", "v0632")] |>
    ggplot(aes(
        x = h,
        y = fevd / 100,
        color = model
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(vars(response)) +
    labs(
        x = "Horizon",
        y = "Share of Forecast Error Variance",
        color = "Target:"
    ) +
    scale_color_manual(
        values = c(
            u0632 = "gray50",
            u2232 = "orange",
            v0632 = "steelblue"
        ),
        labels = c(
            u0632 = "Anatomy Shock",
            u2232 = "Unemp. 22 to 32 q",
            v0632 = "VFCI 6 to 32 q"
        )
    ) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_pres +
    theme(
        legend.position = c(0.875, 0.15),
        legend.title = element_text(size = 8)
        )


ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig4_fevd.pdf",
    plot,
    units = "in",
    width = 4.5,
    height = 3
    )

plot <-
    data[model %in% c("u0632", "u2232")] |>
    ggplot(aes(
        x = h,
        y = fevd / 100,
        color = model
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(vars(response)) +
    labs(
        x = "Horizon",
        y = "Share of Forecast Error Variance",
        color = "Target:"
    ) +
    scale_color_manual(
        values = c(
            u0632 = "gray50",
            u2232 = "orange",
            v0632 = "steelblue"
        ),
        labels = c(
            u0632 = "Anatomy Shock",
            u2232 = "Unemp. 22 to 32 q",
            v0632 = "VFCI 6 to 32 q"
        )
    ) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_pres +
    theme(
        legend.position = c(0.875, 0.15),
        legend.title = element_text(size = 8)
        )


ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig4b_fevd.pdf",
    plot,
    units = "in",
    width = 4.5,
    height = 3
    )

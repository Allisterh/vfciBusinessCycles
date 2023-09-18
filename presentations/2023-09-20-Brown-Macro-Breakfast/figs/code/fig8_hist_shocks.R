require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

resid_data_v2232 <- fread("./data/residual-mbc-shock/hist_shocks.csv")
resid_data_v0632 <- fread("./data/residual-mbc-shock/hist_shocks_v0632.csv")
iters <- fread("./data/classical_vfcibc_hist_shocks.csv")

data <- rbindlist(list(
    iters[target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 32, .(date, shock, model = "u0632")],
    iters[target == "vfci" & sign == "neg" & period_l == 22 & period_h == 32, .(date, shock, model = "v2232")],
    iters[target == "vfci" & sign == "neg" & period_l == 6 & period_h == 32, .(date, shock, model = "v0632")],
    iters[target == "unemployment" & sign == "pos" & period_l == 22 & period_h == 32, .(date, shock, model = "u2232")],
    resid_data_v0632[, .(date, shock, model = "u0632 - v0632")],
    resid_data_v2232[, .(date, shock, model = "u0632 - v2232")]
))

data[, model := factor(model, levels = c("u0632", "u2232", "v0632",  "v2232", "u0632 - v0632", "u0632 - v2232"), ordered = TRUE)]

plot <-
    data|>
    ggplot(aes(
        x = date,
        y = shock,
        color = model
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(vars(model), ncol = 3) + 
    labs(
        x = NULL,
        y = "Historical Shock"
    ) +
    scale_color_manual(
        values = c(
            u0632 = "goldenrod",
            u2232 = "lightgoldenrod",
            v0632 = "steelblue",
            v2232 = "lightblue",
            `u0632 - v0632` = "mediumorchid",
            `u0632 - v2232` = "plum"
        )
    ) +
    theme_pres +
    theme(legend.position = "none")

plot
ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig8_hist_shocks.pdf",
    plot,
    units = "in",
    width = 4.5,
    height = 3
    )

#####
data |>
    tidyr::pivot_wider(names_from = "model", values_from = "shock") |>
    ggplot(aes(
        x = u0632,
        y = v2232
    )) +
    geom_vline(xintercept = 0) + 
    geom_hline(yintercept = 0) +
    geom_smooth(se = FALSE, method = "lm") +
    geom_point() +
    coord_fixed()

lm_data <- 
data |>
    tidyr::pivot_wider(names_from = "model", values_from = "shock") 
lm_data |>
    lm(formula = "u0632 ~ v2232") |>
    summary()

stats::cor(lm_data$u0632, lm_data$v2232)

####
plot_data <- copy(data)
plot_data[, shock_ma := frollmean(shock, n = 20, align = "center"), by = .(model)]
plot_data[, shock_ma_norm := scale(shock_ma), by = .(model)]
plot <-
    plot_data[model != "resid"] |>
    ggplot(aes(
        x = date,
        y = shock_ma_norm,
        color = model
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    labs(
        x = NULL,
        y = "Historical Shock"
    ) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
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
plot

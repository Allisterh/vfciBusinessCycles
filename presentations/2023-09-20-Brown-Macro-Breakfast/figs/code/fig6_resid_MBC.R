require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

resid_data_v0632 <- fread("./data/residual-mbc-shock/irf_v0632.csv")
resid_data_v2232 <- fread("./data/residual-mbc-shock/irf.csv")

data <- rbindlist(list(
    resid_data_v0632[, .(h, response, irf, model = "u0632 - v0632")],
    resid_data_v2232[, .(h, response, irf, model = "u0632 - v2232")]
))

data[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]

plot <-
    data |>
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
        x = NULL,
        y = "IRF"
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
    theme(legend.position = c(0.875, 0.15))
plot

ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig6_resid_MBC.pdf",
    plot,
    units = "in",
    width = 4.75,
    height = 3
    )

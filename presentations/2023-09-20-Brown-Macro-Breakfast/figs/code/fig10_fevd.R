require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

resid_data_v2232 <- fread("./data/residual-mbc-shock/fevd.csv")
resid_data_v0632 <- fread("./data/residual-mbc-shock/fevd_v0632.csv")
iters <- fread("./data/classical_vfcibc_fevd.csv")

data <- rbindlist(list(
    iters[target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 32, .(h, fevd, response, model = "u0632")],
    iters[target == "vfci" & sign == "neg" & period_l == 22 & period_h == 32, .(h, fevd, response, model = "v2232")],
    iters[target == "vfci" & sign == "neg" & period_l == 6 & period_h == 32, .(h, fevd, response, model = "v0632")],
    iters[target == "unemployment" & sign == "pos" & period_l == 22 & period_h == 32, .(h, fevd, response, model = "u2232")],
    resid_data_v0632[, .(h, fevd, response, model = "u0632 - v0632")],
    resid_data_v2232[, .(h, fevd, response, model = "u0632 - v2232")]
))

data[, model := factor(model, levels = c("u0632", "u2232", "v0632",  "v2232", "u0632 - v0632", "u0632 - v2232"), ordered = TRUE)]
data[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]

plot <-
    data |>
    ggplot(aes(
        x = h,
        y = fevd/100,
        color = model
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(vars(response)) +
    labs(
        x = NULL,
        y = "FEVDFD"
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
    theme(legend.position = "top")


ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig10_fevd.pdf",
    plot,
    units = "in",
    width = 4.5,
    height = 3
    )

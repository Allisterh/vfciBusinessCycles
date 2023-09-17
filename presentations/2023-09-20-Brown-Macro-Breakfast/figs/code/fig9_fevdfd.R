require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

resid <- fread("./data/residual-mbc-shock/fevdfd.csv")
iters <- fread("./data/classical_vfcibc_fevdfd.csv")

data <- rbindlist(list(
    iters[target == "unemployment" & sign == "pos" & period_l == 6 & period_h == 32, .(f, fevdfd, response, model = "u0632")],
    iters[target == "vfci" & sign == "neg" & period_l == 22 & period_h == 32, .(f, fevdfd, response, model = "v2232")],
    resid[, .(f, fevdfd, response, model = "resid")]
))

data[, model := factor(model, levels = c("u0632", "v2232", "resid"), ordered = TRUE)]
data[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]

plot <-
    data[f <= pi] |>
    ggplot(aes(
        x = f,
        y = fevdfd,
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
    theme(legend.position = c(0.875, 0.15))


ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig9_fevdfd.pdf",
    plot,
    units = "in",
    width = 4.5,
    height = 3
    )

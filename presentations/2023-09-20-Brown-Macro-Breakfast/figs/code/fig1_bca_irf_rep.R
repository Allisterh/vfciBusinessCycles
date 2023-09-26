require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

data <- fread("./data/replicated_bca_classical_VAR_IRF.csv")

data[, variable := factor(variable, levels = var_order, labels = names(var_order), ordered = TRUE)]

plot <-
    data |>
    ggplot(aes(
        x = h,
        y = value,
        color = model,
        linetype = model
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(
        vars(variable),
        nrow = 3,
        scales = "free_y"
        ) +
    labs(
        x = "Horizon",
        y = "IRF"
    ) +
    scale_color_manual(
        values = c(
            classical_fd = "gray50",
            Replication = "firebrick"
            ),
        labels = c(
            classical_fd = "Original BCA Paper",
            Replication = "Replication"
        )
    ) +
    scale_linetype_manual(
        values = c(
            classical_fd = 1,
            Replication = 2
            ),
        labels = c(
            classical_fd = "Original BCA Paper",
            Replication = "Replication"
        )
    ) +
    theme_pres +
    theme(legend.position = c(0.7, 0.15))

ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig1_bca_replication.pdf",
    plot,
    units = "in",
    width = 4.75,
    height = 3
    )

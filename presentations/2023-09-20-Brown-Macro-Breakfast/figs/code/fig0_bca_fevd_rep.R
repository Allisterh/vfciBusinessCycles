require(data.table)
require(ggplot2)
require(dplyr)
source("./presentations/2023-09-20-Brown-Macro-Breakfast/figs/code/theme_pres.R")

## Create FEVD
require(vars)
require(svars)
require(fevdid)

## Pull the correct vintage of the BCA data
BCAdata <- fread("./data/bca_replication_data.csv") |>
    filter(date <= as.Date("2017-01-01"))

## Fit the VAR
v <- VAR(BCAdata[, -"date"], p = 2, type = "const")
mv <- id_fevdfd(v, "unemployment", c(2 * pi / 32, 2 * pi / 6), sign = "pos")

fevd <- fevd(mv, n.ahead = 40)
fevd_df  <- rbindlist(lapply(names(fevd), function(x) {
    fevd_df <- fevd[[x]] |> setDT()
    fevd_df <- fevd_df[, .(h = .I, fevd = Main)]
    fevd_df[, impulse := "Main"]
    fevd_df[, response := x]
}))

fevd_df[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]

plot <-
    fevd_df |>
    ggplot(aes(
        x = h,
        y = fevd / 100
    )) +
    geom_line() +
    facet_wrap(
        vars(response),
        nrow = 3,
        scales = "free_y"
        ) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(
        x = "Horizon",
        y = "Share of Forecast Error Variance",
    ) +
    theme_pres +
    theme(legend.position = c(0.7, 0.15))

ggsave(
    "./presentations/2023-09-20-Brown-Macro-Breakfast/figs/fig0_bca_fevd_rep.pdf",
    plot,
    units = "in",
    width = 4.75,
    height = 3
    )

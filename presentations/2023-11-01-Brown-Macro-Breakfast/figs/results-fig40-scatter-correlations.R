require(ggplot2)
require(data.table)
require(dplyr)
require(vars)
require(svars)
require(bcadata)
require(fevdid)

## Plot settings
f1 <- "results-fig40-sctatter-correlations.pdf"
prespath <- "./presentations/2023-11-01-Brown-Macro-Breakfast/figs/"
plot_height <- 3
plot_width <- 4.7

plot_order <- c("Output", "Investment", "Consumption", "Hours Worked", "Fed Funds", "Labor Prod.", "Inflation", "TFP", "VFCI")

## Load plot themes
source(paste0(prespath, "theme_pres.R"))

## Pull the correct vintage of the BCA data
x <- fread("./data/BCA_replication_data.csv") |>
    filter(date >= as.Date("1962-01-01"))|>
    filter(date <= as.Date("2017-01-01"))

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
targets <- names(x[,-1])

p <- 2
v <- vars::VAR(x[, -"date"], p = p, type = "const")


## ID shock
mv_l <- lapply(seq_along(targets), function(i) {
  id_fevdfd(v, targets[[i]], bc_freqs, sign = ifelse(targets[[i]] %in% c("unemployment", "labor_share"), "pos", "neg"))
}) 

hs_df <- rbindlist(lapply(seq_along(targets), function(i) {
  hs <- hs(mv_l[[i]])$hs |> setDT()
  hs <- hs[impulse == "Main"]
  hs$target <- targets[[i]]
  hs$date <- x[-c(1:p), "date"][[1]]
  hs
}))

hs_cross <- merge(
  hs_df[, .(date, target.x = target, hs.x = hs)],
  hs_df[, .(date, target.y = target, hs.y = hs)],
  by = "date",
  all = TRUE,
  allow.cartesian = TRUE
)

hs_cross[, target.y := factor(target.y, levels = var_order, labels = names(var_order), ordered = TRUE)]
hs_cross <- hs_cross |>
  filter(target.x == "unemployment") |>
  filter(!(target.y %in% c("Unemployment", "Labor Share"))) |>
  mutate(target.y = factor(target.y, levels = plot_order, ordered = T))


corrs <- hs_cross[, .(
  corr = cor(hs.x, hs.y),
  min_x = min(hs.x) * 0.65,
  max_y = max(hs.y) * 0.9
  ), by = .(target.x, target.y)] |>
  filter(target.x == "unemployment") |>
  filter(!(target.y %in% c("Unemployment", "Labor Share"))) |>
  mutate(target.y = factor(target.y, levels = plot_order, ordered = T))

plot_cross_bca <- ggplot() +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_point(
    data = hs_cross,
    size = 0.75,
    alpha = 0.5,
    aes(
      x = hs.x,
      y = hs.y
    )) +
  geom_text(
    data = corrs,
    color = "firebrick",
    size = 2,
    aes(
      x = min_x,
      y = max_y,
      label = round(corr, 3)
    )
  ) +
  facet_wrap(
    vars(target.y),
    scales = "free"
  ) +
  theme_pres +
  theme(
    strip.text = element_text(size = 6),
    axis.text = element_text(size = 5),
    plot.title = element_text(size = 8),
    panel.spacing = unit(1, "pt"),
    plot.margin = margin(2, 4, 2, 2, "pt"),
    axis.title.x = element_text(size = 6)
  ) +
  labs(
    x = "Unemployment Targeted Shock",
    y = NULL,
    title = "Orig. 10 BCA Variables"
  ) +
  theme(plot.margin = margin(2, 8, 2, 2, "pt"))

##
require(GGally)
bc_correlations <- 
  hs_df[, .(date, target, hs)] |>
  tidyr::pivot_wider(names_from = "target", values_from = "hs") |>
  dplyr::select(-"date") |>
  mutate(vfci = NA) |>
  ggpairs(
    columns = c("unemployment", "inflation", "TFP", "vfci"),
    diag = NULL,
    title = "Orig. 10 BCA Variables") +
  theme_pres +
  theme(
    panel.spacing = unit(1, "pt"),
    axis.text = element_blank()
    )

####################################################
##
##
##
####################################################

x <- fread("./data/vfciBC_data.csv") |>
    filter(date <= as.Date("2017-01-01"))

x <- x[, .(date, output, investment, consumption, hours_worked,
    unemployment, labor_share, interest, inflation, productivity, TFP, vfci = vfci_fgr10gdpc1)]

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
targets <- names(x[, -1])

p <- 2
v <- vars::VAR(x[, -"date"], p = p, type = "const")


## ID shock
mv_l <- lapply(seq_along(targets), function(i) {
  id_fevdfd(v, targets[[i]], bc_freqs, sign = ifelse(targets[[i]] %in% c("unemployment", "labor_share"), "pos", "neg"))
}) 

hs_df <- rbindlist(lapply(seq_along(targets), function(i) {
  hs <- hs(mv_l[[i]])$hs |> setDT()
  hs <- hs[impulse == "Main"]
  hs$target <- targets[[i]]
  hs$date <- x[-c(1:p), "date"][[1]]
  hs
}))

hs_cross <- merge(
  hs_df[, .(date, target.x = target, hs.x = hs)],
  hs_df[, .(date, target.y = target, hs.y = hs)],
  by = "date",
  all = TRUE,
  allow.cartesian = TRUE
)


hs_cross[, target.y := factor(target.y, levels = var_order, labels = names(var_order), ordered = TRUE)]
hs_cross <- hs_cross |>
  filter(target.x == "unemployment") |>
  filter(!(target.y %in% c("Unemployment", "Labor Share"))) |>
  mutate(target.y = factor(target.y, levels = plot_order, ordered = T))


corrs <- hs_cross[, .(
  corr = cor(hs.x, hs.y),
  min_x = min(hs.x) * 0.65,
  max_y = max(hs.y) * 0.9
  ), by = .(target.x, target.y)] |>
  filter(target.x == "unemployment") |>
  filter(!(target.y %in% c("Unemployment", "Labor Share"))) |>
  mutate(target.y = factor(target.y, levels = plot_order, ordered = T))

plot_cross_vfci <- ggplot() +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_point(
    data = hs_cross,
    size = 0.75,
    alpha = 0.5,
    aes(
      x = hs.x,
      y = hs.y
    )) +
  geom_text(
    data = corrs,
    color = "firebrick",
    size = 2,
    aes(
      x = min_x,
      y = max_y,
      label = round(corr, 3)
    )
  ) +
  facet_wrap(
    vars(target.y),
    scales = "free"
  ) +
  theme_pres +
  theme(
    strip.text = element_text(size = 6),
    axis.text = element_text(size = 5),
    plot.title = element_text(size = 8),
    panel.spacing = unit(1, "pt"),
    plot.margin = margin(2, 4, 2, 2, "pt"),
    axis.title.x = element_text(size = 6)
  ) +
  labs(
    x = "Unemployment Targeted Shock",
    y = NULL,
    title = "... + VFCI"
  ) +
  theme(plot.margin = margin(2, 4, 2, 6, "pt"))


##
require(GGally)
vfciBC_correlations <- 
  hs_df[, .(date, target, hs)] |>
  tidyr::pivot_wider(names_from = "target", values_from = "hs") |>
  dplyr::select(-"date") |>
  ggpairs(
    columns = c("unemployment", "inflation", "TFP", "vfci"),
    diag = NULL,
    title = "... + VFCI"
  ) +
  scale_size_manual(values = c(0.4)) +
  theme_pres +
  theme(
    panel.spacing = unit(1, "pt"),
    axis.text = element_blank(),
    title = element_text(size = 7),
    strip.text = element_text(size = 6)
  )

####################################################
##
## Combine
##
####################################################

#plot_ggally <- cowplot::plot_grid(
#  ggmatrix_gtable(bc_correlations),
#  ggmatrix_gtable(vfciBC_correlations),
#  nrow = 1
#  )

plot <- cowplot::plot_grid(
  plot_cross_bca,
  plot_cross_vfci,
  nrow = 1
  )

ggsave(
  paste0(prespath, f1),
  plot,
  width = plot_width,
  height = plot_height,
  units = "in"
)

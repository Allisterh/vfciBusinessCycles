## Packages
require(data.table)
require(dplyr)
require(vars)
require(svars)
require(fevdid)
require(ggplot2)

## Plot settings
f1 <- "mp-fig30-scatter_u_vfci.pdf"
f2 <- "mp-fig31-mp_scatter_corr.pdf"
prespath <- "./presentations/2023-11-01-Brown-Macro-Breakfast/figs/"
plot_height <- 3
plot_width <- 4.5

## Load plot themes
source(paste0(prespath, "theme_pres.R"))

## Construct shocks
mp <- fread("./data-raw/mp_shocks.csv")

vfciBCdata <- fread(here::here("./data/vfciBC_data.csv")) |>
    filter(date <= as.Date("2017-01-01"))

vfciBCdata <- vfciBCdata[, .(date, output, investment, consumption, hours_worked,
    unemployment, labor_share, interest, inflation, productivity, TFP, vfci = vfci_fgr10gdpc1)]

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
p <- 2

## Fit the VAR
v <- VAR(vfciBCdata[, -"date"], p = p, type = "const")

mv_vfci <- id_fevdfd(v, "vfci", bc_freqs, sign = "neg")
mv_u <- id_fevdfd(v, "unemployment", bc_freqs, sign = "pos")
mv_int <- id_fevdfd(v, "interest", bc_freqs, sign = "neg")
mv_inf <- id_fevdfd(v, "inflation", bc_freqs, sign = "neg")

## Shocks
hs_vfci <- hs(mv_vfci)$hs |> setDT() |> filter(impulse == "Main")
hs_u <- hs(mv_u)$hs |> setDT() |> filter(impulse == "Main")
hs_int <- hs(mv_int)$hs |> setDT() |> filter(impulse == "Main")
hs_inf <- hs(mv_inf)$hs |> setDT() |> filter(impulse == "Main")

hs_vfci$date <- vfciBCdata[-(1:p), "date"][[1]]
hs_u$date <- vfciBCdata[-(1:p), "date"][[1]]
hs_int$date <- vfciBCdata[-(1:p), "date"][[1]]
hs_inf$date <- vfciBCdata[-(1:p), "date"][[1]]

## Merge
df <- mp |>
  merge(hs_vfci[, .(date, vfci = hs)], by = "date", all = T) |>
  merge(hs_u[, .(date, u = hs)], by = "date", all = T) |>
  merge(hs_int[, .(date, interest = hs)], by = "date", all = T) |>
  merge(hs_inf[, .(date, inflation = hs)], by = "date", all = T)


## Plot!
## Start with Scatter of VFCI and Unemployment
p_hs <-
  df |>
  tidyr::pivot_longer(-"date") |>
  mutate(date = as.Date(date)) |>
  filter(name %in% c("u", "vfci")) |>
  filter(date <= as.Date("2017-07-01")) |>
  ggplot(aes(
    x = date,
    y = value,
    color = name
  )) +
  geom_hline(yintercept = 0) +
  geom_line() +
  scale_x_date(expand = c(0,0)) +
  scale_color_manual(
    name = "Targeted Variable",
  values = c(
    vfci = vfci_color,
    u = u_color,
    inflation = inf_color,
    interest = int_color
  ),
  labels = c(
    vfci = "VFCI",
    u = "Unemployment",
    inflation = "Inflation",
    interest  = "Interest"
  )
  ) +
  theme_pres +
  labs(
    x = "",
    y = "Historical Shock"
  ) +
  theme(
    legend.position = c(0.65, 0.15),
    legend.title = element_text(size = 7)
    ) +
  theme(plot.margin = margin(2, 8, 2, 2, "pt"))

## Scatter plot
p_scatter <- 
  df |>
  ggplot(aes(
    x = u,
    y = vfci
  )) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(alpha = 0.5) +
  annotate(
    "text",
    x = 0.9 * min(df$u, na.rm = T),
    y = 0.9 * max(df$vfci, na.rm = T),
    label = paste0("Corr:\n", round(cor(df$vfci, df$u, use = "pairwise.complete.obs"), digits = 3)),
    color = "firebrick",
    size = 2.5
    ) +
  theme_pres +
  theme(plot.margin = margin(2, 2, 2, 8, "pt")) +
  theme(plot.title = element_text(hjust = 0.5, size = 7)) +
  labs(
    x = "Unemployment Targeted Shock",
    y = "",
    title = "VFCI"
  )

plot <- 
  cowplot::plot_grid(
  p_hs,
  p_scatter,
  align = "h"
  )

## Save to disk
ggsave(
  paste0(prespath, f1),
  plot,
  width = 4,
  height = 2,
  units = "in"
)

## Plot the scatter of everyone
## Pull the correct vintage of the BCA data
x <- fread("./data/vfciBC_data.csv") |>
    filter(date <= as.Date("2017-01-01"))

x <- x[, .(date, output, investment, consumption, hours_worked,
    unemployment, labor_share, interest, inflation, productivity, TFP, vfci = vfci_fgr10gdpc1)]

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
hs_df[, date := as.Date(date)]

## Add in Monetary Policy shocks
mp2 <- tidyr::pivot_longer(mp, -"date", names_to = "target", values_to = "hs") |>
  mutate(date = as.Date(date)) |>
  setDT() |>
  mutate(hs = -1 * hs)

hs_df2 <- rbindlist(list(hs_df[, .(date, target, hs)], mp2[, .(date, target, hs)]))

hs_cross <- merge(
  hs_df2[, .(date, target.x = target, hs.x = hs)],
  hs_df2[, .(date, target.y = target, hs.y = hs)],
  by = "date",
  all = TRUE,
  allow.cartesian = TRUE
)
hs_cross <- hs_cross[target.x == "mp_shock_int_rr_ns"] |>
  filter(!(target.y %in% c("mp_shock_int_rr_mar_ns", "mp_shock_int_rr_ns", "mp_shock_mr", "mp_shock_ns", "mp_shock_rr")))

corrs <- hs_cross[, .(
  corr = cor(hs.x, hs.y),
  min_x = min(hs.x) * 0.65,
  max_y = max(hs.y) * 0.9
  ), by = .(target.x, target.y)] |>
  filter(target.x == "mp_shock_int_rr_ns") |>
  filter(!(target.y %in% c("mp_shock_int_rr_mar_ns", "mp_shock_int_rr_ns", "mp_shock_mr", "mp_shock_ns", "mp_shock_rr")))

hs_cross[, target.y := factor(target.y, levels = var_order, labels = names(var_order), ordered = TRUE)]
corrs[, target.y := factor(target.y, levels = var_order, labels = names(var_order), ordered = TRUE)]

mp_plot <- 
  ggplot() +
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
  labs(
    x = "Monetary Policy Surprise Shock (NS and RR)",
    y = NULL
  )


ggsave(
  paste0(prespath, f2),
  mp_plot,
  width = 4.25,
  height = plot_height,
  units = "in"
)

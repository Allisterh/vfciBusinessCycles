## Packages
require(data.table)
require(dplyr)
require(vars)
require(svars)
require(fevdid)
require(ggplot2)

## Plot settings
filename <- "results-fig1-vfci_u_0632.pdf"
prespath <- "./presentations/2023-11-01-Brown-Macro-Breakfast/figs/"
plot_height <- 3
plot_width <- 4.5

## Load plot themes
source(paste0(prespath, "theme_pres.R"))

## Analysis
vfciBCdata <- fread(here::here("./data/vfciBC_data.csv")) |>
    filter(date <= as.Date("2017-01-01"))

vfciBCdata <- vfciBCdata[, .(date, output, investment, consumption, hours_worked,
    unemployment, labor_share, interest, inflation, productivity, TFP, vfci)]

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
p <- 2

## Fit the VAR
v <- VAR(vfciBCdata[, -"date"], p = p, type = "const")

mv_vfci <- id_fevdfd(v, "vfci", bc_freqs, sign = "neg")
mv_u <- id_fevdfd(v, "unemployment", bc_freqs)

irf_vfci <- irf(mv_vfci, n.ahead = 40, impulse = "Main")$irf
irf_u <- irf(mv_u, n.ahead = 40, impulse = "Main")$irf

irf_vfci$model <- "vfci"
irf_u$model <- "u"

irf_df <- rbind(irf_vfci, irf_u)

setDT(irf_df)

## Plotting
irf_df[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]

plot <-
  irf_df |>
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
    y = "Impulse Response Function",
    x = "Horizon"
  ) +
  scale_color_manual(
    values = c(
      vfci = vfci_color,
      u = u_color
    ),
    labels = c(
      vfci = "VFCI",
      u = "Unemployment"
    )
  ) +
  theme_pres +
  theme(legend.position = c(0.875, 0.15))

## Save to disk
ggsave(
  paste0(prespath, filename),
  plot,
  width = plot_width,
  height = plot_height,
  units = "in"
  )

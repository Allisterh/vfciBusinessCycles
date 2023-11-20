## Packages
require(data.table)
require(dplyr)
require(vars)
require(svars)
require(fevdid)
require(ggplot2)

## Plot settings
irf_filename <- "results-fig1-vfci_u_0632_irf.pdf"
hd_filename <- "results-fig2-vfci_u_0632_hd.pdf"
fevdfd_filename <- "results-fig3-vfci_u_0632_fevdfd.pdf"
qb_filename <- "results-fig4-vfci_u_0632_qb.pdf"
prespath <- "./presentations/2023-11-01-Brown-Macro-Breakfast/figs/"
plot_height <- 3
plot_width <- 4.5

## Load plot themes
source(paste0(prespath, "theme_pres.R"))

## Analysis
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
mv_u <- id_fevdfd(v, "unemployment", bc_freqs)

##### IRF

irf_vfci <- irf(mv_vfci, n.ahead = 40, impulse = "Main")$irf
irf_u <- irf(mv_u, n.ahead = 40, impulse = "Main")$irf

irf_vfci$model <- "vfci"
irf_u$model <- "u"

irf_df <- rbind(irf_vfci, irf_u)

setDT(irf_df)

## Get Recession data
require(quantmod)
getSymbols("USREC", src = "FRED")

start <- zoo::index(USREC[which(diff(USREC$USREC)==1)])
end   <- zoo::index(USREC[which(diff(USREC$USREC)==-1)-1])

rec <- data.frame(start = start, end = end[-1])
rec <- subset(rec, start >= as.Date("1962-01-01"))
rec <- subset(rec, end <= as.Date("2018-01-01"))


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
    name = "Targeted Variable",
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
  theme(
    legend.position = c(0.875, 0.15),
    legend.title = element_text(size = 7)
    )

## Save to disk
ggsave(
  paste0(prespath, irf_filename),
  plot,
  width = plot_width,
  height = plot_height,
  units = "in"
  )


##### HD

hd_vfci <- hd(mv_vfci)$hd
hd_u <- hd(mv_u)$hd

hd_vfci$model <- "vfci"
hd_u$model <- "u"

hd_vfci$date <- rep(vfciBCdata[-(1:p), "date"][[1]], times = v$K * v$K)
hd_u$date <- rep(vfciBCdata[-(1:p), "date"][[1]], times = v$K * v$K)

hd_df <- rbind(hd_vfci, hd_u)

setDT(hd_df)

## Plotting
hd_df[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]
plot_order <- c("VFCI", "Unemployment", "Inflation")
hd_df <- hd_df[response %in% plot_order]
hd_df[, response := factor(response, levels = plot_order, ordered = TRUE)]

plot <- ggplot() +
  geom_rect(
    data = rec,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = +Inf
    ),
    fill = "gray80",
    color = NA,
    alpha = 0.5
    ) +
  geom_hline(yintercept = 0) +
  geom_line(
    data = hd_df[impulse == "Main"],
    aes(
      x = date,
      y = hd,
      color = model
    )
  ) +
  facet_wrap(
    vars(response),
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    y = "Historical Contribution of Shock",
    x = NULL
  ) +
  scale_color_manual(
    name = "Targeted Variable",
    values = c(
      vfci = vfci_color,
      u = u_color
    ),
    labels = c(
      vfci = "VFCI",
      u = "Unemployment"
    )
  ) +
  scale_x_date(
    breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "5 years"),
    labels = seq(1960, 2020, 5),
    expand = c(0, 0)
  ) +
  theme_pres +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7)
    )

## Save to disk
ggsave(
  paste0(prespath, hd_filename),
  plot,
  width = plot_width,
  height = plot_height,
  units = "in"
)

##### FEVDFD

fevdfd_vfci <- fevdfd(mv_vfci)$fevdfd
fevdfd_u <- fevdfd(mv_u)$fevdfd

fevdfd_vfci$model <- "vfci"
fevdfd_u$model <- "u"

fevdfd_df <- rbind(fevdfd_vfci, fevdfd_u)

setDT(fevdfd_df)

## Plotting
fevdfd_df[, response := factor(response, levels = var_order, labels = names(var_order), ordered = TRUE)]

# plot_order <- c("VFCI", "Unemployment",  "Inflation", "Output")
# fevdfd_df <- fevdfd_df[response %in% plot_order]
# fevdfd_df[, response := factor(response, levels = plot_order, ordered = TRUE)]

fevdfd_df <- fevdfd_df[impulse == "Main"]
fevdfd_df <- fevdfd_df[ f <= pi]

plot <-
  fevdfd_df |>
  ggplot(aes(
    x = f,
    y = fevdfd,
    color = model
  )) +
  geom_rect(
    xmin = bc_freqs[[1]],
    xmax = bc_freqs[[2]],
    ymin = -Inf,
    ymax = +Inf,
    fill = "gray90",
    color = NA
    ) +
  geom_line() +
  facet_wrap(
    vars(response),
    nrow = 3,
    scales = "free_y"
  ) +
  labs(
    y = "Forecast Error Variance Decomposition",
    x = "Frequency Domain"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    name = "Targeted Variable",
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
  theme(
    legend.position = c(0.875, 0.15),
    legend.title = element_text(size = 7)
    )

## Save to disk
ggsave(
  paste0(prespath, fevdfd_filename),
  plot,
  width = plot_width,
  height = plot_height,
  units = "in"
)

## Q weights
q_df <- data.table(
  impulse = rownames(mv_u$B),
  u = mv_u$Q[, 1],
  vfci = mv_vfci$Q[, 1]
) |> tidyr::pivot_longer(-impulse, names_to = "model") |>
setDT()

q_df[, impulse := factor(impulse, levels = var_order, labels = names(var_order), ordered = TRUE)]

q_plot <- q_df |>
  ggplot(aes(
  x = impulse,
  y = value,
  fill = model
 )) +
 geom_hline(yintercept = 0) + 
 geom_col(position = position_dodge()) +
 labs(
  x = "Emprirical Shock",
  y = "Standardized Variance Weights"
 ) +
 scale_fill_manual(
  name = "Targeted Variable",
    values = c(
      vfci = vfci_color,
      u = u_color
    ),
    labels = c(
      vfci = "VFCI",
      u = "Unemployment"
    )
  ) +
  scale_x_discrete(
    guide = guide_axis(n.dodge = 2)
  ) +
 theme_pres +
 theme(
  legend.position = c(0.85, 0.25),
    legend.title = element_text(size = 7)
    )


## B Weights
b_df <- data.table(
  impulse = rownames(mv_u$B),
  u = solve(mv_u$B)[1, ],
  vfci = solve(mv_vfci$B)[1, ]
  ) |> tidyr::pivot_longer(-impulse, names_to = "model") |>
  setDT()

b_df[, impulse := factor(impulse, levels = var_order, labels = names(var_order), ordered = TRUE)]


b_plot <- b_df |>
  ggplot(aes(
  x = impulse,
  y = value,
  fill = model
 )) +
 geom_hline(yintercept = 0) + 
 geom_col(position = position_dodge()) +
 labs(
  x = "Emprirical Shock",
  y = "Weights"
 ) +
 scale_x_discrete(
    guide = guide_axis(n.dodge = 2)
  ) +
 scale_fill_manual(
  name = "Targeted Variable",
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
 theme(legend.position = c(0.85, 0.25),
    legend.title = element_text(size = 7))

plot <- cowplot::plot_grid(q_plot, b_plot, ncol = 1)
ggsave(
  paste0(prespath, qb_filename),
  plot,
  width = plot_width,
  height = plot_height,
  units = "in"
)

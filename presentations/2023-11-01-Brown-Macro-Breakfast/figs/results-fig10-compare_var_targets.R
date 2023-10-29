require(ggplot2)
require(data.table)
require(dplyr)
require(vars)
require(svars)
require(fevdid)
require(stringr)

##
orth_filename <- "results-fig10-compare_var_targets_orth.pdf"
fevdfd_filename <- "results-fig11-compare_var_target_fevdfd.pdf"
prespath <- "./presentations/2023-11-01-Brown-Macro-Breakfast/figs/"
plot_height <- 3
plot_width <- 4.5

## Pull the correct vintage of the BCA data
vfciBCdata <- fread("./data/vfciBC_data.csv") |>
    filter(date <= as.Date("2017-01-01"))
vfciBCdata <- vfciBCdata[, .(date, output, investment, consumption, hours_worked, unemployment, labor_share, interest, inflation, productivity, TFP, vfci = vfci_fgr10gdpc1)]

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
target_vars <- c("unemployment", "output", "consumption", "investment", "hours_worked", "interest", "inflation", "vfci", "productivity", "TFP", "labor_share")

## Fit the VAR
p <- 2
v <- VAR(vfciBCdata[, -"date"], p = p, type = "const")

## ID shock
mv_l <- lapply(target_vars, function(i) {
  id_fevdfd(v, i, bc_freqs, sign = ifelse(i %in% c("unemployment", "TFP", "productivity"), "pos", "neg"))
}) 

irf_df <-
  lapply(seq_along(target_vars), function(i){
    irf <- irf(mv_l[[i]], n.ahead = 40, impulse = "Main")$irf
    irf$model <- target_vars[[i]]
    irf
  }) |>
  rbindlist()


fevdfd_df <-
  lapply(seq_along(target_vars), function(i){
    fevdfd <- fevdfd(mv_l[[i]])$fevdfd
    fevdfd$model <- target_vars[[i]]
    fevdfd <- fevdfd[fevdfd$impulse == "Main", ]
    fevdfd
  }) |>
  rbindlist()

hd_df <-
  lapply(seq_along(target_vars), function(i){
    hd <- hd(mv_l[[i]])$hd
    hd$model <- target_vars[[i]]
    hd <- hd[hd$impulse == "Main", ]
    hd$date <- rep(vfciBCdata[-c(1:p), "date"][[1]], times = v$K)
    hd
  }) |>
  rbindlist()

hs_df <-
  lapply(seq_along(target_vars), function(i){
    hs <- hs(mv_l[[i]])$hs
    hs$model <- target_vars[[i]]
    hs <- hs[hs$impulse == "Main", ]
    hs$date <- vfciBCdata[-c(1:p), "date"][[1]]
    hs
  }) |>
  rbindlist()


orth_df <- rbindlist(lapply(seq_along(mv_l), function(i) {
    rbindlist(lapply(seq_along(mv_l), function(j) {
        orth <- mv_l[[i]]$Q[, 1] %*% mv_l[[j]]$Q[, 1]
        data.table(
            i = i,
            j = j,
            target_i = target_vars[[i]],
            target_j  = target_vars[[j]],
            orth = orth[[1]]
        )
    }))
}))

orth_m <- matrix(orth_df$orth, length(target_vars), length(target_vars), dimnames =  list(target_vars, target_vars))

## Plot orth
var_order <- c("unemployment", "output", "investment", "hours_worked", "consumption", "vfci", "interest", "inflation", "labor_share",  "productivity", "TFP")
var_order <- names(sort(orth_m["unemployment", ], TRUE))

plot <-
orth_df |>
  ggplot(aes(
    x = target_i,
    y = target_j,
    fill = round(orth, digits = 4),
    label = round(orth, digits = 2)
  )) +
  geom_tile() +
  geom_text(size = 1.5) + 
  scale_x_discrete(
    expand = c(0,0),
    limits = var_order,
    guide = guide_axis(n.dodge = 2)
  ) +
  scale_y_discrete(
    expand = c(0,0),
    limits = var_order
  ) +
  scale_fill_distiller(
    limits = c(-1, 1),
    type = "div",
    palette = "RdBu",
    direction = 1
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Corr"
  ) +
  coord_fixed() +
  theme_pres +
  theme(
    legend.position = "right",
    axis.text.x = element_text(color = ifelse(var_order == "vfci", "darkorange", "black")),
    axis.text.y = element_text(color = ifelse(var_order == "vfci", "darkorange", "black")),
    legend.title = element_text(size = 7)
    )


## Save to disk
ggsave(
  paste0(prespath, orth_filename),
  plot,
  width = plot_width,
  height = plot_height,
  units = "in"
)


##### Plot FEVDFD contributions to each variable, varying VAR target
fevdfd_summ <- fevdfd_df[f %between% bc_freqs, .(fevdfd = mean(fevdfd)), by = .(impulse, model, response)]
setorder(fevdfd_summ, response, -fevdfd)
plot_order <- fevdfd_summ[response == "unemployment", model]

plot <- 
  fevdfd_summ |>
  ggplot(aes(
    x = model,
    y = response,
    fill = fevdfd,
    label = round(fevdfd, 2)
  )) +
  geom_tile() +
  geom_text(size = 1.5) +
  coord_fixed() +
  labs(
    x = "Target Variable for VAR",
    y = "Response Variable for FEVDFD",
    fill = "FEVDFD"
  ) +
  scale_fill_distiller(
    limits = c(0, 1),
    type = "seq",
    palette = "Blues",
    direction = 1
  ) +
  scale_x_discrete(
    expand = c(0,0),
    guide = guide_axis(n.dodge = 2),
    limits = plot_order
  ) +
  scale_y_discrete(
    expand = c(0,0),
    limits = plot_order
  ) + 
  theme_pres +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 7)
  )

ggsave(
  paste0(prespath, fevdfd_filename),
  plot,
  width = plot_width,
  height = plot_height,
  units = "in"
)

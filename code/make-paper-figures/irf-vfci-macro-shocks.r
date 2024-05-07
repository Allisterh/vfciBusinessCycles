## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")

data <- get_var_data()

all_variables <- names(data[, -"date"])

bc_targets <- c("vfci", "unemployment", "output", "investment", "consumption", "hours_worked")

all_svars <-
  fit_fevdfd_svar(
    data,
    lags = 2,
    target_variables = bc_targets,
    target_freqs = bc_freqs,
    model_names = bc_targets,
    sign = ifelse(bc_targets == "unemployment", "pos", "neg")
  )

p <-
  all_svars$irf_dt |>
  _[impulse == "Main"] |>
  ggplot(aes(
     x = h,
    y = irf,
    color = model_name
  )) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(
    vars(response),
    scales = "free_y",
    nrow = 4
  ) +
  theme_classic() +
  theme(legend.position = c(0.875, 0.125))

library(patchwork)
p1 | p

ggsave(
  "./paper-Overleaf/figs/irf-vfci-macro-shocks.pdf",
  p, width = 5, height = 5, units = "in"
)

## Plot for IRFs of VFCI-targeted and 5 Macro Targets
library(ggplot2)

source("./code/vfci-bc/target-all-var-bc-freqs.R")
source("./code/vfci-bc/vfci-cholesky.R")

#####

p <-
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(
    data = irf_dt[impulse == "Main" & target_variable == "vfci"],
    aes(
      x = h,
      y = irf,
      color = "max_share"
    )
  ) +
  geom_line(
    data = cv_irf_dt[impulse == "Main" & target_variable == "vfci"],
    aes(
      x = h,
      y = irf,
      color = "chol"
    )
  ) +
  facet_wrap(
    vars(response),
    nrow = 4,
    scales = "free"
  ) +
  theme_classic() +
  theme(legend.position = c(0.875, 0.125))

p

ggsave(
  "./paper-Overleaf/figs/irf-vfci-targeted-vfci-chol.pdf",
  p, width = 5, height = 5, units = "in"
)

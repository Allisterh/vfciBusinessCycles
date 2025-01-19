library(ggplot2)
library(data.table)
library(vfciBCHelpers)

data <-
  list(
    fread("./data/paper-figures/charts/hs-fevdfd.csv"),
    fread("./data/paper-figures/charts/hs-chol.csv"),
    fread("./data/paper-figures/charts/hs-hetreg.csv")
  ) |>
  purrr::list_rbind()



## General Function to make HS plots

plot_hs <- function(data, identifications, moving_average_length = 8) {
  date <- hs <- identification <- NULL

  data |>
    _[identification %in% identifications] |>
    _[, hs := frollmean(hs, moving_average_length, align = "right"), by = .(identification)] |>
    ggplot(aes(
      x = date,
      y = hs,
      color = identification
    )) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_line() +
    labs(
      x = NULL,
      y = "Historical Shocks (5yr mva)"
    ) +
    theme_paper
}

#####
## Make plots and save them

## FEVDFD

p <- plot_hs(data, c("fevdfd_vfci_f1", "fevdfd_vfci_f12", "fevdfd_unem"))

p

ggsave(
  "./paper-figures/charts/hs-fevdfd-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 2, units = "in"
)

## Chol

p <- plot_hs(data, c("chol_vfci_f1", "chol_vfci_f12", "fevdfd_unem"))

p

ggsave(
  "./paper-figures/charts/hs-chol-ext-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 2, units = "in"
)

## Chol - other measures

p <- plot_hs(data, c("fevdfd_unem", "chol_fcig", "chol_epu"))

p

ggsave(
  "./paper-figures/charts/hs-chol-epu-fcig-fevdfd-unem.pdf",
  p, width = 5.5, height = 2, units = "in"
)

## Hetreg Macro

p <- plot_hs(data, c("fevdfd_unem", "hr_macro"))

p

ggsave(
  "./paper-figures/charts/hs-hr-macro-fevdfd-unem.pdf",
  p, width = 5.5, height = 2, units = "in"
)

## Hetreg Fin

p <- plot_hs(data, c("fevdfd_unem", "hr_fin"))

p

ggsave(
  "./paper-figures/charts/hs-hr-fin-fevdfd-unem.pdf",
  p, width = 5.5, height = 2, units = "in"
)

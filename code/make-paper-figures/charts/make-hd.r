library(ggplot2)
library(data.table)
library(vfciBCHelpers)

data <-
  list(
    fread("./data/paper-figures/charts/hd-fevdfd.csv"),
    fread("./data/paper-figures/charts/hd-chol.csv"),
    fread("./data/paper-figures/charts/hd-hetreg.csv")
  ) |>
  purrr::list_rbind()



## General Function to make HS plots

plot_hd <- function(data, identifications, responses, ncol = 1) {
  date <- hd <- identification <- response <- NULL

  data |>
    _[identification %in% identifications] |>
    _[response %in% responses] |>
    ggplot(aes(
      x = date,
      y = hd,
      color = identification
    )) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_line() +
    facet_wrap(vars(response), scales = "free_y", ncol = ncol) +
    labs(
      x = NULL,
      y = "Historical Contribution"
    ) +
    theme_paper
}

#####
## Make plots and save them

## FEVDFD

p <- plot_hd(data, c("fevdfd_vfci_f1", "fevdfd_vfci_f12", "fevdfd_unem"), c("unemployment", "output"))

p

ggsave(
  "./paper-figures/charts/hd-fevdfd-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 3, units = "in"
)

## Chol

p <- plot_hd(data, c("chol_vfci_f1", "chol_vfci_f12", "fevdfd_unem"), c("unemployment", "output"))

p

ggsave(
  "./paper-figures/charts/hd-chol-ext-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 3, units = "in"
)

## Chol - other measures

p <- plot_hd(data, c("fevdfd_unem", "chol_fcig", "chol_epu"), c("unemployment", "output"))

p

ggsave(
  "./paper-figures/charts/hd-chol-epu-fcig-fevdfd-unem.pdf",
  p, width = 5.5, height = 3, units = "in"
)

## Hetreg Macro

p <- plot_hd(data, c("fevdfd_unem", "hr_macro"), c("unemployment", "output"))

p

ggsave(
  "./paper-figures/charts/hd-hr-macro-fevdfd-unem.pdf",
  p, width = 5.5, height = 3, units = "in"
)

## Hetreg Fin

p <- plot_hd(data, c("fevdfd_unem", "hr_fin"), c("unemployment", "output"))

p

ggsave(
  "./paper-figures/charts/hd-hr-fin-fevdfd-unem.pdf",
  p, width = 5.5, height = 3, units = "in"
)

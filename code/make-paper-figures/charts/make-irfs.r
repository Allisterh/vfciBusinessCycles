library(ggplot2)
library(data.table)
library(vfciBCHelpers)

data <-
  list(
    fread("./data/paper-figures/charts/irf-fevdfd.csv"),
    fread("./data/paper-figures/charts/irf-chol.csv"),
    fread("./data/paper-figures/charts/irf-hetreg.csv")
  ) |>
  purrr::list_rbind()

## Label and order variable responses
data[, response :=
  factor(
    response,
    levels = c(
      `FCI Growth` = "fci_g",
      `EPU` = "epu",
      variable_labels
    ),
    labels = c(
      "FCI Growth",
      "EPU",
      labels(variable_labels)
    ),
    ordered = TRUE
  )]


## General Function to make IRF plots

plot_irf <- function(data, identifications) {
  h <- irf <- response <- identification <- NULL

  data |>
    _[identification %in% identifications] |>
    ggplot(aes(
      x = h,
      y = irf,
      color = identification
    )) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_line() +
    facet_wrap(
      vars(response),
      scales = "free_y",
      nrow = 3
    ) +
    labs(
      x = "Horizon (quarters)",
      y = "Impulse Response Function"
    ) +
    theme_paper +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.88, 0.125)
    )
}

#####
## Make plots and save them

## FEVDFD

p <- plot_irf(data, c("fevdfd_vfci_f1", "fevdfd_vfci_f12", "fevdfd_unem"))

p

ggsave(
  "./paper-figures/charts/irf-fevdfd-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)

## Chol

p <- plot_irf(data, c("chol_vfci_f1", "chol_vfci_f12", "fevdfd_unem"))

p

ggsave(
  "./paper-figures/charts/irf-chol-ext-vfci-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)

## Chol - other measures

p <- plot_irf(data, c("fevdfd_unem", "chol_fcig", "chol_epu"))

p

ggsave(
  "./paper-figures/charts/irf-chol-epu-fcig-fevdfd-unem.pdf",
  p, width = 5.5, height = 6, units = "in"
)

## Hetreg Macro

p <- plot_irf(data, c("fevdfd_unem", "hr_macro"))

p

ggsave(
  "./paper-figures/charts/irf-hr-macro-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)

## Hetreg Fin

p <- plot_irf(data, c("fevdfd_unem", "hr_fin"))

p

ggsave(
  "./paper-figures/charts/irf-hr-fin-fevdfd-unem.pdf",
  p, width = 5.5, height = 4, units = "in"
)
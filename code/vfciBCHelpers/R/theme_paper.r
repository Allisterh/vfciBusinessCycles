#' {ggplot2} theme to use for paper figures.
#'
#' @export
#'
#' @import ggplot2
#'
theme_paper <-
  theme_classic(base_size = 10) +
  theme(
    panel.background = element_rect(color = "black", linewidth = 1),
    axis.line = element_blank(),
    strip.background = element_blank(),
    legend.key = element_blank()
  )

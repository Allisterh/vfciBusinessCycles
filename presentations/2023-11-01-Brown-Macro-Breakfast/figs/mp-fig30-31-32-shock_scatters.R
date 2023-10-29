## Packages
require(data.table)
require(dplyr)
require(vars)
require(svars)
require(fevdid)
require(ggplot2)

## Plot settings
f1 <- "mp-fig30-scatter_u_vfci.pdf"
f2 <- "mp-fig31-scatter_mp_u_vfci.pdf"
f3 <- "mp-fig32-scatter_mp_inf_int.pdf"
prespath <- "./presentations/2023-11-01-Brown-Macro-Breakfast/figs/"
plot_height <- 3
plot_width <- 4.5

## Load plot themes
source(paste0(prespath, "theme_pres.R"))

## Construct shocks
mp <- fread("./data/mp")
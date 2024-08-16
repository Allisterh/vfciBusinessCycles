library(data.table)
library(purrr)
library(vars)
library(svars)
require(fevdid)
require(ggplot2)
library(vfciBCHelpers)
library(plotly)

#####

rec_dates <- get_recession_dt() |> _[year(start) >= 1960]

#####

bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
lags <- 2

add_cols <- c(
  "pc1", "pc2", "pc3", "pc4", "fci_g"
)

data <- get_var_data(vfci = "vfci_fgr1gdpc1", add_cols = add_cols)

x <- data[, -"vfci"] |> copy()

v <- fit_var(x[, -c(add_cols), with = FALSE], lags = lags)
v2 <- fit_var(data[, -c(add_cols), with = FALSE][, vfci := -1 * vfci], lags = lags)

mv_list <- list(
  max_share_u = id_fevdfd(v, "unemployment", bc_freqs),
  het_reg_pc = id_linear_het_reg(v, "consumption", x2 = add_cols,
    extra_data = data[, ..add_cols], method = "mriv", sign = "neg"),
  het_reg_pc_lags = id_linear_het_reg(v, "consumption", x2 = add_cols,
    extra_data = data[, ..add_cols], method = "mriv", sign = "neg", het_reg_lags = 0:2),
  het_reg = id_linear_het_reg(v, "consumption", sign = "neg", x2 = c(colnames(v$y), add_cols), extra_data = data[, ..add_cols], method = "mriv"),
  het_reg_vfci = id_linear_het_reg(v2, "consumption", sign = "neg"),
  chol = id_ordered_chol(reorder_var(v2, "vfci"))
)

#####

hs_df <- rbindlist(lapply(seq_along(mv_list), function(i) {
  hs <- hs(mv_list[[i]])$hs |> setDT()
  hs$model <- names(mv_list)[[i]]
  hs <- hs[impulse == "Main" | impulse == "Chol_1"]
  hs$date <- x[-c(1:lags), "date"][[1]]
  hs
}))


hs_df
data
plot_data <-
  tidyfast::dt_pivot_wider(hs_df[, .(date, hs, model)], names_from = model, values_from = hs) |>
  merge(data, by = "date", all = T) |>
  tidyfast::dt_pivot_longer(-date) |>
  _[, value := scale(value), by = name]

#####

rec_plotly <- lapply(seq_len(nrow(rec_dates)), function(i) {
  list(
    type = "rect",
    fillcolor = "gray",
    line = list(color = "gray"),
    opacity = 0.5,
    x0 = rec_dates[[i, "start"]],
    x1 = rec_dates[[i, "end"]],
    xref = "x",
    y0 = 0,
    y1 = 1,
    yref = "paper"
  )
})

#####


plot_data |>
  plot_ly(x = ~date, y = ~value, color = ~name, type = "scatter", mode = "lines") |>
  layout(
    xaxis = list(
      rangeslider = list(type = "date")
    ),
    shapes = rec_plotly
  )

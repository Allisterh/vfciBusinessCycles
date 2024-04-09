library(data.table)
library(purrr)
library(vars)
library(svars)
library(fevdid)

data <-
  fread(here::here("./data/vfciBC_data.csv")) |>
  _[date <= as.Date("2017-01-01")]

data <-
  data[, .(
    date,
    output,
    investment,
    consumption,
    hours_worked,
    unemployment,
    labor_share,
    interest,
    inflation,
    productivity,
    TFP,
    vfci = vfci_fgr10gdpc1
  )]

var_variables <- names(data)[-1]

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
lags <- 2

## Fit the VAR
v <- VAR(data[, -"date"], p = lags, type = "const")

## Target Each Variable, store as list
mv_l <-
  var_variables |>
  set_names() |>
  map(function(x) {
    id_fevdfd(
      v, target = x, freqs = bc_freqs,
      sign = ifelse(x == "unemployment", "pos", "neg")
    )
  })

## Compute the IRFs
irf_dt <-
  var_variables |>
  map(function(x) {
    res <- irf(mv_l[[x]], n.ahead = 40)$irf
    res$target_variable <- x
    res
  }) |>
  list_rbind() |>
  setDT()


## Compute the FEVD in Frequency Domain
fevdfd_dt <-
  var_variables |>
  map(function(x) {
    res <- fevdfd(mv_l[[x]])$fevdfd
    res$target_variable <- x
    res
  }) |>
  list_rbind() |>
  setDT()

## Compute the Shocks
shock_dt <-
  var_variables |>
  map(function(x) {
    res <- hs(mv_l[[x]])$hs
    res$target_variable <- x
    res$date <- rep(data[-c(1:lags), "date"][[1]], length(var_variables))
    res
  }) |>
  list_rbind() |>
  setDT()

## Compute the Contribution of the Shocks
contr_dt <-
  var_variables |>
  map(function(x) {
    res <- hd(mv_l[[x]])$hd
    res$target_variable <- x
    res$date <- rep(data[-c(1:lags), "date"][[1]], length(var_variables))
    res
  }) |>
  list_rbind() |>
  setDT()


## Correlation Matrix for BC Shocks
cross_dt <- merge(
  shock_dt[impulse == "Main", .(t, impulse, date, target_variable.x = target_variable, hs.x = hs)],
  shock_dt[impulse == "Main", .(t, impulse, date, target_variable.y = target_variable, hs.y = hs)],
  by = c("t", "impulse", "date"),
  allow.cartesian = TRUE
)

corr_dt <-
  cross_dt[, .(
    corr = stats::cor(hs.x, hs.y)
  ),
    by = .(impulse, target_variable.x, target_variable.y)
  ]

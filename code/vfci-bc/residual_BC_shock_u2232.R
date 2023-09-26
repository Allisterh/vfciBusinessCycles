require(ggplot2)
require(data.table)
require(dplyr)
require(vars)
require(svars)
require(bcadata)
require(fevdid)

## Pull the correct vintage of the BCA data
vfciBCdata <- fread("./data/vfciBC_data.csv") |>
    filter(date <= as.Date("2017-01-01"))

## Fit the VAR
v <- VAR(vfciBCdata[, -"date"], p = 2, type = "const")

mv <- id_fevdfd(v, "unemployment", c(2 * pi / 32, 2 * pi / 6), sign = "pos")
mv_v <- id_fevdfd(v, "unemployment", c(2 * pi / 32, 2 * pi / 22), sign = "neg")

## Weights, project vfci onto MBC, take resid
new_weights <- mv$Q[, 1] - mosaic::project(mv$Q[, 1], mv_v$Q[, 1])
new_weights <- new_weights / sqrt(sum(new_weights^2))

weight_df <- data.table(
    variable = names(mv$B[, 1]),
    weight = new_weights
)

## Are the original two VARs orthogonal?
mv$Q[, 1] %*% mv_v$Q[, 1]

##  Change the first shock in the new model to the residual new weights
mv_new <- mv
mv_new$Q[, 1] <- new_weights
mv_new$Q[, -1] <- pracma::nullspace(t(new_weights))
mv_new$B <- mv$B %*% solve(mv$Q) %*% mv_new$Q

B_df <- data.table(
    variable = names(mv$B[, 1]),
    weight = mv_new$B[, 1]
)

## Create the IRF
irf_df <- irf(mv_new, impulse = "Main", n.ahead = 40) |> setDT()

## Create the Historical Shocks
hist_df <-
    data.table(
        date = vfciBCdata[-(1:mv_new$p), date],
        shock = (resid(mv_new$VAR) %*% mv_new$B[, 1])[, 1]
    )

## FEVDFD
fevdfd <- fevdfd(mv_new)
fevdfd_df  <- rbindlist(lapply(names(fevdfd), function(x) {
    fevdfd_df <- fevdfd[[x]] |> setDT()
    fevdfd_df <- fevdfd_df[, .(f, fevdfd = Main)]
    fevdfd_df[, impulse := "Main"]
    fevdfd_df[, response := x]
}))

## FEVD
fevd <- fevd(mv_new, n.ahead = 40)
fevd_df  <- rbindlist(lapply(names(fevd), function(x) {
    fevd_df <- fevd[[x]] |> setDT()
    fevd_df <- fevd_df[, .(fevd = Main)]
    fevd_df[, h := .I]
    fevd_df[, impulse := "Main"]
    fevd_df[, response := x]
}))

## Save results
fwrite(irf_df, "./data/residual-mbc-shock/irf_u2232.csv")
fwrite(weight_df, "./data/residual-mbc-shock/weights_u2232.csv")
fwrite(B_df, "./data/residual-mbc-shock/B_u2232.csv")
fwrite(hist_df, "./data/residual-mbc-shock/hist_shocks_u2232.csv")
fwrite(fevdfd_df, "./data/residual-mbc-shock/fevdfd_u2232.csv")
fwrite(fevd_df, "./data/residual-mbc-shock/fevd_u2232.csv")

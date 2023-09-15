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
vfciBCdata

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
target_var <- "unemployment"

## Fit the VAR
v <- VAR(vfciBCdata[, -"date"], p = 2, type = "const")

## Grid of possible targets and frequencies to try
grid <- expand.grid(
    target = c("unemployment", "vfci"),
    sign = c("neg", "pos"),
    period_l = seq(4, 200, by = 2),
    period_h = seq(4, 200, by = 2)
    ) |> setDT()

grid[, diff := period_h - period_l]
grid <- grid[diff %between% c(6, 50)] ## only keep some periods
grid <- grid[period_l < 40 | diff > 20] ## Drop short ranges for long periods
grid <- grid[period_l < 40 | period_l %% 10 == 0]

grid[, freq_l := 2 * pi / period_h]
grid[, freq_h := 2 * pi / period_l]


## Actual identification of the shocks
irf_df <-
    rbindlist(lapply(seq_len(nrow(grid)), function(i) {
        mv <- id_fevdfd(
            v,
            target = grid[[i, "target"]],
            freqs = c(grid[[i, "freq_l"]], grid[[i, "freq_h"]]),
            sign = grid[[i, "sign"]],
            grid_size = 2000
            )

        irf_df <- vars::irf(mv, impulse = "Main", n.ahead = 40) |>
            setDT() |>
            cbind(grid[i, ])
    }))

u_irfsign <- irf_df[target == "unemployment" & h == 12 & response == "unemployment"]
u_irfsign[, u_irfsign := "pos"][irf <= 0, u_irfsign := "neg"]
u_irfsign <- u_irfsign[, .(target, sign, period_l, period_h, u_irfsign)]

irf_df <- merge(irf_df, u_irfsign, by = c("target", "sign", "period_l", "period_h"), all = T)

## Optimal Matches
results <- list()
vfci_irf <- irf_df[target == "vfci", .(
    h,
    response,
    vfci_sign = sign,
     vfci_period_l = period_l,
     vfci_period_h = period_h,
     vfci_irf = irf
     )]
u_irf <- irf_df[target == "unemployment", .(
    h,
    response,
    u_sign = sign,
    u_period_l = period_l,
    u_period_h = period_h,
    u_irf = irf)]

u_grid <- grid[target == "unemployment"]

for (i in seq_len(nrow(u_grid))) {

    comb_df <- merge(
        vfci_irf,
        u_irf[u_sign == u_grid[i, sign] & u_period_l == u_grid[i, period_l] & u_period_h == u_grid[i, period_h]],
        by = c("h", "response")
)
    comb_df[, se := (u_irf - vfci_irf)^2]

    summ_df <- comb_df[,
        .(rmse = sqrt(mean(se))), 
        by = .(u_sign, vfci_sign, u_period_h, u_period_l, vfci_period_l, vfci_period_h)]

    u_summ_df <- comb_df[response == "unemployment",
        .(u_rmse = sqrt(mean(se))),
        by = .(u_sign, vfci_sign, u_period_h, u_period_l, vfci_period_l, vfci_period_h)]

    summ_df <- merge(summ_df, u_summ_df)

    results <- c(results, list(summ_df))

}

results_df <- rbindlist(results)

## Save to Disk
fwrite(irf_df, "./data/classical_vfcibc_VAR_IRF.csv")
fwrite(results_df, "./data/classical_vfcibc_VAR_IRF_rmse.csv")

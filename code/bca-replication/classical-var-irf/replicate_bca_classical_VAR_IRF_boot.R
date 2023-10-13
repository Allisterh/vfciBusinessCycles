require(data.table)
require(dplyr)
require(vars)
require(svars)
require(bcadata)
require(fevdid)

## Pull the correct vintage of the BCA data
bcadata <- fread("./data/bca_replication_data.csv") |>
    filter(date <= as.Date("2017-01-01"))
x <- bcadata[, -"date"]

## Load the original BCA IRFs
bca_irf_df <- fread("./data/bca_original_var_results.csv")

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
target_var <- "unemployment"

## Fit the VAR
v <- VAR(x, p = 2, type = "const")

## Identify the shock
mv <- id_fevdfd(v, target = target_var, freqs = bc_freqs)

## Get the IRF
mv_irf <- vars::irf(mv, impulse = "Main", n.ahead = 40)$irf

## Bootstraps
boot_df_resample_ba <-
    bootstrap(
    mv, id_fevdfd, nboot = 1000, n_ahead = 40,
    method = "resample", design = "recursive", bias_adjust = TRUE,
    target = "unemployment", freqs = bc_freqs
    )$IRF_df |>
    filter(impulse == "Main") |>
    mutate(
        model = "classical_fd",
        version = "Replication"
    )

## Some data wrangling to get a clean dataframe
## with original and replicated IRFs
bca_irf_df <- bca_irf_df |>
    filter(model == "classical_fd") |>
    mutate(version = "Original BCA") |>
    rename(lower = "pctl_16") |>
    rename(upper = "pctl_84")

## Merge on VAR irf and bootstrap IRFs
irf_df <-
    merge(boot_df_resample_ba, mv_irf, by = c("h", "impulse", "response"))

comb_df <- rbindlist(list(
    bca_irf_df,
    irf_df
    ), use.names = TRUE, fill = TRUE)

fwrite(comb_df, "./data/replicated_bca_classical_VAR_IRF_boot.csv")

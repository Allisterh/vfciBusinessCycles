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
mv_irf <- irf(mv, impulse = "Main", n.ahead = 40)

## Bootstraps
boot_df_resample <-
    bootstrap(
    mv, id_fevdfd, nboot = 1000, n_ahead = 40,
    method = "resample", design = "recursive", bias_adjust = FALSE,
    target = "unemployment", freqs = bc_freqs
    )$IRF_df |>
    filter(shock == "Main") |>
    mutate(model = "Bootstrap Replication Resample")


boot_df_resample_ba <-
    bootstrap(
    mv, id_fevdfd, nboot = 1000, n_ahead = 40,
    method = "resample", design = "recursive", bias_adjust = TRUE,
    target = "unemployment", freqs = bc_freqs
    )$IRF_df  |>
    filter(shock == "Main") |>
    mutate(model = "Bootstrap Replication Resample Bias Adjust")

boot_df_wild_g <-
    bootstrap(
    mv, id_fevdfd, nboot = 1000, n_ahead = 40,
    method = "wild", design = "recursive", bias_adjust = FALSE,
    wild_distr = "gaussian",
    target = "unemployment", freqs = bc_freqs
    )$IRF_df  |>
    filter(shock == "Main") |>
    mutate(model = "Bootstrap Replication Wild Gaussian")

boot_df_wild_g_2000 <-
    bootstrap(
    mv, id_fevdfd, nboot = 2000, n_ahead = 40,
    method = "wild", design = "recursive", bias_adjust = FALSE,
    wild_distr = "gaussian",
    target = "unemployment", freqs = bc_freqs
    )$IRF_df  |>
    filter(shock == "Main") |>
    mutate(model = "Bootstrap Replication Wild Gaussian 2000")

boot_df_wild_r <-
    bootstrap(
    mv, id_fevdfd, nboot = 1000, n_ahead = 40,
    method = "wild", design = "recursive", bias_adjust = FALSE,
    wild_distr = "rademacher",
    target = "unemployment", freqs = bc_freqs
    )$IRF_df  |>
    filter(shock == "Main") |>
    mutate(model = "Bootstrap Replication Wild Rademacher")

boot_df_wild_g_sign <-
    bootstrap(
    mv, id_fevdfd, nboot = 1000, n_ahead = 40,
    method = "wild", design = "recursive", bias_adjust = FALSE,
    wild_distr = "gaussian",
    target = "unemployment", freqs = bc_freqs, sign_horizon = 20
    )$IRF_df  |>
    filter(shock == "Main") |>
    mutate(model = "Bootstrap Replication Wild Gaussian, Sign Horizon")

## Some data wrangling to get a clean dataframe
## with original and replicated IRFs
bca_irf_df <- bca_irf_df |>
    filter(model == "classical_fd") |>
    rename(h = "horizon") |>
    mutate(h = h) |>
    rename(value = "varirf") |>
    rename(lower = "pctl_16") |>
    rename(upper = "pctl_84") |>
    mutate(value = value) |>
    mutate(shock = "Main")

mv_irf_df <- mv_irf[[1]] |>
    as_tibble() |>
    rename(h = "V1") |>
    tidyr::pivot_longer(-h) |>
    mutate(variable = stringr::str_extract(name, "(?<=%->%).*$")) |>
    mutate(shock = stringr::str_extract(name, "(?<= ).*(?= )")) |>
    dplyr::select(!name) |>
    mutate(model = "Replication")

comb_df <- rbindlist(list(
    mv_irf_df, bca_irf_df, boot_df_resample, boot_df_resample_ba,
    boot_df_wild_g, boot_df_wild_g_2000, boot_df_wild_r, boot_df_wild_g_sign
    ), use.names = TRUE, fill = TRUE)

fwrite(comb_df, "./data/replicated_bca_classical_VAR_IRF_boot.csv")

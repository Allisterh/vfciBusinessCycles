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
vfciBCdata[, -"date"]

## Target the BC frequency and umemployment variable
bc_freqs <- c(2 * pi / 32, 2 * pi / 6)
target_var <- "unemployment"

## Fit the VAR
v0 <- VAR(vfciBCdata[, -c("date", "vfci")], p = 2, type = "const") 
v <- VAR(vfciBCdata[, -"date"], p = 2, type = "const")

## Identify the shock
mv0 <- id_fevdfd(v0, target = "unemployment", freqs = bc_freqs)
mv <- id_fevdfd(v, target = "unemployment", freqs = bc_freqs)
mv_vfci_bc <- id_fevdfd(v, target = "vfci", freqs = bc_freqs, sign = "neg")
mv_vfci_sr <- id_fevdfd(v, target = "vfci", freqs = c(0.175, 0.3), sign = "neg")


## Get the IRF
mv0_irf <- vars::irf(mv0, impulse = "Main", n.ahead = 40)
mv_irf <- vars::irf(mv, impulse = "Main", n.ahead = 40)
mv_vfci_bc_irf <- vars::irf(mv_vfci_bc, impulse = "Main", n.ahead = 40)
mv_vfci_sr_irf <- vars::irf(mv_vfci_sr, impulse = "Main", n.ahead = 40)

mv_fevdfd <-  mv |> fevdfd()
mv_fevdfd$vfci |>
    ggplot(aes(x = f, y = Main)) +
    geom_vline(xintercept = 0.175, color = "red") +
    geom_vline(xintercept = 0.3, color = "red") +
    geom_line() +
    scale_x_continuous(limits = c(0, pi))

## IRF Cleaning
bca_irf_df <- fread("./data/bca_original_var_results.csv")
bca_irf_df <- bca_irf_df |>
    filter(model == "classical_fd") |>
    mutate(version = "original") |>
    rename(h = "horizon") |>
    mutate(h = h) |>
    rename(lower = "pctl_16") |>
    rename(upper = "pctl_84") |>
    rename(value = "varirf") |>
    mutate(shock = "Main") |>
    mutate(target = "u_bc")

mv0_irf_df <- mv0_irf[[1]] |>
    as_tibble() |>
    rename(h = "V1") |>
    tidyr::pivot_longer(-h) |>
    mutate(variable = stringr::str_extract(name, "(?<=%->%).*$")) |>
    mutate(shock = stringr::str_extract(name, "(?<= ).*(?= )")) |>
    dplyr::select(!name) |>
    mutate(model = "classical_fd") |>
    mutate(version = "bca_variables") |>
    mutate(target = "u_bc")

mv_irf_df <- mv_irf[[1]] |>
    as_tibble() |>
    rename(h = "V1") |>
    tidyr::pivot_longer(-h) |>
    mutate(variable = stringr::str_extract(name, "(?<=%->%).*$")) |>
    mutate(shock = stringr::str_extract(name, "(?<= ).*(?= )")) |>
    dplyr::select(!name) |>
    mutate(model = "classical_fd") |>
    mutate(version = "vfci") |>
    mutate(target = "u_bc")

mv_vfci_bc_irf_df <- mv_vfci_bc_irf[[1]] |>
    as_tibble() |>
    rename(h = "V1") |>
    tidyr::pivot_longer(-h) |>
    mutate(variable = stringr::str_extract(name, "(?<=%->%).*$")) |>
    mutate(shock = stringr::str_extract(name, "(?<= ).*(?= )")) |>
    dplyr::select(!name) |>
    mutate(model = "classical_fd") |>
    mutate(version = "vfci") |>
    mutate(target = "vfci_bc")

mv_vfci_sr_irf_df <- mv_vfci_sr_irf[[1]] |>
    as_tibble() |>
    rename(h = "V1") |>
    tidyr::pivot_longer(-h) |>
    mutate(variable = stringr::str_extract(name, "(?<=%->%).*$")) |>
    mutate(shock = stringr::str_extract(name, "(?<= ).*(?= )")) |>
    dplyr::select(!name) |>
    mutate(model = "classical_fd") |>
    mutate(version = "vfci") |>
    mutate(target = "vfci_lr")

comb_df <- rbindlist(list(
    bca_irf_df,
    mv0_irf_df,
    mv_irf_df,
    mv_vfci_bc_irf_df,
    mv_vfci_sr_irf_df
    ), use.names = TRUE, fill = TRUE)

## Save the combined DF out to disk
fwrite(comb_df, "./data/vfciBC_classical_VAR_IRF.csv")

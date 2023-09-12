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
v0 <- VAR(vfciBCdata[, -c("date", "vfci")], p = 2, type = "const") 
v <- VAR(vfciBCdata[, -"date"], p = 2, type = "const")

## Identify the shock
mv0 <- id_fevdfd(v0, target = "unemployment", freqs = bc_freqs)

grid <- rbindlist(list(
    list("unemployment", bc_freqs[1], bc_freqs[2], "pos"),
    list("unemployment", 2 * pi / 100, 2 * pi / 40, "pos"),
    list("unemployment", 2 * pi / 40, 2 * pi / 20, "pos"),
    list("unemployment", 2 * pi / 20, 2 * pi / 10, "pos"),
    list("unemployment", 2 * pi / 10, 2 * pi / 5, "pos"),
    list("unemployment", 2 * pi / 5, 2 * pi / 2, "pos"),
    list("vfci", bc_freqs[1], bc_freqs[2], "pos"),
    list("vfci", 2 * pi / 100, 2 * pi / 40, "pos"),
    list("vfci", 2 * pi / 40, 2 * pi / 20, "pos"),
    list("vfci", 2 * pi / 20, 2 * pi / 10, "pos"),
    list("vfci", 2 * pi / 10, 2 * pi / 5, "pos"),
    list("vfci", 2 * pi / 5, 2 * pi / 2, "pos")
    ))
names(grid) <- c("target", "freq_l", "freq_h", "sign")
grid

irf_df <-
rbindlist(lapply(seq_len(nrow(grid)), function(i) {
    mv <- id_fevdfd(
        v,
        target = grid[[i, "target"]],
        freqs = c(grid[[i, "freq_l"]], grid[[i, "freq_h"]]),
        sign = grid[[i, "sign"]]
        )

    irf_df <- vars::irf(mv, impulse = "Main", n.ahead = 40) |> 
        setDT() |>
        mutate(
            target = grid[[i, "target"]],
            freq_l = grid[[i, "freq_l"]],
            freq_h = grid[[i, "freq_h"]],
            sign = grid[[i, "sign"]]
        )
}))

irf_df |>
    filter(target == "vfci") |>
    mutate(label = paste(target, ":", round(freq_l, 2), "-", round(freq_h, 2))) |>
    ggplot(aes(
        x = h,
        y = irf,
        color = label
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(vars(response), scales = "free_y")

irf_df |>
    filter(target == "unemployment") |>
    mutate(label = paste(target, ":", round(freq_l, 2), "-", round(freq_h, 2))) |>
    ggplot(aes(
        x = h,
        y = irf,
        color = label
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(vars(response), scales = "free_y")

## Two Similar ones
irf_df |>
    filter(freq_l == 2 * pi / 40) |>
    mutate(label = paste(target, ":", round(freq_l, 2), "-", round(freq_h, 2))) |>
    ggplot(aes(
        x = h,
        y = irf,
        color = label
    )) +
    geom_hline(yintercept = 0) +
    geom_line() +
    facet_wrap(vars(response), scales = "free_y")





#####
#####
#####


mv <- id_fevdfd(v, target = "unemployment", freqs = bc_freqs)
mv_vfci_bc <- id_fevdfd(v, target = "vfci", freqs = bc_freqs, sign = "neg")
mv_vfci_sr <- id_fevdfd(v, target = "vfci", freqs = c(2 * pi / 40, 2 * pi / 20), sign = "neg")


## Get the IRF
mv0_irf <- vars::irf(mv0, impulse = "Main", n.ahead = 40) |> setDT()
mv_irf <- vars::irf(mv, impulse = "Main", n.ahead = 40) |> setDT()
mv_vfci_bc_irf <- vars::irf(mv_vfci_bc, impulse = "Main", n.ahead = 40) |> setDT()
mv_vfci_sr_irf <- vars::irf(mv_vfci_sr, impulse = "Main", n.ahead = 40) |> setDT()

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
    rename(response = "variable") |>
    rename(irf = "varirf") |>
    mutate(shock = "Main") |>
    mutate(target = "u_bc")

mv0_irf_df <- mv0_irf |>
    mutate(model = "classical_fd") |>
    mutate(version = "bca_variables") |>
    mutate(target = "u_bc")

mv_irf_df <- mv_irf |>
    mutate(model = "classical_fd") |>
    mutate(version = "vfci") |>
    mutate(target = "u_bc")

mv_vfci_bc_irf_df <- mv_vfci_bc_irf |>
    mutate(model = "classical_fd") |>
    mutate(version = "vfci") |>
    mutate(target = "vfci_bc")

mv_vfci_sr_irf_df <- mv_vfci_sr_irf |>
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

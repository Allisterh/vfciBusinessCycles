## code to prepare `var_results_xxx` datasets
require(R.matlab)
require(dplyr)
require(readr)
require(hdf5r)
require(reshape2)

require(ggplot2)

## Map of variable codes to names
var_names <- tibble(
    varnames = c(
        "output", "investment", "consumption",
        "hours_worked", "unemployment", "labor_share",
        "interest", "inflation", "productivity", "TFP"
        ),
    variable = 1:10)



## Read in and organize the data behind Figure 1
## Bayesian VAR results, frequency domain, unemployment target
file <- h5file(
    "./data-raw/var_results/benchmark_var_1955_2017_fd_sr.mat", mode = "r")

irf <- file$open("IRFsr")$read()

irf_df <- melt(irf) |> as_tibble()
names(irf_df) <- c("target_variable", "iter", "temp", "value")

irf_df <- irf_df |>
    group_by(target_variable, iter) |>
    mutate(
        variable = ceiling(temp / 40),
        horizon = temp %% 40
    ) |>
    mutate(
        horizon = ifelse(horizon == 0, 40, horizon)
    ) |>
    select(-temp) |>
    ungroup()

irf_summ <- irf_df |>
    group_by(target_variable, variable, horizon) |>
    summarize(
        median = median(value),
        pctl_84 = quantile(value, 0.84),
        pctl_16 = quantile(value, 0.16)
        ) |>
    ungroup()

## Only keep the results that target unemployment
irf_summ <- irf_summ |> filter(target_variable == 5) |> select(-target_variable)

df <- left_join(irf_summ, var_names, by = "variable") |>
    select(-variable) |>
    rename(variable = "varnames")

df_bvar_fd <- df |> mutate(model = "bayesian_fd")


## Repeat process for other var_results: Time Domain targetting 0 - 4 qtrs
file <- h5file(
    "./data-raw/var_results/benchmark_var_1955_2017_td4.mat", mode = "r")

## 'm' is for median
mirf <- file$open("mirf")$read()

## The second case is the one targetting unemployment
mirf <- mirf[2, , ]

mirf_df <- melt(mirf) |> as_tibble()
names(mirf_df) <- c("horizon", "variable", "median")

## 'l' is for low, the lower 68% conf band
lirf <- file$open("lirf")$read()
lirf <- lirf[2, , ]
lirf_df <- melt(lirf) |> as_tibble()
names(lirf_df) <- c("horizon", "variable", "pctl_16")

## 's' is for...supremum?, the upper 68% conf band
sirf <- file$open("sirf")$read()
sirf <- sirf[2, , ]
sirf_df <- melt(sirf) |> as_tibble()
names(sirf_df) <- c("horizon", "variable", "pctl_84")

df <- mirf_df |>
    left_join(lirf_df, by = c("horizon", "variable")) |>
    left_join(sirf_df, by = c("horizon", "variable"))

df <- left_join(df, var_names, by = "variable") |>
    select(-variable) |>
    rename(variable = "varnames")

df_bvar_td4 <- df |> mutate(model = "bayesian_td4")


## Repeat process for other var_results: Time Domain targetting 6 - 32 qtrs
file <- h5file(
    "./data-raw/var_results/benchmark_var_1955_2017_td632.mat", mode = "r")

## 'm' is for median
mirf <- file$open("mirf")$read()

## The second case is the one targetting unemployment
mirf <- mirf[2, , ]

mirf_df <- melt(mirf) |> as_tibble()
names(mirf_df) <- c("horizon", "variable", "median")

## 'l' is for low, the lower 68% conf band
lirf <- file$open("lirf")$read()
lirf <- lirf[2, , ]
lirf_df <- melt(lirf) |> as_tibble()
names(lirf_df) <- c("horizon", "variable", "pctl_16")

## 's' is for...supremum?, the upper 68% conf band
sirf <- file$open("sirf")$read()
sirf <- sirf[2, , ]
sirf_df <- melt(sirf) |> as_tibble()
names(sirf_df) <- c("horizon", "variable", "pctl_84")

df <- mirf_df |>
    left_join(lirf_df, by = c("horizon", "variable")) |>
    left_join(sirf_df, by = c("horizon", "variable"))

df <- left_join(df, var_names, by = "variable") |>
    select(-variable) |>
    rename(variable = "varnames")

df_bvar_td632 <- df |> mutate(model = "bayesian_td632")


## Repeat process for other var_results: Classical VAR, frequency domain
file <- h5file(
    "./data-raw/var_results/classical_var_1955_2017_fd_sr.mat", mode = "r")

## 'm' is for median
mirf <- file$open("mirf")$read()

mirf_df <- melt(mirf) |> as_tibble()
names(mirf_df) <- c("horizon", "variable", "median")

## 'l' is for low, the lower 68% conf band
lirf <- file$open("lirf")$read()
lirf_df <- melt(lirf) |> as_tibble()
names(lirf_df) <- c("horizon", "variable", "pctl_16")

## 's' is for...supremum?, the upper 68% conf band
sirf <- file$open("sirf")$read()
sirf_df <- melt(sirf) |> as_tibble()
names(sirf_df) <- c("horizon", "variable", "pctl_84")

df <- mirf_df |>
    left_join(lirf_df, by = c("horizon", "variable")) |>
    left_join(sirf_df, by = c("horizon", "variable"))

df <- left_join(df, var_names, by = "variable") |>
    select(-variable) |>
    rename(variable = "varnames")

df_var_fd <- df |> mutate(model = "classical_fd")


## Combine all the models
original_var_results <-
    Reduce(rbind, list(
        df_bvar_fd,
        df_bvar_td4,
        df_bvar_td632,
        df_var_fd
    ))


## Add to the package
write_csv(original_var_results, "data/bca_original_var_results.csv")

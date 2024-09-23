##
##  No calculations or graphing performed,
##  just copying figures to the Overleaf git repository.
##
files <- list.files("./paper-figures/", full.names = TRUE, recursive = TRUE)
files <- files[!grepl("paper-figurepack", files)]

new_location <- stringr::str_replace_all(files, "paper-figures", "paper-Overleaf/figures/")

for (i in seq_along(files)) file.copy(files[[i]], new_location[[i]], overwrite = TRUE)

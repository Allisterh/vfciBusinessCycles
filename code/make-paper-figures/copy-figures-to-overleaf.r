##
##  No calculations or graphing performed,
##  just copying figures to the Overleaf git repository.
##

## Remove the old figures
files <- list.files("./paper-Overleaf/figures/", full.names = TRUE, recursive = TRUE)
for (i in seq_along(files)) file.remove(files[[i]])

## Copy over the new figures
files <- list.files("./paper-figures/", full.names = TRUE, recursive = TRUE)
new_location <- stringr::str_replace_all(files, "paper-figures", "paper-Overleaf/figures/")
for (i in seq_along(files)) file.copy(files[[i]], new_location[[i]], overwrite = TRUE)

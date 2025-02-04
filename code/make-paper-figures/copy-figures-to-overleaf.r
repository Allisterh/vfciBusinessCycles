##
##  No calculations or graphing performed,
##  just copying figures to the Overleaf git repository.
##  Checks that Overleaf folder exists and accessible.
##

path <- "./paper-Overleaf/"

if (file.exists(path)) {
  if (file.access(path, mode = 0) == 0) {

    ## Remove the old figures
    files <- list.files("./paper-Overleaf/figures/", full.names = TRUE, recursive = TRUE)
    for (i in seq_along(files)) file.remove(files[[i]])

    ## Copy over the new figures
    files <- list.files("./paper-figures/", full.names = TRUE, recursive = TRUE)
    new_location <- stringr::str_replace_all(files, "paper-figures", "paper-Overleaf/figures/")
    for (i in seq_along(files)) file.copy(files[[i]], new_location[[i]], overwrite = TRUE)

  } else {
    message("Folder exists but is not accessible.")
  }
} else {
  message("Folder does not exist.")
}

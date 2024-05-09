##
## Run all files in "make-paper-figures" subfolders
##
folders <- c(
  "./code/make-paper-figures/charts/",
  "./code/make-paper-figures/tables/",
  "./code/make-paper-figures/inline-values/"
)
files <- list.files(folders, full.names = TRUE, recursive = TRUE)
for (i in files) source(i)

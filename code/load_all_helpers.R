helper_files <- list.files(
    path = "./code/helpers/",
    pattern = "*.R",
    full.names = TRUE
    )

for (f in helper_files) source(f)

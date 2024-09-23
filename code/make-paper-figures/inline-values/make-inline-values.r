## Function to construct Latex command with a name and a value
def_latex_value <- function(name, value, math_mode = TRUE) {
  if (math_mode) {
    paste0("\\newcommand{\\", name, "}{", value, "}")
  } else {
    paste0("\\newcommand{\\", name, "}{\\textnormal{", value, "}}")
  }
}

## Read in datasets needed for values
library(data.table)
library(vfciBCHelpers)

data <- get_var_data()

v <- list(
  dataStartDate = zoo::as.yearqtr(min(data$date)),
  dataEndDate = zoo::as.yearqtr(max(data$date))
)


## Define the Latex commands for each value
values <- c(
  def_latex_value("dataStartDate", v$dataStartDate, math_mode = FALSE),
  def_latex_value("dataEndDate", v$dataEndDate, math_mode = FALSE)
)

## Write to disk
file_conn <- file("./paper-figures/inline-values/inline-values.tex")
writeLines(values, file_conn)
close(file_conn)

saveRDS(v, "./data/paper-figures/inline-values/inline-values.rds")

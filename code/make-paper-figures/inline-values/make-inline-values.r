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
corrs <- fread("./data/paper-figures/tables/hs-cross-correlations.csv")
avg_bc_fevdfds <- fread("./data/paper-figures/tables/avg-fevdfd-bc-freqs-max-share.csv")


## Define the Latex commands for each value
values <- c(
  def_latex_value(
    "vfciUnemploymentMaxShareCorrelation",
    round(corrs[var_x == "vfci" & var_y == "unemployment", corr], 2)
  ),
  def_latex_value(
    "vfciOutputMaxShareCorrelation",
    round(corrs[var_x == "vfci" & var_y == "output", corr], 2)
  ),
  def_latex_value(
    "vfciLaborShareMaxShareCorrelation",
    round(corrs[var_x == "vfci" & var_y == "labor_share", corr], 2)
  ),
  def_latex_value("dataStartDate", zoo::as.yearqtr(min(data$date)), math_mode = FALSE),
  def_latex_value("dataEndDate", zoo::as.yearqtr(max(data$date)), math_mode = FALSE),
  def_latex_value(
    "vfciFEVDFD",
    100 * round(avg_bc_fevdfds[target == "vfci" & response == "vfci", avg_fevdfd], 2)
  )
)

## Write to disk
file_conn <- file("./paper-figures/inline-values/inline-values.tex")
writeLines(values, file_conn)
close(file_conn)

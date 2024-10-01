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

compare_int_ext_vfci <- readRDS("./data/paper-figures/inline-values/compare-int-ext-vfci-corrs.rds")

v <- v |>
  c(compare_int_ext_vfci)

## Define the Latex commands for each value
values <- c(
  def_latex_value("dataStartDate", v$dataStartDate, math_mode = FALSE),
  def_latex_value("dataEndDate", v$dataEndDate, math_mode = FALSE),
  def_latex_value("intExtFinVFCICorr", round(v$int_ext_fin_vfci_corr, 2)),
  def_latex_value("intExtMacroVFCICorr", round(v$int_ext_macro_vfci_corr, 2)),
  def_latex_value("intMacroFinVFCICorr", round(v$int_macro_fin_vfci_corr, 2))
)

## Write to disk
file_conn <- file("./paper-figures/inline-values/inline-values.tex")
writeLines(values, file_conn)
close(file_conn)

saveRDS(v, "./data/paper-figures/inline-values/inline-values.rds")

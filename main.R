##
##  Script to call all other scripts in the project
##

## Reinstantiate Renv Environment
renv::restore()
renv::install("./code/vfciBCHelpers/") ## Helper functions in a local package

## Data Construction
source("./code/clean-raw-data/construct_vfci_data.r")
source("./code/clean-raw-data/merge_all_analysis_data.r")

## VFCI Business Cycle
source("./code/run-vfci-bc-analysis/compare-vfci.r")
source("./code/run-vfci-bc-analysis/fit-max-share-vars.r")
source("./code/run-vfci-bc-analysis/fit-cholesky-vars.R")
source("./code/run-vfci-bc-analysis/calc-mean-vol.r")

## Make Figures For Paper
source("./code/make-paper-figures/run-figure-scripts.r")

## Copy Figures to Overleaf git submodule
## NOTE: This only runs on Matt's machine
if (grepl("matthewdehaven", getwd())) source("./code/make-paper-figures/copy-figures-to-overleaf.r")

## Run Tests
testthat::test_dir("tests")

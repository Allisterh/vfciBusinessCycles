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
source("./code/run-vfci-bc-analysis/fit-max-share-vars.r")
source("./code/run-vfci-bc-analysis/fit-cholesky-vars.r")
source("./code/run-vfci-bc-analysis/calc-mean-vol.r")

## Make Figures For Paper

## Run Tests
testthat::test_dir("tests")

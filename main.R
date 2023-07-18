##
##  Script to call all other scripts in the project
##

## Reinstantiate Renv Environment
## (this should happen automatically when opening the project)
renv::restore()

## Data Cleaning
source("./code/clean-data-raw/bca_original_var_results.R")
source("./code/clean-data-raw/bca_replication_data.R")

## BCA Replication
source("./code/bca-replication/classical-var-irf/replicate_bca_classical_VAR_IRF.R")

## Build BCA Replication Reports
## Windows' TEX doesn't like spaces in document names 
rmarkdown::render("./code/reports/classical_VAR_IRF_replication.RMD", "pdf_document", "ClassicalVARIRFReplication.pdf", "./reports/")

## Run tests
testthat::test_dir("tests")
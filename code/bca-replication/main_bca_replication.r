##
##  Script to replicate Business Cycle Anatomy Paper
##  Code in ./code/bca-replication/ separated from
##  rest of project.
##

## BCA Replication
source("./code/bca-replication/classical-var-irf/replicate_bca_classical_VAR_IRF.R")
source("./code/bca-replication/classical-var-irf/replicate_bca_classical_VAR_IRF_boot.R")
# source("./code/bca-replication/bayesian-var-irf/replicate_bca_bayesian_VAR_IRF_BVAR.R")
source("./code/bca-replication/bayesian-var-irf/replicate_bca_bayesian_VAR_IRF_bvartools.R")

## Rereun BCA with Current Data
source("./code/bca-replication/current-data-var-irf/current_data_classical_VAR_IRF.R")
source("./code/bca-replication/current-data-var-irf/current_data_bayesian_VAR_IRF.R")

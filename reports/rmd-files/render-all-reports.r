
## Don't put spaces in document names

## Build BCA Replication Reports
rmarkdown::render("./code/reports/classical_VAR_IRF_replication.RMD", "pdf_document", "ClassicalVARIRFReplication.pdf", "./reports/")
rmarkdown::render("./code/reports/classical_VAR_IRF_boot_replication.RMD", "pdf_document", "ClassicalVARIRFbootReplication.pdf", "./reports/")
rmarkdown::render("./code/reports/bayesian_VAR_IRF_replication.RMD", "pdf_document", "BayesianVARIRFReplication.pdf", "./reports/")
rmarkdown::render("./code/reports/compare_FD_TD_targetting.RMD", "pdf_document", "CompareFDTDTargetting.pdf", "./reports/")
rmarkdown::render("./code/reports/current_VAR_IRF.RMD", "pdf_document", "CurrentDataVARIRFs.pdf", "./reports/")

## VFCI BC reports
rmarkdown::render("./code/reports/vfciBC_data_summary.RMD", "pdf_document", "VFCIBusinessCycleDataSummary.pdf", "./reports/")
rmarkdown::render("./code/reports/vfciBC_classical_VAR_IRF.RMD", "pdf_document", "VFCIBusinessCycleVARIRFs.pdf", "./reports/")
rmarkdown::render("./code/reports/historical_shocks_decomposition.RMD", "pdf_document", "HistoricalShocksAndDecompositions.pdf", "./reports/")
rmarkdown::render("./code/reports/vfciBC_fgrXX.RMD", "pdf_document", "VFCIVariations.pdf", "./reports")
rmarkdown::render("./code/reports/compare_FD_TD_spacing.RMD", "pdf_document", "CompareFDTDSpacing.pdf", "./reports")
rmarkdown::render("./code/reports/vfciBC_allVARcharts.RMD", "pdf_document", "AllVARcharts.pdf", "./reports")

rmarkdown::render("./code/reports/business_cycle_frequencies.RMD", "pdf_document", "BusinessCycleFrequencies.pdf", "./reports")
rmarkdown::render("./code/reports/vfciBC_choleskey_id.RMD", "pdf_document", "CholeskeyIDVARS.pdf", "./reports")
rmarkdown::render("./code/reports/vfciBC_heteroskedastic_id.RMD", "pdf_document", "HeteroskedasticIDVARS.pdf", "./reports")
rmarkdown::render("./code/reports/vfciBC_tdistribution_id.RMD", "pdf_document", "tDistributionIDVARS.pdf", "./reports")
rmarkdown::render("./code/reports/mean-vol-relationship.RMD", "pdf_document", "MeanVolRelationship.pdf", "./reports")
rmarkdown::render("./code/reports/estimate-vfci-from-VAR.RMD", "pdf_document", "EstimateVFCIfromVAR.pdf", "./reports")
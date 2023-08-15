[![Run Main.R](https://github.com/VFCI/vfciBusinessCycles/actions/workflows/main.yml/badge.svg)](https://github.com/VFCI/vfciBusinessCycles/actions/workflows/main.yml)
[![Run Tests](https://github.com/VFCI/vfciBusinessCycles/actions/workflows/test.yml/badge.svg)](https://github.com/VFCI/vfciBusinessCycles/actions/workflows/test.yml)
# vfciBusinessCycles Summary
Research project exploring the relationship between financial conditions and business cycles.

Makes use of two helper packages: [bcadata](https://github.com/VFCI/bcadata) and [fevdid](https://github.com/VFCI/fevdid).

# Running this Project
Running `main.R` will recreate all output of the project, relying only upon `data-raw` and packages.

This project uses the R package [renv](https://rstudio.github.io/renv/articles/renv.html) for package version control.
The first line in `main.R` will reinstantiate the project with the correct packages, but this can also be done manually with `renv::restore()`.

This project uses Github Actions to run `main.R` and unit tests in `tests/`.
The `Run Tests` Github Action is run for every push to repository, for any branch.
The `Run Main.R` Github Action is only run on pull requests to the main branch.

The tests are written using the R package [testthat](https://testthat.r-lib.org).
Tests can be manually run with `testthat::test_dir("tests")`.

In order to handle large data files, this project makes use of [Github Large File Storage](https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-git-large-file-storage).
Currently this is only used for some of the data in `data-raw/`.

# Folder Structure

- `main.R` runs all of the code in the project from start to finish.
- `data-raw/` contains all of the raw data files copied from elsewhere.

It also contains data scripts that should not run everytime `main.R` is called, i.e. those that pull vintages of data from online. These then output the file to `data-raw/`.
- `data/` contains cleaned versions of all data from data-raw .
- `code/` contains all of the code in the project.
    - `reports/` contains the Rmarkdown *scripts*.
    - `clean-data-raw` contains the code for converting raw data files to tidy csvs outputted to `data/`.
    - `bca-replication` contains the code for replicating the BCA paper.
    - `.../` more subparts of the project.
- `reports/` contains the *output* from Rmarkdown scripts.

Default should be '.pdf' files as those display well on GitHub and are self contained.  Difficulties with relative paths for markdown, html, etc.

- `tests/` contains the unit tests for the project.


# Reports

Reports are compiled from Rmarkdown to PDFs and stored in `reports/`.
For convenience, here are links to each report.

### BCA Replication Reports

[Classical VAR IRF Replication](reports/ClassicalVARIRFReplication.pdf)

[Classical VAR IRF Replication with Bootstrap](reports/ClassicalVARIRFbootReplication.pdf)

[Bayesian VAR IRF Replication](reports/BayesianVARIRFReplication.pdf)

[Correction: Comparing FD and TD Targetting](reports/compareFDTDTargetting.pdf)


# Project Description

### Business Cycle Anatomy Replication
The first step in this project replicates key results from the paper "[Business Cycle Anatomy](https://www.aeaweb.org/articles?id=10.1257/aer.20181174)" (2020) by Angeletos, Collard, and Dellas published in the AER.


### Next step in the project

### Next next step
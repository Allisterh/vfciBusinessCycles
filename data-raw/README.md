# data-raw README

Provides information on how to acquire each of the raw data files in this folder.

---


- `bca_original_var_results/*.mat`

    - Copied from the [replication files of "Business Cycle Anatomy"](https://www.openicpsr.org/openicpsr/project/118082/version/V1/view). From the `empirics_main_text/results_var/` directory.

    - Cleaned by the `bca_original_var_results.R` script to create `bca_original_var_results.csv`.

- `bca_replication_data.csv`

    - Created by the `bca_replication_data.R` script which uses the [bcadata](https://github.com/VFCI/bcadata) data package.

- `figs-raw/`

    - `bca-replication/*.pdf`

        - Figures taken from the [replication files of "Business Cycle Anatomy"](https://www.openicpsr.org/openicpsr/project/118082/version/V1/view). From `empirics_main_text/figures/` and `online_appendix/figures/` directories.

- `bis_dp_search_export_20240731-134219.csv`

    - Downloaded from the [BIS Data Portal](https://data.bis.org) on July 31, 2024. Series codes are 'Q.US.P.B.M.770.A' and 'Q.US.P.A.M.770.A'. Quarterly frequency, Borrowers' country: United States, Borrowing sector; Private non-financial sector, Lending Sector: {All Sectors; Banks, domestic}, Valuation: Market Value,  Unit type: Percentage of GDP, Adjustment: Adjusted for breaks. [Direct data URL](https://data.bis.org/topics/TOTAL_CREDIT/BIS,WS_TC,2.0/Q.US.P.B.M.770.A?additional_ts=BIS%2CWS_TC%2C2.0%255EQ.US.P.A.M.770.A). 

- `ebp_csv.csv`

    - Excess Bond Premium, Gilchrist and Zakrajšek (GZ) Spread, and implied resession probability.  Downloaded from the Board of Governors of the Federal Reserve System, Feds Note, [Recession Risk and the Excess Bond Premium](https://www.federalreserve.gov/econresdata/notes/feds-notes/2016/recession-risk-and-the-excess-bond-premium-20160408.html) on July 31, 2024. CSV link under Figure 2.

        - Convert from monthly to quarterly by taking the last observation of the quarter.

- `MacroFinanceUncertainty_202402Update/`

    - Macro, Financial and Real Uncertainty indexes at different horizons.  Downloaded from [Sydney Ludbigson's website](https://www.sydneyludvigson.com/macro-and-financial-uncertainty-indexes) on July 31, 2024.  This vintage is the 202402 update (the latest at the time). Measures are introduced in "[Measuring Uncertainty](https://static1.squarespace.com/static/54397369e4b0446f66937a73/t/63629f53023c031c27dd70d4/1667407699615/jlnAER.pdf)".

        - Convert from monthly to quarterly by taking the last observation of the quarter.
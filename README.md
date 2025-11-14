# IESPFI_ML_DATA_CODE

This repository contains the data processing and analysis code used in the manuscript examining the dimensional structure of family involvement and parent engagement profiles using National Household Education Survey (NHES) data.

All raw data files and codebooks are obtained directly from the U.S. Department of Education, Institute of Education Sciences (IES), National Center for Education Statistics (NCES) and are redistributed here only as a convenience for reproducibility. Please consult NCES documentation for full technical details and usage restrictions.

## Repository Structure

`data/`

-   `NHES_raw/` – Original NHES data files downloaded from the IES/NCES website.

-   `codebook/` – Corresponding codebook and survey documentation (PDF) from NCES.

`R/`

-   `01_data_cleaning.R` – Imports raw NHES files, selects FPP variables and demographic/student characteristics, recodes responses, and saves analysis-ready data.

-   `02_ega_dimension_reduction.R` – Runs Exploratory Graph Analysis (EGA), GLASSO estimation, walk-trap community detection, and bootstrap stability checks; outputs figures and tables for FPP dimensions.

-   `03_kmeans_clustering.R` – Performs k-means clustering on EGA component scores, selects optimal k using NbClust, and generates cluster plots and descriptive statistics by profile.

-   `04_multinomial_regression.R` – Fits multinomial logistic regression models predicting FPP profiles from demographic and child variables; computes accuracy, confusion matrix, and mosaic plot.

-   `05_xgboost_model.R` – Trains and tunes multiclass XGBoost models, conducts cross-validation, computes performance metrics, and produces variable importance plots.

-   `figures/` – All figures used in the manuscript (EGA network, stability plots, cluster plot, confusion matrix, variable importance, etc.).

## Data Source and Citation

The NHES data and documentation are produced and maintained by NCES. Users should follow NCES guidelines for citation and responsible use. For full documentation and original downloads, visit the NCES website.

## Contact

For questions about the code or replication of the analyses, please initiate pull requests in this repository. Any concerns are welcome for bug fixes, clarification, or extensions of the analyses.

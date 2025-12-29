# Childhood-Inequities-of-Sedentarism
# Childhood Inequities of Sedentarism

## Overview

This repository contains the analytical code and data structure for a study on **Inequality trends in sedentarism among children aged 6 to 15 years**, using the **Relative Index of Inequality (RII)**.  
The analysis is based on microdata obtained from the **Instituto Nacional de Estadística (INE)** and focuses on quantifying inequalities across proxy socioeconomic gradients, with stratified analyses by sex.

The project is designed to support transparent, reproducible analysis and the structured production of tables and figures for dissemination.

---

## Study Population

- Children aged **6 to 15 years**
- Analyses also include datasets **0 to 15 years** for preprocessing
- Sex-stratified analyses: **females, males*

---

## Methodological Approach

- Inequality measured using the **Relative Index of Inequality (RII)**
- Analyses conducted using cleaned and complete-case datasets
- Outputs exported for integration into tables, figures, and maps

---

## Repository Structure

### Main directory (analysis datasets)

The main folder contains the core analytical datasets:

- `joined`  
  Raw merged dataset including children aged 0 to 15 years.

- `joined_clean`  
  Cleaned version of the merged dataset (no missing values), ages 0 to 15.

- `joined_6`  
  Raw merged dataset restricted to children aged 6 to 15 years.

- `joined_clean_6`  
  Cleaned complete-case dataset used for the main analyses (ages 6 to 15).

---

### `Raw Data/`

Contains all files **downloaded directly from the INE database**, without modification.

- Original survey microdata
- Data are stored as received to preserve provenance
- These files serve as the input for all data cleaning steps
- Contains cleaned datasets created for **each survey year**

---

### `Datasets/`

Contains **exported analytical datasets** used to generate specific tables and figures.

Examples include:
- RII estimates for females
- RII estimates for males
- Overall RII datasets

These datasets allow tables and figures to be reproduced without re-running the full pipeline.

---

### `Tables/`

Contains tables exported from R and later integrated into Excel files for reporting.

- Regression outputs
- Inequality estimates
- Descriptive statistics

---

### `Figures/`

Contains all exported figures generated during the analysis.

- Inequality plots
- Stratified visualizations
- Maps

---

### `Resources/`

Supporting files used across analyses, including:

- List of Spanish **Autonomous Communities**
- Files required for **mapping Autonomous Communities**
- **NUTS 1 region** classification

---

## Reproducibility Notes

- Temporary R session files (e.g. `.RData`, `.Rhistory`) are intentionally excluded
- Analyses are fully script-based
- Data cleaning and analytical steps are documented in the code

---

## Data Access and Ethics

- INE microdata are subject to data access agreements.  
- Users wishing to reproduce the analysis must obtain the data directly from the **Instituto Nacional de Estadística (INE)**.

---

## Contact

For questions related to the analysis or repository structure, please contact:

**Diana Juanita Mora**  
PhD Candidate  
Universidad de Alcalá  

---

## Acknowledgements

This work is part of research on childhood inequalities and sedentary behavior, supported by the Project **EVERYMOVE**.
